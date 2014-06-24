#----------------------------------------------------------------------
# global.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
# Indicate if run is on the server
run.on.server <- ifelse(Sys.info()["sysname"] == "Windows", FALSE, TRUE)

library(foreach)
if (run.on.server) {
  # change JR, 20140402
  library(doParallel)
  registerDoParallel(cores = detectCores())
  # library(doMC)
  # registerDoMC()
}

library(shiny)
library(shinyIncubator)
library(shinysky)
library(MCMCpack)
library(rjags)
library(R2jags)
library(lattice)
library(abind)
library(msm)
library(proto)
library(plyr)
library(ggplot2)
library(reshape2)

Rfiles <- list.files(file.path(getwd(), "R"))
Rfiles <- Rfiles[grepl(".R", Rfiles)]
sapply(paste0("R/", Rfiles), source)
#----------------------------------------------------------------------
percentiles.for.change <- c(0.025, 0.5, 0.975)
country.info.file <- "data/Country-and-area-classification-inclFP2020.csv"
country.info.inclFP2020 <- read.csv(country.info.file, header = T, stringsAsFactors = F, strip.white = T)
countrycodes.FP2020 <- paste0(country.info.inclFP2020$ISO.Code[country.info.inclFP2020$FP2020 == "Yes"])
#----------------------------------------------------------------------
getRunnameUNPD <- function() {
  run.name <- "Run20140520" # change JR, 20140612
  return(list(name = "UNPD 2014", run.name = run.name))
}
#----------------------------------------------------------------------
subsetData <- function(
  data,
  iso.select = NULL,
  name.country.select = NULL # change JR, 20140409
) {
  names(data)[grepl("Country.letter.code|Country..letter.code", names(data))] <- "Country..letter.code" # change JR, 20140331
  names(data)[names(data) == "Age.range"] <- "Age..range"
  names(data)[grepl("exclude", names(data), ignore.case = TRUE)] <- "EXCLUDE1isyes"
  names(data)[grepl("GEO.biases", names(data), ignore.case = TRUE)] <- "GEO.biases..unknown.direction." # change JR, 20140612
  if (is.null(data$New.population))
    data$New.population <- rep(NA, nrow(data))
  if (!is.null(iso.select))
    data <- data[gsub(" ", "", data$Country..letter.code) == gsub(" ", "", iso.select), ]
  # change JR, 20140612
  vars <- c("Country", "New.population", "Country..letter.code",
            "Data.series.type", "Source.name", "Start.year", "End.year", 
            "Contraceptive.use.ANY", "Contraceptive.use.MODERN", "Unmet", 
            "Age..range", "Population.type", 
            "GEO.biases..unknown.direction.", "Non.pregnant.and.other.positive.biases", "Modern.method.bias",
            "Folk.method.positive.bias", "EXCLUDE1isyes")
  check <- !(vars %in% colnames(data))
  if (any(check))
    stop(paste0("The following variables cannot be found in the data file: ", 
                paste(vars[check], collapse = ", ")))
  # change JR, 20140409
  if (is.null(name.country.select)) {
    data.output <- data.frame(Country = data$Country)
  } else {
    data.output <- data.frame(Country = name.country.select,
                              Population = data$New.population) 
  }  
  data.output <- data.frame(data.output, 
                            Source.type = data$Data.series.type,
                            Source.name = data$Source.name,
                            Year = ifelse(data$Start.year == data$End.year, 
                                          data$Start.year, paste0(data$Start.year, "-", data$End.year)), 
                            Any.method = data$Contraceptive.use.ANY,
                            Any.modern.method = data$Contraceptive.use.MODERN,
                            Unmet.need = data$Unmet,
                            Age = data$Age..range, 
                            Population = data$Population.type)  
  if (any(!is.na(data$GEO.biases..unknown.direction.)))
    data.output <- data.frame(data.output, Geo.biases = data$GEO.biases..unknown.direction.)
  if (any(!is.na(data$Non.pregnant.and.other.positive.biases)))
    data.output <- data.frame(data.output, Non.pregnant.positive.biases = data$Non.pregnant.and.other.positive.biases)
  if (any(!is.na(data$Modern.method.bias)))
    data.output <- data.frame(data.output, Modern.method.bias = data$Modern.method.bias)
  if (any(!is.na(data$Folk.method.positive.bias)))
    data.output <- data.frame(data.output, Folk.method.positive.bias = data$Folk.method.positive.bias)
  if (any(!is.na(data$EXCLUDE1isyes) & data$EXCLUDE1isyes != 0))
    data.output <- data.frame(data.output, Exclude = data$EXCLUDE1isyes)
  return(data.output = data.output)
}
#----------------------------------------------------------------------
outputResults <- function(
  run.name,
  indicator, ##<< Any of the following choices: "Total", "Modern", "Traditional", 
  ## "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "Met Demand" ("Met Demand" only for \code{type.is.prop = TRUE})
  type.is.prop = TRUE, # change JR, 20140401
  iso.select = NULL
) {
  res <- read.csv(file.path("tables", paste0(run.name, 
                                             ifelse(type.is.prop, "_Country_perc_", "_Country_count_"), # change JR, 20140401
                                             indicator, ".csv")), 
                  header = T, stringsAsFactors = F)
  if (!is.null(iso.select))
    res <- res[res$Iso == getUNCode(run.name = run.name, iso = iso.select), ] # change JR, 20140409
  res <- melt(res, id.vars = c("Name", "Iso", "Percentile"), variable.name = "Year", 
              value.name = "Estimate")
  results <- data.frame(Country = res$Name,
                        Year = as.numeric(as.character(gsub("X", "", res$Year))), 
                        Percentile = res$Percentile*100,
                        Estimate = roundoff(res$Estimate*ifelse(type.is.prop, 100, 1), 
                                            digits = ifelse(type.is.prop, 1, 0))) # change JR, 20140602
  names(results)[names(results) == "Country"] <- "Country/population" # change JR, 20140409
  names(results)[names(results) == "Estimate"] <- paste0(getIndicatorLabel(indicator), " ",
                                                         ifelse(type.is.prop, "(%)", "('000s)")) # change JR, 20140402
  return(results)
}
#----------------------------------------------------------------------
getIndicatorLabel <- function(indicator) { # change JR, 20140402
  indicator.labels <- c("Total CP", "Modern CP", "Traditional CP", "Unmet need in FP", 
                        "Total demand in FP", "Demand in FP (excl modern)" )
  names(indicator.labels) <- c("Total", "Modern", "Traditional", "Unmet", 
                               "TotalPlusUnmet", "TradPlusUnmet")
  return(indicator.labels[paste0(indicator)])
}
#----------------------------------------------------------------------
getNameCountry <- function(iso) { # change JR, 20140409
  run.name.global <- getRunnameUNPD()$run.name
  load(file.path("output", run.name.global, "data.global.rda"))
  return(names(data.global$iso.c)[match(iso, data.global$iso.c)])
}
#----------------------------------------------------------------------
getUNCode <- function(
  run.name,
  iso
) {
  data <- read.csv(file.path("output", run.name, "dataCPmodel_input.csv"), 
                   header = T, stringsAsFactors = F)
  uncode <- data$ISO.code[match(iso, gsub(" ", "", data$Country..letter.code))]
  return(uncode)
}
#----------------------------------------------------------------------
roundoff <- function(x, digits) {
  z <- trunc(abs(x)*10^digits + 0.5)
  z <- sign(x)*z/10^digits
  return(z)
}
#----------------------------------------------------------------------
ShinyPlotResults <- function(# Plot results.
  run.name = "test", ##<< Run name
  iso.select = NULL, ##<< ISO country code of select country to plot results for
  plot.prop = TRUE, ##<< Plot proportions? If \code{FALSE}, plot counts.
  add.info = TRUE, ##<< Add detailed data info
  categories.to.plot = NULL, ##<< Selected names of categories to plot
  cex.adj.factor = 1
) {
  output.dir <- file.path(getwd(), "output", run.name, "/")
  
  load(file = paste(output.dir,"par.ciq.rda", sep = ""))
  load(file = paste(output.dir,"mcmc.meta.rda", sep = ""))
  do.country.specific.run <- mcmc.meta$general$do.country.specific.run # change JR, 20131104 
  load(file = paste(output.dir,"res.country.rda", sep = ""))

  if (is.null(iso.select)) {
    select.c <- NULL
  } else {
    select.c <- which(gsub(" ", "", mcmc.meta$data.raw$country.info$code.c) == iso.select)
  }
  
  if (plot.prop) {
    data.raw <- mcmc.meta$data.raw
    CI.Lg.Lcat.qt <- res.country$CIprop.Lg.Lcat.qt
    CIratio.Lg.Lcat.qt <- res.country$CIratio.Lg.Lcat.qt
  } else {
    CI.Lg.Lcat.qt <- res.country$CIcount.Lg.Lcat.qt
    data.raw <- CIratio.Lg.Lcat.qt <- NULL
  }
  
  PlotDataAndEstimates(data.raw = data.raw, 
                       CI.Lg.Lcat.qt = CI.Lg.Lcat.qt,
                       CIratio.Lg.Lcat.qt = CIratio.Lg.Lcat.qt,
                       select.c = select.c,
                       add.info = add.info,
                       categories.to.plot = categories.to.plot,
                       plot.prop = plot.prop,
                       ymin.at.0 = FALSE,
                       ymax.at.100 = FALSE,
                       fig.name = NULL,
                       cex.adj.factor = cex.adj.factor)
}
#----------------------------------------------------------------------
# To get plot categories for input selection
getPlotCategories <- function(type) {
  if (is.null(type)) type <- "perc"
  choices <- c("Total CP" = "Total", 
               "Modern CP" = "Modern", 
               "Traditional CP" = "Traditional", 
               "Ratio of modern/total CP" = "Modern/Total", 
               "Unmet need in FP" = "Unmet",
               "Total demand in FP" = "TotalPlusUnmet", 
               "Met demand in FP" = "Met Demand",
               "Data details" = "Show Data")
  selected <- c("Total", "Modern", "Traditional",
                "Modern/Total", "Unmet", 
                "TotalPlusUnmet", "Met Demand",
                "Show Data")
  if (type == "count") {
    choices <- choices[-c(4, 7)]
    selected <- selected[-c(4, 7)]
  }
  return(list(choices = choices, selected = selected))
}
#----------------------------------------------------------------------
outputResultsCompare <- function(
  run.name1,
  run.name2,
  indicator, ##<< Any of the following choices: "Total", "Modern", "Traditional", 
  ## "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "Met Demand" ("Met Demand" only for \code{type.is.prop = TRUE})
  type.is.prop = TRUE, # change JR, 20140401
  iso.select
) {
  res1 <- outputResults(run.name = run.name1, indicator = indicator, type.is.prop = type.is.prop, # change JR, 20140401
                        iso.select = iso.select)
  res2 <- outputResults(run.name = run.name2, indicator = indicator, type.is.prop = type.is.prop, # change JR, 20140401
                        iso.select = iso.select)
  estimate.col.index <- 4
  years <- union(res1$Year, res2$Year)
  percentiles <- unique(union(res1$Percentile, res2$Percentile))
  Year <- as.numeric(rep(years, each = length(percentiles)))
  Percentile <- rep(percentiles, length(years))
  Country <- as.character(rep(res1$Country[1], length(Year)))
  Estimate.1 <- Estimate.2 <- rep(NA, length(Year))
  estimate.col.index <- 4
  Estimate.1[is.element(Year, res1$Year)] <- res1[, estimate.col.index]
  Estimate.2[is.element(Year, res2$Year)] <- res2[, estimate.col.index]
  results <- data.frame(Country = Country,
                        Year = Year, 
                        Percentile = Percentile,
                        Estimate.1 = Estimate.1,
                        Estimate.2 = Estimate.2)[order(Year), ]
  names(results)[names(results) == "Country"] <- "Country/population" # change JR, 20140409
  names(results)[grepl("Estimate", names(results))] <- paste0(names(res1)[estimate.col.index], " ", 1:2)
  return(results)  
}
#----------------------------------------------------------------------
# Differs from PlotComparison in that:
# - isocompare.c => iso.select
# - run.name => run.name1
# - cex.adj.factor = 1 => 0.8
# - fig.name => NULL
ShinyPlotComparison <- function(# Plot lots of results!
  ### Wrapper function to plot lots of results.
  run.name1 = "test", ##<< Run name
  run.name2 = "test2", ##<< Run name
  iso.select = NULL, ##<< Vector of 3-character ISO country code of countries to plot comparison for. 
  ## If NULL, all countries with estimates in \code{run.name1} are plotted.
  plot.prop = TRUE, ##<< Plot proportions? If \code{FALSE}, plot counts.
  legend = run.name1, ##<< Run name
  legend2 = run.name2, ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name1}, default from \code{runMCMC}.
  output.dir2 = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name2}, default from \code{runMCMC}.
  add.info = TRUE, ##<< Add detailed data info
  categories.to.plot = NULL, ##<< Selected names of categories to plot.
  cex.adj.factor = 1
){
  if (is.null(output.dir2))
    output.dir2 <- file.path(getwd(), "output", run.name2, "/")
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name1, "/")
  
  load(file = paste(output.dir2,"res.country.rda", sep = ""))
  res.country2 <- res.country
  load(file = paste(output.dir,"mcmc.meta.rda", sep = ""))
  load(file = paste(output.dir,"res.country.rda", sep = ""))
  
  if (is.null(iso.select)) {
    iso.select <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
    select.c <- NULL
  } else {
    code.c <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
    if (any(!is.element(iso.select, code.c))) {
      stop(paste0("iso.select: ", paste(iso.select[!is.element(iso.select, code.c)], collapse = ", "),
                  " not found in results of run.name1"))
      return(invisible())
    }
    select.c <- match(iso.select, code.c)
  }
  # order country lists of res.country2 so that it has the same country order as res.country
  iso.order <- match(res.country$iso.g, res.country2$iso.g)
  res.country2$CIprop.Lg.Lcat.qt <- res.country2$CIprop.Lg.Lcat.qt[iso.order]
  res.country2$CIratio.Lg.Lcat.qt <- res.country2$CIratio.Lg.Lcat.qt[iso.order]
  res.country2$CIcount.Lg.Lcat.qt <- res.country2$CIcount.Lg.Lcat.qt[iso.order]
  
  if (plot.prop) {
    data.raw <- mcmc.meta$data.raw
    CI.Lg.Lcat.qt <- res.country$CIprop.Lg.Lcat.qt
    CIratio.Lg.Lcat.qt <- res.country$CIratio.Lg.Lcat.qt
    CI2.Lg.Lcat.qt <- res.country2$CIprop.Lg.Lcat.qt
    CIratio2.Lg.Lcat.qt <- res.country2$CIratio.Lg.Lcat.qt
  } else {
    CI.Lg.Lcat.qt <- res.country$CIcount.Lg.Lcat.qt
    CI2.Lg.Lcat.qt <- res.country2$CIcount.Lg.Lcat.qt
    data.raw <- CIratio.Lg.Lcat.qt <- CIratio2.Lg.Lcat.qt <- NULL
  }
  #------------------------------------------------------------------------------------------
  ##details<< Plot country overview plots for proportions without details using
  ##\code{\link{PlotDataAndEstimates}}.
  PlotDataAndEstimates(data.raw = data.raw, 
                       CI.Lg.Lcat.qt = CI.Lg.Lcat.qt,
                       CIratio.Lg.Lcat.qt = CIratio.Lg.Lcat.qt,
                       CI2.Lg.Lcat.qt = CI2.Lg.Lcat.qt,
                       CIratio2.Lg.Lcat.qt = CIratio2.Lg.Lcat.qt,
                       select.c = select.c,
                       plot.prop = plot.prop,
                       name.dir1 = legend,
                       name.dir2 = legend2,
                       add.info = add.info,
                       categories.to.plot = categories.to.plot,
                       ymin.at.0 = FALSE,
                       ymax.at.100 = FALSE,
                       fig.name = NULL,
                       cex.adj.factor = cex.adj.factor)
}
#----------------------------------------------------------------------
EmptyPlot <- function() {
  plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}
#----------------------------------------------------------------------
# Source: https://github.com/leonawicz  
helpPopup <- function(
  title, content,
  placement = c("right", "top", "left", "bottom"),
  trigger = c("focus", "hover", "click", "manual")
){
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", `data-toggle` = "popover", #class = "btn btn-mini",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      tags$i(class="icon-question-sign")
    )
  )
}
