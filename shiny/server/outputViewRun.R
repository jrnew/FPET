#----------------------------------------------------------------------
# outputViewRun.R
# Outputs and reactives for view run
# Jin Rou New, 2013
#----------------------------------------------------------------------
# Main reactive function for viewrun
getResultsExisting <- reactive({
  if (input$chooseAction == "viewrun" & !is.null(input$runnameExisting)) {
    if (input$isoselectview != "???") {
      run.name <- input$runnameExisting
      iso.select <- input$isoselectview
      results <- outputResults(run.name = run.name, indicator = input$resultsIndicatorView, 
                               type.is.prop = ifelse(input$resultsTypeView == "perc", TRUE, FALSE), # change JR, 20140401
                               iso.select = iso.select)
      return(results)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
})
#----------------------------------------------------------------------
# Get run.name and ISO
output$selectRunnameExisting <- renderUI({
  if (input$chooseAction == "viewrun") { 
    selectInput("runnameExisting", "Run name:", choices = runsCompleted(), selectize = FALSE)
  }
})                           
output$selectISOView <- renderUI({
  if (input$chooseAction != "viewrun") return(NULL)
  selectInput("isoselectview", "Country/population:", choices = getISOs(), selectize = FALSE)
})
#----------------------------------------------------------------------
# To get file path of database used
getFilePathExisting <- reactive({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  data.file.path <- file.path("output", input$runnameExisting, 
                              "dataCPmodel_input.csv")
  return(data.file.path)
})
#----------------------------------------------------------------------
# To display data
readInDataExisting <- reactive({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  if (input$runnameExisting == getRunnameUNPD()$run.name) {
    if (input$isoselectview != "???") {
      data.unpd <- read.csv(file = getFilePathExisting(), header = T, stringsAsFactors = F)
      data.output <- subsetData(data.unpd, iso.select = input$isoselectview)
    } else {
      load("shiny/data.output_UNPD2014.rda")
    }
  } else {
    data.unpd <- read.csv(file = getFilePathExisting(), header = T, stringsAsFactors = F)
    # change JR, 20140409
    load(file.path("output", input$runnameExisting, "mcmc.meta.rda"))
    name.country.select <- mcmc.meta$data.raw$country.info$name.country.select
    if (input$isoselectview != "???") {
      data.output <- subsetData(data.unpd, iso.select = input$isoselectview, 
                                name.country.select = name.country.select)
    } else {
      data.output <- subsetData(data.unpd, 
                                name.country.select = name.country.select)
    }
  }
  return(obs = data.output)
})
#----------------------------------------------------------------------
# Data table
output$countryDataExisting <- renderDataTable({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    obs <- readInDataExisting()
    if (!is.null(obs)) {
      setProgress(message = 'Loading', detail = 'Please wait...',
                  value = 60)
      obs
    }
  })  
}, options = list(aLengthMenu = c(5, 10, 20, 50, 100), 
                  bFilter = FALSE,
                  bSortClasses = TRUE, 
                  iDisplayLength = 10))
#----------------------------------------------------------------------
# To display MWRA data # change JR, 20140401
readInMWRADataExisting <- reactive({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  run.name <- input$runnameExisting
  load(file.path("output", run.name, "mcmc.meta.rda"))
  load(file.path("output", run.name, "res.country.rda"))
  if (input$isoselectview == "???") {
    years <- as.numeric(as.character(colnames(res.country$CIprop.Lg.Lcat.qt[[1]][[1]])))-0.5
    MWRA.table <- data.frame(Country = rep(mcmc.meta$data.raw$country.info$name.c, each = length(years)), 
                             Year = rep(years, mcmc.meta$winbugs.data$C),
                             MWRA = 1000*unlist(res.country$W.Lg.t))
  } else {
    iso.select <- input$isoselectview
    c.select <- which(gsub(" ", "", mcmc.meta$data.raw$country.info$code.c) == iso.select)
    years <- as.numeric(as.character(colnames(res.country$CIprop.Lg.Lcat.qt[[c.select]][[1]])))-0.5
    MWRA.table <- data.frame(Country = rep(mcmc.meta$data.raw$country.info$name.c[c.select], length(years)),
                             Year = years,
                             MWRA = 1000*res.country$W.Lg.t[[c.select]])
  }
  names(MWRA.table)[names(MWRA.table) == "Country"] <- "Country/population" # change JR, 20140409
  return(MWRA.table)
})
#----------------------------------------------------------------------
# MWRA data table # change JR, 20140401
output$countryMWRADataExisting <- renderDataTable({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    MWRA <- readInMWRADataExisting()
    if (!is.null(MWRA)) {
      setProgress(message = 'Loading', detail = 'Please wait...',
                  value = 60)
      MWRA
    }
  })  
}, options = list(aLengthMenu = c(5, 10, 20, 50, 100), 
                  bFilter = FALSE,
                  bSortClasses = TRUE, 
                  iDisplayLength = 10))
#----------------------------------------------------------------------
# Display results data table
output$resultsViewExisting <- renderDataTable({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    results <- getResultsExisting()
    if (is.null(results)) return(NULL)
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 60)
    results
  })
}, options = list(aLengthMenu = c(5, 10, 20, 50),
                  bFilter = FALSE,
                  bSortClasses = TRUE, 
                  iDisplayLength = 10))
#----------------------------------------------------------------------
# To get plot categories
output$selectPlotCategoriesView <- renderUI({
  if (input$chooseAction != "viewrun") return(NULL)
  div(class = "span12",
      checkboxGroupInput("plotCategoriesView", "Display the following: ", 
                         choices = getPlotCategories(input$plotTypeView)$choices, 
                         selected = getPlotCategories(input$plotTypeView)$selected),
      tags$style(type="text/css", 
                 HTML(paste0("#", "plotCategoriesView", 
                             ">*{float: left; margin-right: 15px; height: 20px;} #", 
                             "plotCategoriesView", " {height: 20px;}"))))
})
#----------------------------------------------------------------------
# Show plot
output$resultsPlotExisting <- renderPlot({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    if (input$chooseAction != "viewrun") EmptyPlot()
    if (is.null(input$runnameExisting)) EmptyPlot()
    if (input$runnameExisting == "NULL") EmptyPlot()
    if (input$isoselectview == "???") EmptyPlot()
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 90)
    run.name <- input$runnameExisting
    iso.select <- input$isoselectview
    ShinyPlotResults(run.name = run.name, iso.select = iso.select, 
                     plot.prop = ifelse(input$plotTypeView == "perc", TRUE, FALSE), # change JR, 20140411
                     add.info = "Show Data" %in% input$plotCategoriesView, # change JR, 20140409
                     categories.to.plot = input$plotCategoriesView[!(input$plotCategoriesView %in% "Show Data")], # change JR, 20140409
                     cex.adj.factor = 0.7)
  })
})
#----------------------------------------------------------------------
# Make results available for download
output$downloadEstimatesExisting <- downloadHandler(
  filename <- function() {
    paste0("Results ", input$runnameExisting, "_", input$isoselectview,
           "_", paste(input$resultsIndicatorView, collapse = "-"), " ", Sys.Date(), ".csv")
  },
  content <- function(file) {
    results <- getResultsExisting()
    write.csv(results, file, row.names = F, na = "")
  }
)
#----------------------------------------------------------------------
# Make plot available for download
output$downloadPlotExisting <- downloadHandler(
  filename <- function() {
    paste0("Results Plot ", input$runnameExisting, "_", input$isoselectview, " ", Sys.Date(), ".pdf")
  },
  content <- function(file) {
    pdf(file, width = 21, height = 12)
    run.name <- input$runnameExisting
    iso.select <- input$isoselectview
    ShinyPlotResults(run.name = run.name, iso.select = iso.select, 
                     plot.prop = ifelse(input$plotTypeView == "perc", TRUE, FALSE), # change JR, 20140411
                     add.info = "Show Data" %in% input$plotCategoriesView, # change JR, 20140409
                     categories.to.plot = input$plotCategoriesView[!(input$plotCategoriesView %in% "Show Data")]) # change JR, 20140409
    dev.off()
  }
)
#----------------------------------------------------------------------
output$postprobOutputExisting <- renderText({ # change JR, 20140210
  if (is.null(input$isoselectview)) return(NULL)
  if (input$isoselectview == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 40)
    if (is.null(input$indicatorppview) | is.null(input$yearppview) | is.null(input$booleanpview) |
          is.null(input$postprobInputview)) return(NULL)
    postprob <- GetPosteriorProbFromCutoffProp(run.name = input$runnameExisting,
                                               iso.select = gsub(" ", "", input$isoselectview),
                                               indicator = input$indicatorpview,
                                               year = input$yearpview+0.5,
                                               cutoffprop = input$percInputview/100,
                                               sign = input$booleanpview)
    paste0(round(postprob*100), "%")
  })
})
#----------------------------------------------------------------------
output$percOutputExisting <- renderText({ # change JR, 20140210
  if (is.null(input$isoselectview)) return(NULL)
  if (input$isoselectview == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 40)
    if (is.null(input$indicatorppview) | is.null(input$yearppview) | is.null(input$booleanppview) |
          is.null(input$postprobInputview)) return(NULL)
    res <- GetPropFromPosteriorProb(run.name = input$runnameExisting,
                                    iso.select = gsub(" ", "", input$isoselectview),
                                    indicator = input$indicatorppview,
                                    year = input$yearppview+0.5,
                                    postprob = input$postprobInputview/100,
                                    sign = input$booleanppview)
    paste0(round(res$prop*100), "%")
  })
})
#----------------------------------------------------------------------
output$postprobwOutputExisting <- renderText({ # change JR, 201403112
  if (is.null(input$isoselectview)) return(NULL)
  if (input$isoselectview == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 40)
    if (is.null(input$indicatorpwview) | is.null(input$yearpwview) | is.null(input$booleanpwview) |
          is.null(input$nwomenInputview) | is.null(input$selectwomenpwview)) return(NULL)
    if (input$selectwomenpwview == "total") {
      cutoffwomen <- input$nwomenInputview
      cutoffwomenadd <- year.current <- NULL
    } else {
      cutoffwomen <- NULL
      cutoffwomenadd <- input$nwomenInputview
      year.current <- input$yearstartpwview+0.5
    }
    postprob <- GetPosteriorProbFromCutoffProp(run.name = input$runnameExisting,
                                               iso.select = gsub(" ", "", input$isoselectview),
                                               indicator = input$indicatorpwview,
                                               year = input$yearpwview+0.5,
                                               cutoffwomen = cutoffwomen,
                                               cutoffwomenadd = cutoffwomenadd, 
                                               year.current = year.current,
                                               sign = input$booleanpwview)
    paste0(round(postprob*100), "%")
  })
})
#----------------------------------------------------------------------
output$nwomenOutputExisting <- renderText({ # change JR, 201403112
  if (is.null(input$isoselectview)) return(NULL)
  if (input$isoselectview == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 40)
    if (is.null(input$indicatorppwview) | is.null(input$yearppwview) | is.null(input$booleanppwview) |
          is.null(input$postprobwInputview) | is.null(input$selectwomenppwview)) return(NULL)
    if (input$selectwomenppwview == "total") {
      year.current <- NULL
    } else if (input$selectwomenppwview == "add") {
      if (is.null(input$yearstartppwview)) {
        return(NULL)
      } else {
        year.current <- input$yearstartppwview+0.5
      }
    }
    res <- GetPropFromPosteriorProb(run.name = input$runnameExisting,
                                    iso.select = gsub(" ", "", input$isoselectview),
                                    indicator = input$indicatorppwview,
                                    year = input$yearppwview+0.5,
                                    postprob = input$postprobwInputview/100,
                                    year.current = year.current,
                                    sign = input$booleanppwview)
    if (input$selectwomenppwview == "total") {
      paste0(round(res$women))
    } else {
      paste0(round(res$women.add))
    }
  })
})
#----------------------------------------------------------------------
output$targetPanelExisting1 <- renderUI ({
  wellPanel(
    p("Q: What is the probability that "),
    selectInput("indicatorpview", "",
                choices = c("total CP" = "Total", 
                            "modern CP" = "Modern", 
                            "traditional CP" = "Traditional", 
                            "unmet need in FP" = "Unmet",
                            "total demand in FP" = "TotalPlusUnmet", 
                            "demand in FP (excl modern)" = "TradPlusUnmet" 
                            # , "met demand in FP" = "Met Demand"
                ), selected = "Total", selectize = FALSE),
    br(" in the year "),
    numericInput("yearpview", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" is "),
    selectInput("booleanpview", "",
                choices = c("above" = ">", 
                            # "at least" = ">=", 
                            "below" = "<"
                            #, "at most" = "<="
                ), selected = ">", selectize = FALSE),
    numericInput("percInputview", "", 50,
                 min = 0, max = 100, step = 1),
    br("%?"),
    br("A:"),
    strong(textOutput("postprobOutputExisting"))
  )
})
#----------------------------------------------------------------------
output$targetPanelExisting2 <- renderUI ({
  wellPanel(
    p("Q: What target value of "),
    selectInput("indicatorppview", "",
                choices = c("total CP" = "Total", 
                            "modern CP" = "Modern", 
                            "traditional CP" = "Traditional", 
                            "unmet need in FP" = "Unmet",
                            "total demand in FP" = "TotalPlusUnmet", 
                            "demand in FP (excl modern)" = "TradPlusUnmet" 
                            # , "met demand in FP" = "Met Demand"
                ), selected = "Total", selectize = FALSE),
    br(" in the year"),
    numericInput("yearppview", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" corresponds to an attainment probability of "),
    numericInput("postprobInputview", "", 90,
                 min = 0, max = 100, step = 1),
    br("%, where attainment probability refers to the probability of an outcome "),
    selectInput("booleanppview", "",
                choices = c(
                  "greater" = ">",
                  "smaller" = "<" 
                ), selected = ">", selectize = FALSE),
    br(" than the target value?"),
    br("A: "),
    strong(textOutput("percOutputExisting"))
  )
})
#----------------------------------------------------------------------
output$targetPanelExisting3 <- renderUI ({
  wellPanel(
    p("Q: What is the probability that the "),
    selectInput("selectwomenpwview", "",
                choices = c("total" = "total", 
                            "additional" = "add"
                ), selected = "total", selectize = FALSE),
    br(" number of women "),
    conditionalPanel(
      condition = "input.selectwomenpwview == 'add'",
      br("(relative to the year "),
      numericInput("yearstartpwview", "", 2012, 
                   min = 1990, max = 2020, step = 1),
      br(")")),
    selectInput("indicatorpwview", "",
                choices = c("on any contraception" = "Total", 
                            "on modern contraception" = "Modern", 
                            "on traditional contraception" = "Traditional", 
                            "with unmet need in FP" = "Unmet",
                            "with demand in FP" = "TotalPlusUnmet", 
                            "with demand in FP (excl modern)" = "TradPlusUnmet" 
                            # , "with met demand in FP" = "Met Demand"
                ), selected = "Total", selectize = FALSE),
    br(" in the year "),
    numericInput("yearpwview", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" is "),
    selectInput("booleanpwview", "",
                choices = c("above" = ">", 
                            # "at least" = ">=", 
                            "below" = "<"
                            #, "at most" = "<="
                ), selected = ">", selectize = FALSE),
    numericInput("nwomenInputview", "", 1000,
                 min = 0, max = NA, step = 10),
    br("A:"),
    strong(textOutput("postprobwOutputExisting"))
  )
})
#----------------------------------------------------------------------
output$targetPanelExisting4 <- renderUI ({
  wellPanel(
    p("Q: What target "),
    selectInput("selectwomenppwview", "",
                choices = c("total" = "total", 
                            "additional" = "add"
                ), selected = "total", selectize = FALSE),
    br(" number of women "),
    conditionalPanel(
      condition = "input.selectwomenppwview == 'add'",
      br("(relative to the year "),
      numericInput("yearstartppwview", "", 2012, 
                   min = 1990, max = 2020, step = 1),
      br(")")),
    selectInput("indicatorppwview", "",
                choices = c("on any contraception" = "Total", 
                            "on modern contraception" = "Modern", 
                            "on traditional contraception" = "Traditional", 
                            "with unmet need in FP" = "Unmet",
                            "with demand in FP" = "TotalPlusUnmet", 
                            "with demand in FP (excl modern)" = "TradPlusUnmet" 
                            # , "with met demand in FP" = "Met Demand"
                ), selected = "Total", selectize = FALSE),
    br(" in the year "),
    numericInput("yearppwview", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" corresponds to an attainment probability of "),
    numericInput("postprobwInputview", "", 90,
                 min = 0, max = 100, step = 1),
    br("%, where attainment probability refers to the probability of an outcome "),
    selectInput("booleanppwview", "",
                choices = c(
                  "greater" = ">", 
                  # "at least" = ">=", 
                  "smaller" = "<" 
                  # , "at most" = "<="
                ), selected = ">", selectize = FALSE),
    br(" than the target value."),
    br("A: "),
    strong(textOutput("nwomenOutputExisting"))
  )
})
#----------------------------------------------------------------------
output$changeOutputExisting <- renderText({ # change JR, 20140623
  if (is.null(input$areacview)) return(NULL)
  if (input$areacview == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 40)
    if (is.null(input$indicatorcview) | is.null(input$year1cview) | is.null(input$year2cview))
      return(NULL)
    if (input$areacview %in% getISOs()) {
      change <- GetChangeFromYear1ToYear2(run.name = input$runnameExisting,
                                          iso.select = getUNCode(run.name = input$runnameExisting,
                                                          iso = gsub(" ", "", input$areacview)), 
                                          indicator = input$indicatorcview, 
                                          year1 = input$year1cview+0.5, year2 = input$year2cview+0.5)
      P.uis <- round(quantile(change$P.s, probs = percentiles.for.change)*100, digits = 1)
      paste0(P.uis[2], "% (", P.uis[1], "%, ", P.uis[3], "%)")
    } else if (input$areacview %in% getRegions() & input$runnameExisting == getRunnameUNPD()$run.name) {
      output.dir <- file.path("output", input$runnameExisting)
      load(file.path(output.dir, "mcmc.meta.rda"))
      load(file.path(output.dir, "countrytrajectories", paste0("P.tp3s_country", 1, ".rda")))
      iso.c <- mcmc.meta$data.raw$country.info$iso.c
      if (input$areacview == "FP2020 countries") {
        select.c <- iso.c %in% countrycodes.FP2020
      } else {
        select.c <- SelectCountriesInRegion(region.name = input$areacview,
                                            country.info = mcmc.meta$data.raw$country.info,
                                            region.info = mcmc.meta$data.raw$region.info)
      }
      n.sim <- dim(P.tp3s)[3]
      P.cs <- array(NA, dim = c(sum(select.c), n.sim))
      for (i in 1:sum(select.c)) {
        change <- GetChangeFromYear1ToYear2(run.name = input$runnameExisting,
                                            iso.select = gsub(" ", "", iso.c[select.c][i]),  
                                            indicator = input$indicatorcview, 
                                            year1 = input$year1cview+0.5, year2 = input$year2cview+0.5)
        P.cs[i, ] <- change$P.s
        setProgress(message = 'Loading', detail = 'Please wait...',
                    value = 40+50*i/sum(select.c))
      }
      P.uis <- round(quantile(colSums(P.cs), probs = percentiles.for.change)*100, digits = 1)
      paste0(P.uis[2], "% (", P.uis[1], "%, ", P.uis[3], "%)")
    } else {
      NULL
    }
  })
})
#----------------------------------------------------------------------
output$changewOutputExisting <- renderText({ # change JR, 20140623
  if (is.null(input$areacwview)) return(NULL)
  if (input$areacwview == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 40)
    if (is.null(input$indicatorcwview) | is.null(input$year1cwview) | is.null(input$year2cwview))
      return(NULL)
    if (input$areacwview %in% getISOs()) {
      change <- GetChangeFromYear1ToYear2(run.name = input$runnameExisting,
                                          iso.select = getUNCode(run.name = input$runnameExisting,
                                                          iso = gsub(" ", "", input$areacwview)), 
                                          indicator = input$indicatorcwview, 
                                          year1 = input$year1cwview+0.5, year2 = input$year2cwview+0.5)
      WP.uis <- round(quantile(change$WP.s, probs = percentiles.for.change)*1000, digits = 0)
      paste0(WP.uis[2], " (", WP.uis[1], ", ", WP.uis[3], ")")
    } else if (input$areacwview %in% getRegions() & input$runnameExisting == getRunnameUNPD()$run.name) {
      load(file.path(output.dir, "mcmc.meta.rda"))
      load(file.path(output.dir, "countrytrajectories", paste0("P.tp3s_country", 1, ".rda")))
      iso.c <- mcmc.meta$data.raw$country.info$iso.c
      if (input$areacwview == "FP2020 countries") {
        select.c <- iso.c %in% countrycodes.FP2020
      } else {
        select.c <- SelectCountriesInRegion(region.name = input$areacwview,
                                            country.info = mcmc.meta$data.raw$country.info,
                                            region.info = mcmc.meta$data.raw$region.info)
      }
      n.sim <- dim(P.tp3s)[3]
      WP.cs <- array(NA, dim = c(sum(select.c), n.sim))
      for (i in 1:sum(select.c)) {
        change <- GetChangeFromYear1ToYear2(run.name = input$runnameExisting,
                                            iso.select = gsub(" ", "", iso.c[select.c][i]), 
                                            indicator = input$indicatorcwview, 
                                            year1 = input$year1cwview+0.5, year2 = input$year2cwview+0.5)
        WP.cs[i, ] <- change$WP.s  
        setProgress(message = 'Loading', detail = 'Please wait...',
                    value = 40+50*i/sum(select.c))
      }
      WP.uis <- round(quantile(colSums(WP.cs), probs = percentiles.for.change)*1000, digits = 0)
      paste0(WP.uis[2], " (", WP.uis[1], ", ", WP.uis[3], ")")
    } else {
      NULL
    }
  })
})
#----------------------------------------------------------------------
output$progressPanelExisting1 <- renderUI ({
  wellPanel(
    p("The change in "),
    selectInput("indicatorcview", "",
                choices = c("total CP" = "Total", 
                            "modern CP" = "Modern", 
                            "traditional CP" = "Traditional", 
                            "unmet need in FP" = "Unmet",
                            "total demand in FP" = "TotalPlusUnmet", 
                            "demand in FP (excl modern)" = "TradPlusUnmet" 
                            # , "met demand in FP" = "Met Demand"
                ), selected = "Total", selectize = FALSE),
    br(" from the year "),
    numericInput("year1cview", "", 2012, 
                 min = 1990, max = 2020, step = 1),
    br(" to the year "),
    numericInput("year2cview", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br( " for "),
    selectInput("areacview", "",
                choices = c(getISOs()[1], getRegions(), getISOs()[-1]), selectize = FALSE),
    br(" is "),
    strong(textOutput("changeOutputExisting"))
  )
})
#----------------------------------------------------------------------
output$progressPanelExisting2 <- renderUI ({
  wellPanel(
    p("The change in the number of women"),
    selectInput("indicatorcwview", "",
                choices = c("on any contraception" = "Total", 
                            "on modern contraception" = "Modern", 
                            "on traditional contraception" = "Traditional", 
                            "with unmet need in FP" = "Unmet",
                            "with demand in FP" = "TotalPlusUnmet", 
                            "with demand in FP (excl modern)" = "TradPlusUnmet" 
                            # , "with met demand in FP" = "Met Demand"
                ), selected = "Total", selectize = FALSE),
    br(" from the year "),
    numericInput("year1cwview", "", 2012, 
                 min = 1990, max = 2020, step = 1),
    br(" to the year "),
    numericInput("year2cwview", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br( " for "),
    selectInput("areacwview", "",
                choices = c(getISOs()[1], getRegions(), getISOs()[-1]), selectize = FALSE),
    br(" is "),
    strong(textOutput("changewOutputExisting"))
  )
})
#----------------------------------------------------------------------
# UIs
output$countryDataExistingChart <- renderUI({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 90)
    dataTableOutput("countryDataExisting")
  })
})
output$countryMWRADataExistingChart <- renderUI({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 90)
    dataTableOutput("countryMWRADataExisting")
  })
})
output$resultsViewExistingChart <- renderUI({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  if (input$isoselectview == "???") return(NULL)
  div(p(strong("Results options")),
      fluidRow(
        # change JR, 20140401
        div(class = "span3",
            selectInput("resultsTypeView", "Result to display",
                        choices = c("Percentage" = "perc", 
                                    "Count (in '000s)" = "count"),
                        selected = "perc", selectize = FALSE)),
        div(class = "span3",
            selectInput("resultsIndicatorView", "Indicator to display",
                        choices = c("Total CP" = "Total", 
                                    "Modern CP" = "Modern", 
                                    "Traditional CP" = "Traditional", 
                                    "Unmet need in FP" = "Unmet",
                                    "Total demand in FP" = "TotalPlusUnmet", 
                                    "Demand in FP (excl modern)" = "TradPlusUnmet"
                                    # , "Met demand in FP" = "Met Demand"
                        ),
                        selected = "Total", selectize = FALSE)),
        div(class = "span6", align = "right",
            downloadButton("downloadEstimatesExisting", "Download results"))
      ),
      fluidRow(p()),
      dataTableOutput("resultsViewExisting")
  )
})
output$resultsPlotExistingChart <- renderUI({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  if (input$isoselectview == "???") return(NULL)
  div(p(strong("Graph options")),
      fluidRow(
        # change JR, 20140411
        div(class = "span3",
            selectInput("plotTypeView", "Result to display",
                        choices = c("Percentage" = "perc", 
                                    "Count (in '000s)" = "count"),
                        selected = "perc", selectize = FALSE)
        ),
        div(class = "span9", align = "right",
            downloadButton("downloadPlotExisting", "Download graph"))
      ),
      fluidRow(uiOutput("selectPlotCategoriesView")),
      fluidRow(p()),
      plotOutput("resultsPlotExisting", width = "1050px", height = "550px"))
})
output$targetPanelExistingAll <- renderUI({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  if (input$isoselectview == "???") return(NULL)
  div(
    h4("Information for target-setting"),
    p("Select entries below to find a probability associated with a given level of an indicator of interest and vice versa."),
    fluidRow(
      div(class = "span4", uiOutput("targetPanelExisting1")), 
      div(class = "span4", uiOutput("targetPanelExisting2"))
    ),
    fluidRow(
      div(class = "span4", uiOutput("targetPanelExisting3")),
      div(class = "span4", uiOutput("targetPanelExisting4"))
    )
  )
})
output$progressPanelExistingAll <- renderUI({
  if (input$chooseAction != "viewrun") return(NULL)
  if (is.null(input$runnameExisting)) return(NULL)
  if (input$runnameExisting == "NULL") return(NULL)
  # if (input$isoselectview == "???") return(NULL)
  div(
    h4("Information for measuring progress"),
    fluidRow(
      div(class = "span4", uiOutput("progressPanelExisting1")), 
      div(class = "span4", uiOutput("progressPanelExisting2"))
    )
  )
})
