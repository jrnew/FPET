#----------------------------------------------------------------------
# outputNewRun.R
# Outputs and reactives for new run
# Jin Rou New, 2014
#----------------------------------------------------------------------
# Start run alert # change JR, 20140418
observe({
  if (input$startRun == 0) return()
  if (input$isoselect == "???") return(NULL)
  if (duplicateRun()) return(NULL)
  showshinyalert(session, id = "startRunAlert", HTMLtext = paste("Run started! Proceed to <em>Log</em> tab."))
})

# Duplicate run alert # change JR, 20140418
observe({
  if (input$startRun == 0) return()
  if (input$isoselect == "???") return(NULL)
  if (duplicateRun()) {
    showshinyalert(session, id = "duplicateRunAlert", HTMLtext = paste("Run name already exists! Please use a different run name."), styleclass = "danger")
  } else {
    return(NULL)
  }
})

# Main reactive function for newrun
RunMCMCAndGetResults <- reactive({
  if (input$startRun == 0) return(NULL)
  if (input$isoselect == "???") return(NULL)
  if (duplicateRun()) return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)  
    isolate({
      setProgress(message = 'Run is in progress', detail = 'Please wait...',
                  value = 0)
      iso.select <- gsub(" ", "", input$isoselect)
      if (newPopulation()) {
        if (!is.null(input$isocountryselect)) {
          iso.country.select <- input$isocountryselect
        } else {
          return(NULL)
        }
      } else {
        iso.country.select <- NULL
      }
      run.name <- getRunName()
      run.name.global <- getRunnameUNPD()$run.name
      # File path of data.CSV file in UNPD database format for one country if default database is not used
      if (input$chooseDatabase == "default" | is.null(input$datafile$datapath)) {
        data.csv.file <- "data/dataCPmodel.csv"
      } else {
        data.csv.file <- input$datafile$datapath
      }
      # File path of MWRA.CSV file in UNPD database format if default database is not used # change JR, 20140401
      if (input$chooseMWRADatabase == "default" | is.null(input$MWRAfile$datapath)) {
        MWRA.csv.file <- "data/Number-of-women-married-in union_15-49.csv"
      } else {
        MWRA.csv.file <- input$MWRAfile$datapath
      }
      regioninfo.csv <- "data/Country-and-area-classification.csv"
      # check if do.SS.run
      data.preprocessed <- PreprocessData(data.csv = data.csv.file,
                                          iso.select = iso.select)
      select.ss <- grepl("Service statistic", data.preprocessed$Data.series.type)
      do.SS.run <- any(select.ss)
      if (do.SS.run & sum(select.ss) == 1) 
        stop(paste0("Only 1 observation of SS data is available. Not possible to use SS data for projection."))
      setProgress(message = 'Run is in progress', detail = 'Please wait...',
                  value = 1)
      #----------------------------------------------------------------------
      # Country-specific run
      print(paste0("Running in parallel? ", # change JR, 20140402
                   ifelse(getDoParWorkers() == 1,  "No.", paste0("Yes, with ", getDoParWorkers(), " cores."))))
      if (input$shortRun) {
        if (!do.SS.run) {
          RunMCMC(run.name = run.name, 
                  do.country.specific.run = TRUE,
                  iso.select = iso.select, 
                  iso.country.select = iso.country.select,
                  run.name.global = run.name.global,
                  data.csv = data.csv.file, 
                  regioninfo.csv = regioninfo.csv,
                  run.on.server = run.on.server,
                  N.ITER = 10, N.STEPS = 2, N.THIN = 1, N.BURNIN = 0, ChainNums = seq(1,3))
        } else {
          message("Service statistics data is available.")
          # First pass
          RunMCMC(run.name = run.name, 
                  do.country.specific.run = TRUE,
                  iso.select = iso.select, 
                  iso.country.select = iso.country.select,
                  run.name.global = run.name.global,
                  data.csv = data.csv.file, 
                  regioninfo.csv = regioninfo.csv,
                  run.on.server = run.on.server,
                  N.ITER = 10, N.STEPS = 2, N.THIN = 1, N.BURNIN = 0, ChainNums = seq(1,3))
          ConstructMCMCArray(run.name = run.name, do.SS.run.first.pass = TRUE)
          ConstructOutput(run.name = run.name,
                          MWRA.csv = MWRA.csv.file,
                          start.year = 1970.5, end.year = 2035.5, # change to shorter period including year required to estimate bias.modern to speed up?
                          do.SS.run.first.pass = TRUE)
          #----------------------------------------------------------------------
          # Second pass
          RunMCMC(run.name = run.name, 
                  do.country.specific.run = TRUE,
                  iso.select = iso.select, 
                  iso.country.select = iso.country.select,
                  run.name.global = run.name.global,
                  data.csv = data.csv.file, 
                  regioninfo.csv = regioninfo.csv,
                  run.on.server = run.on.server,
                  N.ITER = 10, N.STEPS = 2, N.THIN = 1, N.BURNIN = 0, ChainNums = seq(1,3))
        }
      } else {
        if (!do.SS.run) {
          RunMCMC(run.name = run.name, 
                  do.country.specific.run = TRUE,
                  iso.select = iso.select, 
                  iso.country.select = iso.country.select,
                  run.name.global = run.name.global,
                  data.csv = data.csv.file, 
                  regioninfo.csv = regioninfo.csv,
                  run.on.server = run.on.server)
        } else {
          message("Service statistics data is available.")
          # First pass
          RunMCMC(run.name = run.name, 
                  do.country.specific.run = TRUE,
                  iso.select = iso.select, 
                  iso.country.select = iso.country.select,
                  run.name.global = run.name.global,
                  data.csv = data.csv.file, 
                  regioninfo.csv = regioninfo.csv,
                  run.on.server = run.on.server)
          ConstructMCMCArray(run.name = run.name, do.SS.run.first.pass = TRUE)
          ConstructOutput(run.name = run.name,
                          MWRA.csv = MWRA.csv.file,
                          start.year = 1970.5, end.year = 2035.5, # change to shorter period including year required to estimate bias.modern to speed up?
                          do.SS.run.first.pass = TRUE)
          #----------------------------------------------------------------------
          # Second pass
          RunMCMC(run.name = run.name, 
                  do.country.specific.run = TRUE,
                  iso.select = iso.select, 
                  iso.country.select = iso.country.select,
                  run.name.global = run.name.global,
                  data.csv = data.csv.file, 
                  regioninfo.csv = regioninfo.csv,
                  run.on.server = run.on.server)
        }
      }
      setProgress(message = 'Run is in progress', detail = 'Reading output...',
                  value = 80)
      ConstructMCMCArray(run.name = run.name)
      setProgress(message = 'Run is in progress', detail = 'Constructing output...',
                  value = 82)
      ConstructOutput(run.name = run.name,
                      MWRA.csv = MWRA.csv.file,
                      start.year = 1990.5, # change JR, 20140409
                      end.year = 2020.5) # change JR, 20140409
      setProgress(message = 'Run complete!', value = 85)
      GetTablesRes(run.name = run.name, name.res = "Country")
      GetTablesChange(run.name = run.name, name.res = "Country")
      setProgress(message = 'Run complete!', value = 90)
      return("Done")
    })
  })
})
#----------------------------------------------------------------------
# To get ISOs for input
output$selectISO <- renderUI({
  if (input$chooseAction != "newrun") return(NULL)
  selectInput("isoselect", "Country/population:", choices = getISOs(), selectize = FALSE)
})
#----------------------------------------------------------------------
# To get ISOs for input # change JR, 20140409
output$selectISOCountry <- renderUI({
  if (!newPopulation()) return(NULL)
  run.name.global <- getRunnameUNPD()$run.name
  load(file.path("output", run.name.global, "data.global.rda"))
  iso.c <- c("???", data.global$iso.c)
  names(iso.c) <- c("Please select country", names(data.global$iso.c))
  selectInput("isocountryselect", "Country that this population belongs to:", choices = iso.c, selectize = FALSE)
})
#----------------------------------------------------------------------
# To get file path of database used
getFilePath <- reactive({
  if (input$chooseDatabase == "default") {
    data.file.path <- file.path("data", "dataCPmodel.csv")
  } else if (!is.null(input$datafile$datapath)) {
    data.file.path <- input$datafile$datapath
  } else {
    data.file.path <- NULL
  }
  return(data.file.path)
})
#----------------------------------------------------------------------
# To display data
readInData <- reactive({
  if (input$chooseDatabase == "default") {
    if (is.null(input$isoselect)) {
      load("shiny/data.output_UNPD2014.rda")
    } else if (input$isoselect == "???") {
      load("shiny/data.output_UNPD2014.rda")
    } else {
      data.unpd <- read.csv(file = getFilePath(), header = T, stringsAsFactors = F)
      data.output <- subsetData(data.unpd, iso.select = gsub(" ", "", input$isoselect))
    }
    obs <- data.output
  } else {
    if (!is.null(getFilePath())) {
      data.unpd <- read.csv(file = getFilePath(), header = T, stringsAsFactors = F)
      if (is.null(input$isoselect)) {
        data.output <- subsetData(data.unpd)
      } else if (input$isoselect == "???") {
        data.output <- subsetData(data.unpd)
      } else {
        # change JR, 20140409
        if (newPopulation()) {
          if (!is.null(input$isocountryselect)) {
            name.country.select <- getNameCountry(input$isocountryselect)
            data.output <- subsetData(data.unpd, iso.select = gsub(" ", "", input$isoselect),
                                      name.country.select = name.country.select)
          } else {
            data.output <- NULL
          }
        } else {
          data.output <- subsetData(data.unpd, iso.select = gsub(" ", "", input$isoselect))
        }
      }
      obs <- data.output
    } else {
      obs <- NULL
    }
  }
  return(obs = obs)
})
#----------------------------------------------------------------------
# Data table
output$countryData <- renderDataTable({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    obs <- readInData()
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
readInMWRAData <- reactive({
  if (input$chooseMWRADatabase == "default") {
    data.MWRA.file <- "data/Number-of-women-married-in union_15-49.csv"
  } else if (!is.null(input$MWRAfile$datapath)) {
    data.MWRA.file <- input$MWRAfile$datapath
  } else {
    return(NULL)
  }
  data.MWRA <- read.csv(file = data.MWRA.file, header = T, stringsAsFactors = F, strip.white = T)
  names(data.MWRA)[grepl("Country.letter.code|Country..letter.code", names(data.MWRA))] <- "Country.letter.code" # change JR, 2014015
  if (is.null(data.MWRA$New.population))
    data.MWRA$New.population <- rep(NA, nrow(data.MWRA)) # change JR, 20140412
  data.MWRA$Country <- ifelse(is.na(data.MWRA$New.population) | data.MWRA$New.population == "", 
                              data.MWRA$Country, data.MWRA$New.population) # change JR, 20140409
  data.MWRA.long <- melt(data.MWRA[, colnames(data.MWRA) != "New.population"], 
                         id.vars = c("Country.letter.code", "ISO.code", "Country"),  
                         variable.name = "Year", value.name = "MWRA")
  data.MWRA.long$Year <- gsub("X", "", data.MWRA.long$Year)
  # data.MWRA.long$Year <- as.numeric(as.character(data.MWRA.long$Year)) + 0.5
  if (is.null(input$isoselect)) {
    data.output <- data.MWRA.long[order(data.MWRA.long$Country, data.MWRA.long$Year), ]
  } else if (input$isoselect == "???") {
    data.output <- data.MWRA.long[order(data.MWRA.long$Country, data.MWRA.long$Year), ]
  } else {
    data.MWRA.subset <- data.MWRA.long[data.MWRA.long$Country.letter.code == gsub(" ", "", input$isoselect), ]
    data.output <- data.MWRA.subset[order(data.MWRA.subset$Year), ]
  }
  names(data.output)[names(data.output) == "Country"] <- "Country/population"
  data.output <- data.output[, !(colnames(data.output) %in% c("Country.letter.code", "ISO.code"))]
  return(data.output)
})
#----------------------------------------------------------------------
# MWRA data table # change JR, 20140401
output$countryMWRAData <- renderDataTable({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    MWRA <- readInMWRAData()
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
# Show log file
output$log <- renderPrint({
  if (input$startRun == 0) return(NULL)
  if (is.null(RunMCMCAndGetResults())) return(NULL)
  RunMCMCAndGetResults()
})
#----------------------------------------------------------------------
# Display results data table
output$resultsView <- renderDataTable({
  if (input$startRun == 0) return(NULL)
  if (is.null(RunMCMCAndGetResults())) return(NULL)
  RunMCMCAndGetResults()
  run.name <- getRunName()
  iso.select <- gsub(" ", "", input$isoselect)
  results <- outputResults(run.name = run.name, indicator = input$resultsIndicator, 
                           type.is.prop = ifelse(input$resultsType == "perc", TRUE, FALSE), # change JR 20140401
                           iso.select = iso.select)
  results
}, options = list(aLengthMenu = c(5, 10, 20, 50), 
                  bFilter = FALSE,
                  bSortClasses = TRUE, 
                  iDisplayLength = 10))
#----------------------------------------------------------------------
# Show plot
output$resultsPlot <- renderPlot({
  if (input$startRun == 0) {
    EmptyPlot()
  } else {
    withProgress(session, min=0, max=100, expr={
      RunMCMCAndGetResults()
      setProgress(message = 'Loading', detail = 'Please wait...',
                  value = 90)
      run.name <- getRunName()
      ShinyPlotResults(run.name = run.name,
                       plot.prop = ifelse(input$plotType == "perc", TRUE, FALSE), # change JR, 20140411
                       add.info = "Show Data" %in% input$plotCategories, # change JR, 20140409
                       categories.to.plot = input$plotCategories[!(input$plotCategories %in% "Show Data")], # change JR, 20140409
                       cex.adj.factor = 0.7)
    })
  }
})
#----------------------------------------------------------------------
# Make results available for download
output$downloadEstimates <- downloadHandler(
  filename <- function() {
    paste0("Results ", gsub(" ", "", input$isoselect), 
           paste(input$resultsIndicator, collapse = "-"),
           " ", Sys.Date(), ".csv")
  },
  content <- function(file) {
    results <- RunMCMCAndGetResults()
    run.name <- getRunName()
    iso.select <- gsub(" ", "", input$isoselect)
    results <- outputResults(run.name = run.name, indicator = input$resultsIndicator, 
                             type.is.prop = ifelse(input$resultsType == "perc", TRUE, FALSE), # change JR 20140401
                             iso.select = iso.select)
    write.csv(results, file, row.names = F, na = "")
  }
)
#----------------------------------------------------------------------
# Make plot available for download
output$downloadPlot <- downloadHandler(
  filename <- function() {
    paste0("Results Plot ", gsub(" ", "", input$isoselect), " ", Sys.Date(), ".pdf")
  },
  content <- function(file) {
    pdf(file, width = 21, height = 12)
    RunMCMCAndGetResults()
    run.name <- getRunName()
    ShinyPlotResults(run.name = run.name,
                     plot.prop = ifelse(input$plotType == "perc", TRUE, FALSE), # change JR, 20140411
                     add.info = "Show Data" %in% input$plotCategories, # change JR, 20140409
                     categories.to.plot = input$plotCategories[!(input$plotCategories %in% "Show Data")]) # change JR, 20140409
    dev.off()}
)
#----------------------------------------------------------------------
# To get plot categories
output$selectPlotCategories <- renderUI({
  if (input$chooseAction != "newrun") return(NULL)
  div(class = "span12",
      checkboxGroupInput("plotCategories", "Display the following: ", 
                         choices = getPlotCategories(input$plotType)$choices, 
                         selected = getPlotCategories(input$plotType)$selected),
      tags$style(type="text/css", 
                 HTML(paste0("#", "plotCategories", 
                             ">*{float: left; margin-right: 15px; height: 20px;} #", 
                             "plotCategories", " {height: 20px;}"))))
})
#----------------------------------------------------------------------
output$countryDataChart <- renderUI({
  if (input$chooseAction != "newrun") return(NULL)
  if (input$chooseDatabase == "other" & is.null(input$datafile$datapath)) return(NULL)
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 90)
    dataTableOutput("countryData")
  })
})
#----------------------------------------------------------------------
output$countryMWRADataChart <- renderUI({
  if (input$chooseAction != "newrun") return(NULL)
  if (input$chooseMWRADatabase == "other" & is.null(input$MWRAfile$datapath)) return(NULL)
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 90)
    dataTableOutput("countryMWRAData")
  })
})
#----------------------------------------------------------------------
output$postprobOutput <- renderText({ # change JR, 20140210
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    if (is.null(RunMCMCAndGetResults())) return(NULL)
    RunMCMCAndGetResults()
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 60)
    if (is.null(input$indicatorpp) | is.null(input$yearpp) | is.null(input$booleanp) |
          is.null(input$postprobInput)) return(NULL)
    run.name <- getRunName()
    postprob <- GetPosteriorProbFromCutoffProp(run.name = run.name,
                                               iso.select = gsub(" ", "", input$isoselect),
                                               indicator = input$indicatorp,
                                               year = input$yearp+0.5,
                                               cutoffprop = input$percInput/100,
                                               sign = input$booleanp)
    paste0(round(postprob*100), "%")
  })
})
#----------------------------------------------------------------------
output$percOutput <- renderText({ # change JR, 20140210
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    if (is.null(RunMCMCAndGetResults())) return(NULL)
    RunMCMCAndGetResults()
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 60)
    if (is.null(input$indicatorpp) | is.null(input$yearpp) | is.null(input$booleanpp) |
          is.null(input$postprobInput)) return(NULL)
    run.name <- getRunName()
    res <- GetPropFromPosteriorProb(run.name = run.name,
                                    iso.select = gsub(" ", "", input$isoselect),
                                    indicator = input$indicatorpp,
                                    year = input$yearpp+0.5,
                                    postprob = input$postprobInput/100,
                                    sign = input$booleanpp)
    paste0(round(res$prop*100), "%")
  })
})
#----------------------------------------------------------------------
output$postprobwOutput <- renderText({ # change JR, 201403112
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    if (is.null(RunMCMCAndGetResults())) return(NULL)
    RunMCMCAndGetResults()
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 60)
    if (is.null(input$indicatorpw) | is.null(input$yearpw) | is.null(input$booleanpw) |
          is.null(input$nwomenInput) | is.null(input$selectwomenpw)) return(NULL)
    if (input$selectwomenpw == "total") {
      cutoffwomen <- input$nwomenInput
      cutoffwomenadd <- year.current <- NULL
    } else {
      cutoffwomen <- NULL
      cutoffwomenadd <- input$nwomenInput
      year.current <- input$yearstartpw+0.5
    }
    run.name <- getRunName()
    postprob <- GetPosteriorProbFromCutoffProp(run.name = run.name,
                                               iso.select = gsub(" ", "", input$isoselect),
                                               indicator = input$indicatorpw,
                                               year = input$yearpw+0.5,
                                               cutoffwomen = cutoffwomen,
                                               cutoffwomenadd = cutoffwomenadd, 
                                               year.current = year.current,
                                               sign = input$booleanpw)
    paste0(round(postprob*100), "%")
  })
})
#----------------------------------------------------------------------
output$nwomenOutput <- renderText({ # change JR, 201403112
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    if (is.null(RunMCMCAndGetResults())) return(NULL)
    RunMCMCAndGetResults()
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 60)
    if (is.null(input$indicatorppw) | is.null(input$yearppw) | is.null(input$booleanppw) |
          is.null(input$postprobwInput) | is.null(input$selectwomenppw)) return(NULL)
    if (input$selectwomenppw == "total") {
      year.current <- NULL
    } else if (input$selectwomenppw == "add") {
      if (is.null(input$yearstartppw)) {
        return(NULL)
      } else {
        year.current <- input$yearstartppw+0.5
      }
    }
    run.name <- getRunName()
    res <- GetPropFromPosteriorProb(run.name = run.name,
                                    iso.select = gsub(" ", "", input$isoselect),
                                    indicator = input$indicatorppw,
                                    year = input$yearppw+0.5,
                                    postprob = input$postprobwInput/100,
                                    year.current = year.current,
                                    sign = input$booleanppw)
    if (input$selectwomenppw == "total") {
      paste0(round(res$women))
    } else {
      paste0(round(res$women.add))
    }
  })
})
#----------------------------------------------------------------------
output$targetPanel1 <- renderUI ({
  wellPanel(
    p("Q: What is the probability that "),
    selectInput("indicatorp", "",
                choices = indicator.labels, 
                selected = "Total", selectize = FALSE),
    br(" in the year "),
    numericInput("yearp", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" is "),
    selectInput("booleanp", "",
                choices = c("above" = ">", 
                            # "at least" = ">=", 
                            "below" = "<"
                            #, "at most" = "<="
                ), selected = ">", selectize = FALSE),
    numericInput("percInput", "", 50,
                 min = 0, max = 100, step = 1),
    br("%?"),
    br("A:"),
    strong(textOutput("postprobOutput"))
  )
})
#----------------------------------------------------------------------
output$targetPanel2 <- renderUI ({
  wellPanel(
    p("Q: What target value of "),
    selectInput("indicatorpp", "",
                choices = indicator.labels, 
                selected = "Total", selectize = FALSE),
    br(" in the year "),
    numericInput("yearpp", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" corresponds to an attainment probability of "),
    numericInput("postprobInput", "", 90,
                 min = 0, max = 100, step = 1),
    br("%, where attainment probability refers to the probability of an outcome "),
    selectInput("booleanpp", "",
                choices = c(
                  "greater" = ">", 
                  # "at least" = ">=", 
                  "smaller" = "<" 
                  # , "at most" = "<="
                ), selected = ">", selectize = FALSE),
    br(" than the target value."),
    br("A: "),
    strong(textOutput("percOutput"))
  )
})
#----------------------------------------------------------------------
output$targetPanel3 <- renderUI ({
  wellPanel(
    p("Q: What is the probability that the "),
    selectInput("selectwomenpw", "",
                choices = c("total" = "total", 
                            "additional" = "add"
                ), selected = "total", selectize = FALSE),
    br(" number of women "),
    conditionalPanel(
      condition = "input.selectwomenpw == 'add'",
      br("(relative to the year "),
      numericInput("yearstartpw", "", 2013, 
                   min = 1990, max = 2020, step = 1),
      br(")")),
    selectInput("indicatorpw", "",
                choices = head(indicator.count.labels, length(indicator.count.labels)-2), 
                selected = "Total", selectize = FALSE),
    br(" in the year "),
    numericInput("yearpw", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" is "),
    selectInput("booleanpw", "",
                choices = c("above" = ">", 
                            # "at least" = ">=", 
                            "below" = "<"
                            #, "at most" = "<="
                ), selected = ">", selectize = FALSE),
    numericInput("nwomenInput", "", 1000,
                 min = 0, max = NA, step = 10),
    br("A:"),
    strong(textOutput("postprobwOutput"))
  )
})
#----------------------------------------------------------------------
output$targetPanel4 <- renderUI ({
  wellPanel(
    p("Q: What target "),
    selectInput("selectwomenppw", "",
                choices = c("total" = "total", 
                            "additional" = "add"
                ), selected = "total", selectize = FALSE),
    br(" number of women "),
    conditionalPanel(
      condition = "input.selectwomenppw == 'add'",
      br("(relative to the year "),
      numericInput("yearstartppw", "", 2013, 
                   min = 1990, max = 2020, step = 1),
      br(")")),
    selectInput("indicatorppw", "",
                choices = head(indicator.count.labels, length(indicator.count.labels)-2), 
                selected = "Total", selectize = FALSE),
    br(" in the year "),
    numericInput("yearppw", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" corresponds to an attainment probability of "),
    numericInput("postprobwInput", "", 90,
                 min = 0, max = 100, step = 1),
    br("%, where attainment probability refers to the probability of an outcome "),
    selectInput("booleanppw", "",
                choices = c(
                  "greater" = ">", 
                  # "at least" = ">=", 
                  "smaller" = "<" 
                  # , "at most" = "<="
                ), selected = ">", selectize = FALSE),
    br(" than the target value."),
    br("A: "),
    strong(textOutput("nwomenOutput"))
  )
})
#----------------------------------------------------------------------
output$changeOutput <- renderText({
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 40)
    if (is.null(input$indicatorc) | is.null(input$year1c) | is.null(input$year2c))
      return(NULL)
    change <- GetChangeFromYear1ToYear2(run.name = getRunName(),
                                        iso.select = getUNCode(run.name = getRunName(),
                                                        iso = gsub(" ", "", input$isoselect)), 
                                        indicator = input$indicatorc, 
                                        year1 = input$year1c+0.5, year2 = input$year2c+0.5)
    P.uis <- round(quantile(change$P.s, probs = percentiles.for.change)*100, digits = 1)
    paste0(P.uis[2], "% (", P.uis[1], "%, ", P.uis[3], "%)")
  })
})
#----------------------------------------------------------------------
output$changewOutput <- renderText({ # change JR, 20140623
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 40)
    if (is.null(input$indicatorcw) | is.null(input$year1cw) | is.null(input$year2cw))
      return(NULL)
    change <- GetChangeFromYear1ToYear2(run.name = getRunName(),
                                        iso.select = getUNCode(run.name = getRunName(),
                                                        iso = gsub(" ", "", input$isoselect)), 
                                        indicator = input$indicatorcw, 
                                        year1 = input$year1cw+0.5, year2 = input$year2cw+0.5)
    WP.uis <- round(quantile(change$WP.s, probs = percentiles.for.change)*1000, digits = 0)
    paste0(WP.uis[2], " (", WP.uis[1], ", ", WP.uis[3], ")")
  })
})
#----------------------------------------------------------------------
output$progressPanel1 <- renderUI ({
  wellPanel(
    p("The change in "),
    selectInput("indicatorc", "",
                choices = indicator.labels, 
                selected = "Total", selectize = FALSE),
    br(" from the year "),
    numericInput("year1c", "", 2013, 
                 min = 1990, max = 2020, step = 1),
    br(" to the year "),
    numericInput("year2c", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" is "),
    strong(textOutput("changeOutput"))
  )
})
#----------------------------------------------------------------------
output$progressPanel2 <- renderUI ({
  wellPanel(
    p("The change in the number of women"),
    selectInput("indicatorcw", "",
                choices = head(indicator.count.labels, length(indicator.count.labels)-2), 
                selected = "Total", selectize = FALSE),
    br(" from the year "),
    numericInput("year1cw", "", 2013, 
                 min = 1990, max = 2020, step = 1),
    br(" to the year "),
    numericInput("year2cw", "", 2020, 
                 min = 1990, max = 2020, step = 1),
    br(" is "),
    strong(textOutput("changewOutput"))
  )
})
#----------------------------------------------------------------------
# UIs
output$resultsViewChart <- renderUI({
  if (input$startRun == 0) return(NULL)
  if (is.null(RunMCMCAndGetResults())) return(NULL)
  div(
    p(strong("Results options")),
    fluidRow(
      div(class = "span3",
          selectInput("resultsType", "Result to display",
                      choices = c("Percentage" = "perc", 
                                  "Count (in '000s)" = "count"),
                      selected = "perc", selectize = FALSE)),
      div(class = "span3",
          selectInput("resultsIndicator", "Indicator to display",
                      choices = indicator.labels,
                      selected = "Total", selectize = FALSE)),
      div(class = "span6", align = "right",
          downloadButton("downloadEstimates", "Download results"))
    ),
    p(),
    dataTableOutput("resultsView")
  )
})
#----------------------------------------------------------------------
output$resultsPlotChart <- renderUI({
  if (input$startRun == 0) return(NULL)
  if (is.null(RunMCMCAndGetResults())) return(NULL)
  div(
    p(strong("Graph options")),
    fluidRow(
      # change JR, 20140411
      div(class = "span3",
          selectInput("plotType", "Result to display",
                      choices = c("Percentage" = "perc", 
                                  "Count (in '000s)" = "count"),
                      selected = "perc", selectize = FALSE)
      ),
      div(class = "span9", align = "right",
          downloadButton("downloadPlot", "Download graph"))
    ),
    fluidRow(uiOutput("selectPlotCategories")),
    plotOutput("resultsPlot", width = "1050px", height = "550px")
  )
})
#----------------------------------------------------------------------
output$targetPanelAll <- renderUI ({
  if (input$startRun == 0) return(NULL)
  if (is.null(RunMCMCAndGetResults())) return(NULL)
  div(
    h4("Information for target-setting"),
    p("Select entries below to find a probability associated with a given level of an indicator of interest and vice versa."),
    fluidRow(
      div(class = "span4", uiOutput("targetPanel1")),
      div(class = "span4", uiOutput("targetPanel2"))
    ),
    fluidRow(
      div(class = "span4", uiOutput("targetPanel3")),
      div(class = "span4", uiOutput("targetPanel4"))
    ))
})
#----------------------------------------------------------------------
output$progressPanelAll <- renderUI({
  if (input$startRun == 0) return(NULL)
  if (is.null(RunMCMCAndGetResults())) return(NULL)
  div(
    h4("Information for measuring progress"),
    fluidRow(
      div(class = "span4", uiOutput("progressPanel1")), 
      div(class = "span4", uiOutput("progressPanel2"))
    )
  )
})