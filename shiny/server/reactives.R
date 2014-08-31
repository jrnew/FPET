#----------------------------------------------------------------------
# reactives.R
# Reactive functions
# Jin Rou New, 2014
#----------------------------------------------------------------------
# To get runname
getRunName <- reactive({
  runname <- paste0(input$runname, "_", 
                    ifelse(newPopulation(), paste0(input$isocountryselect, "_"), ""), # change JR, 20140409
                    input$isoselect,
                    ifelse(input$shortRun, "_shortRun", ""))
  return(runname)
})
#----------------------------------------------------------------------
# To get runnames of stored runs
runsCompleted <- reactive({
  runs.completed <- list.files("output")
  runs.completed <- runs.completed[!is.element(runs.completed, 
                                               c(getRunnameUNPD()$run.name))]
  runs.completed <- c("NULL", runs.completed)
  # append UNPD runname
  runs.completed <- append(runs.completed, getRunnameUNPD()$run.name, 1)
  names(runs.completed) <- c("Please select run", runs.completed[-1])
  # indicate run corresponding to UNPD run
  names(runs.completed)[names(runs.completed) == getRunnameUNPD()$run.name] <- 
    getRunnameUNPD()$name
  return(runs.completed)
})
#----------------------------------------------------------------------
# To get ISOs for input selection
getISOs <- reactive({
  if (input$chooseAction == "newrun") {
    if (input$chooseDatabase == "default") { 
      load("data/iso.all.rda"); iso.c <- iso.all
      load("data/name.all.rda"); name.c <- name.all
    } else if (input$chooseDatabase == "other") {
      if (is.null(input$datafile)) {
        iso.c <- name.c <- NULL
      } else {
        # read in from uploaded database
        data <- read.csv(file = getFilePath(), header = T, stringsAsFactors = F)
        names(data)[grepl("Country.letter.code|Country..letter.code", names(data))] <- "Country..letter.code" # change JR, 20140331
        # change JR, 20140409
        country.table <- unique(data.frame(iso = gsub(" ", "", as.character(data$Country..letter.code)),
                                           name = as.character(ifelse(is.na(data$New.population) | data$New.population == "", 
                                                                      data$Country, data$New.population)), 
                                           stringsAsFactors = F))
        iso.c <- country.table$iso
        name.c <- country.table$name 
      }
    } else {
      iso.c <- name.c <- NULL
    }
  } else if (input$chooseAction == "viewrun" & !is.null(input$runnameExisting)) {
    if (input$runnameExisting != "NULL") {      
      load(file.path("output", input$runnameExisting, "mcmc.meta.rda"))
      iso.c <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
      name.c <- mcmc.meta$data.raw$country.info$name.c
      rm(mcmc.meta)
    } else {
      iso.c <- name.c <- NULL
    }
  } else if (input$chooseAction == "compareruns" & 
               !is.null(input$runnameCompare1) & !is.null(input$runnameCompare2)) {
    if (input$runnameCompare1 != "NULL" & input$runnameCompare2 != "NULL") {    
      load(file.path("output", input$runnameCompare1, "mcmc.meta.rda"))
      iso.c1 <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
      names(iso.c1) <- mcmc.meta$data.raw$country.info$name.c
      rm(mcmc.meta)
      
      load(file.path("output", input$runnameCompare2, "mcmc.meta.rda"))
      iso.c2 <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
      names(iso.c2) <- mcmc.meta$data.raw$country.info$name.c
      rm(mcmc.meta)

      iso.c <- intersect(iso.c1, iso.c2)
      name.c <- intersect(names(iso.c1), names(iso.c2))
    } else {
      iso.c <- name.c <- NULL
    }
  } else {
    iso.c <- name.c <- NULL
  }
  if (!is.null(name.c)) {
    order <- order(name.c)
    iso.c <- iso.c[order]
    name.c <- name.c[order]
  }  
  iso.c <- c("???", iso.c)
  names(iso.c) <- c("Please select country/population", name.c)
  return(iso.c)
})
#----------------------------------------------------------------------
getRegions <- reactive({
  regions <- NULL
  if (input$chooseAction == "viewrun" & !is.null(input$runnameExisting)) {
    if (input$runnameExisting == getRunnameUNPD()$run.name) {
      regions <- c("World", "FP2020 countries",
                   "Developed countries", "Developing countries", "Developing (excl. China)",
                   "Africa", "Asia", "Europe", 
                   "Latin America and the Caribbean", "Oceania",                 
                   "Eastern Africa", "Middle Africa", "Northern Africa", 
                   "Southern Africa", "Western Africa", 
                   "Central Asia", "Eastern Asia", "South-Eastern Asia",
                   "Southern Asia", "Western Asia",
                   "Eastern Europe", "Northern Europe",
                   "Southern Europe", "Western Europe",
                   "Central America", "Northern America", "South America", "Caribbean", 
                   "Australia and New Zealand",                   
                   "Mela-Micro-Polynesia", 
                   "Melanesia", "Micronesia", "Polynesia")
      names(regions) <- regions
    } 
  }
  return(regions)
})
#----------------------------------------------------------------------
# To check if runname is a duplicate # change JR, 20140418
duplicateRun <- reactive({
  if (input$chooseAction != "newrun") return(NULL)
  if (input$startRun == 0) return(NULL)
  if (is.null(input$isoselect)) return(NULL)
  if (input$isoselect == "???") return(NULL)
  return(getRunName() %in% runsCompleted())
})
#----------------------------------------------------------------------
# To check if data is for new population # change JR, 20140409
newPopulation <- reactive({
  if (input$chooseAction != "newrun") return(FALSE)
  if (input$chooseDatabase == "default") return(FALSE)
  if (is.null(input$isoselect)) return(FALSE)
  if (input$isoselect == "???") return(FALSE)
  iso.select <- gsub(" ", "", input$isoselect)
  run.name.global <- getRunnameUNPD()$run.name
  load(file.path("output", run.name.global, "data.global.rda"))
  return(ifelse(iso.select %in% data.global$iso.c, FALSE, TRUE))
})