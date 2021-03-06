#----------------------------------------------------------------------
# outputCompareRuns.R
# Outputs and reactives for compare runs
# Jin Rou New, 2013
#----------------------------------------------------------------------
# Main reactive function for compareruns
getResultsComparison <- reactive({
  if (input$chooseAction != "compareruns") return(NULL)
  if (is.null(input$runnameCompare1) | is.null(input$runnameCompare2)) return(NULL)
  if (input$runnameCompare1 == "NULL" | input$runnameCompare2 == "NULL") return(NULL)
  if (input$isoselectcompare == "???") return(NULL) 
  run.name1 <- input$runnameCompare1
  run.name2 <- input$runnameCompare2
  iso.select <- input$isoselectcompare
  results <- outputResultsCompare(run.name1 = run.name1,
                                  run.name2 = run.name2,
                                  indicator = input$resultsIndicatorCompare,
                                  type.is.prop = ifelse(input$resultsTypeCompare == "perc", TRUE, FALSE), # change JR, 20140401
                                  iso.select = iso.select)
  return(results)
})
#----------------------------------------------------------------------
# Get run.names and ISO
output$selectRunnameCompare1 <- renderUI({
  if (input$chooseAction == "compareruns") {
    selectInput("runnameCompare1", "Run name 1:", choices = runsCompleted(), selectize = FALSE)
  }
})
output$selectRunnameCompare2 <- renderUI({
  if (input$chooseAction == "compareruns") {
    selectInput("runnameCompare2", "Run name 2:", choices = runsCompleted(), selectize = FALSE)
  }
})
output$selectISOCompare <- renderUI({
  if (input$chooseAction != "compareruns") return(NULL)
  selectInput("isoselectcompare", "Country/population:", choices = getISOs(), selectize = FALSE)
})
#----------------------------------------------------------------------
# To get chart categories # change JR, 20150527
output$selectChartCategoriesCompare <- renderUI({
  div(class = "span3",
      selectInput("resultsIndicatorCompare", "Indicator to display",
                  choices = getChartCategories(input$resultsTypeCompare), 
                  selected = "Total", selectize = FALSE))
})
#----------------------------------------------------------------------
# Display results data table
output$resultsViewComparison <- renderDataTable({
  withProgress(min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    results <- getResultsComparison()
    if (!is.null(results)) {
      setProgress(message = 'Loading', detail = 'Please wait...',
                  value = 60)
      results
    }
  })
}, options = list(lengthMenu = c(5, 10, 20, 50), 
                  searching = FALSE,
                  orderClasses = TRUE, 
                  pageLength = 10))
#----------------------------------------------------------------------
# To get plot categories
output$selectPlotCategoriesCompare <- renderUI({
  if (input$chooseAction != "compareruns") return(NULL)
  div(class = "span12",
      checkboxGroupInput("plotCategoriesCompare", "Display the following: ", 
                         choices = getPlotCategories(input$plotTypeCompare)$choices, 
                         selected = getPlotCategories(input$plotTypeCompare)$selected,
                         inline = TRUE), # change JR, 20150422
      tags$style(type="text/css", 
                 HTML(paste0("#", "plotCategoriesCompare", 
                             ">*{float: left; margin-right: 15px; height: 20px;} #", 
                             "plotCategoriesCompare", " {height: 20px;}"))))
})
#----------------------------------------------------------------------
# Show plot
output$resultsPlotComparison <- renderPlot({
  withProgress(min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    if (input$chooseAction != "compareruns") InternalPlotNull()
    if (is.null(input$runnameCompare1) | is.null(input$runnameCompare2)) InternalPlotNull()
    if (input$runnameCompare1 == "NULL" | input$runnameCompare2 == "NULL") InternalPlotNull()
    if (input$isoselectcompare == "???") InternalPlotNull()
    run.name1 <- input$runnameCompare1
    run.name2 <- input$runnameCompare2
    iso.select <- input$isoselectcompare
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 90)
    ShinyPlotComparison(run.name1 = run.name1,
                        run.name2 = run.name2,
                        iso.select = iso.select, 
                        plot.prop = ifelse(input$plotTypeCompare == "perc", TRUE, FALSE), # change JR, 20140411
                        add.info = "Show Data" %in% input$plotCategoriesCompare, # change JR, 20140409
                        categories.to.plot = input$plotCategoriesCompare[!(input$plotCategoriesCompare %in% "Show Data")],
                        cex.adj.factor = 0.7)
  })
})
#----------------------------------------------------------------------
# Make results available for download
output$downloadEstimatesComparison <- downloadHandler(
  filename <- function() {
    paste0("Results ", input$runnameCompare1, " vs ", input$runnameCompare2, 
           "_", input$isoselectcompare, "_", paste(input$resultsIndicatorCompare, "-"), 
           " ", Sys.Date(), ".csv")
  },
  content <- function(file) {
    results <- getResultsComparison()
    write.csv(results, file, row.names = F, na = "")
  }
)
#----------------------------------------------------------------------
# Make plot available for download
output$downloadPlotComparison <- downloadHandler(
  filename <- function() {
    paste0("Results Plot ", input$runnameCompare1, " vs ", input$runnameCompare2, 
           "_", input$isoselectcompare, " ", Sys.Date(), ".pdf")
  },
  content <- function(file) {
    pdf(file, width = 21, height = 18)
    run.name1 <- input$runnameCompare1
    run.name2 <- input$runnameCompare2
    iso.select <- input$isoselectcompare
    ShinyPlotComparison(run.name1 = run.name1,
                        run.name2 = run.name2,
                        iso.select = iso.select,
                        plot.prop = ifelse(input$plotTypeCompare == "perc", TRUE, FALSE), # change JR, 20140411
                        add.info = "Show Data" %in% input$plotCategoriesCompare, # change JR, 20140409
                        categories.to.plot = input$plotCategoriesCompare[!(input$plotCategoriesCompare %in% "Show Data")])
    dev.off()
  }
)
#----------------------------------------------------------------------
# UIs
output$resultsViewComparisonChart <- renderUI({
  if (input$chooseAction != "compareruns") return(NULL)
  if (is.null(input$runnameCompare1) | is.null(input$runnameCompare2)) return(NULL)
  if (input$runnameCompare1 == "NULL" | input$runnameCompare2 == "NULL") return(NULL)
  if (input$isoselectcompare == "???") return(NULL)
  div(p(strong("Results options")),
      fluidRow(
        # change JR, 20140401
        div(class = "span3",
            selectInput("resultsTypeCompare", "Result to display",
                        choices = c("Percentage" = "perc", 
                                    "Count (in '000s)" = "count"),
                        selected = "perc", selectize = FALSE)),
        uiOutput("selectChartCategoriesCompare"), # change JR, 21050527
        div(class = "span6", align = "right",
            downloadButton("downloadEstimatesComparison", "Download results"))
      ),
      fluidRow(p()),
      dataTableOutput("resultsViewComparison")
  )
})
output$resultsPlotComparisonPlot <- renderUI({
  if (input$chooseAction != "compareruns") return(NULL)
  if (is.null(input$runnameCompare1) | is.null(input$runnameCompare2)) return(NULL)
  if (input$runnameCompare1 == "NULL" | input$runnameCompare2 == "NULL") return(NULL)
  if (input$isoselectcompare == "???") return(NULL) 
  div(p(strong("Graph options")),
      fluidRow(
        # change JR, 20140411
        div(class = "span3",
            selectInput("plotTypeCompare", "Result to display",
                        choices = c("Percentage" = "perc", 
                                    "Count (in '000s)" = "count"),
                        selected = "perc", selectize = FALSE)
        ),
        div(class = "span9", align = "right",
            downloadButton("downloadPlotComparison", "Download graph"))
      ),
      fluidRow(uiOutput("selectPlotCategoriesCompare")),
      fluidRow(p()),
      plotOutput("resultsPlotComparison", width = "1050px", height = "850px"))
})
