#----------------------------------------------------------------------
# mainPanelCompare.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
mainPanel(
  width = 9,
  # progressInit(),
  uiOutput("headerPanelCompare"),
  #======================================================================
  # Compare runs
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'compareruns'",
    tabsetPanel(id = "tabsetPanelCompare",
      tabPanel("Results",
               uiOutput("resultsViewComparisonChart")),
      tabPanel("Graph",
               uiOutput("resultsPlotComparisonPlot"))
    )
  ) # end conditionalPanel for compareruns
) # end mainPanel
