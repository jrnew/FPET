#----------------------------------------------------------------------
# mainPanelNew.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
mainPanel(
  width = 9,
  # progressInit(),
  uiOutput("headerPanelNew"),
  #======================================================================
  # Start a new run
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'newrun'",
    tabsetPanel(id = "tabsetPanelNew",
      tabPanel("Data",
               uiOutput("countryDataChart")),  
      tabPanel("Data (MWRA)",
               uiOutput("countryMWRADataChart")), 
      tabPanel("Log", 
               verbatimTextOutput("log")),
      tabPanel("Results",
               uiOutput("resultsViewChart")),
      tabPanel("Graph", 
               uiOutput("resultsPlotChart")),
      tabPanel("Targets",
               uiOutput("targetPanelAll")),
      tabPanel("Progress",
               uiOutput("progressPanelAll"))
    )
  ) # end conditionalPanel for newrun
) # end mainPanel
