#----------------------------------------------------------------------
# mainPanel.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
tabPanelAbout <- source("about.R")$value

mainPanel(
  width = 9,
  progressInit(),
  uiOutput("headerPanel"),
  #======================================================================
  # No action
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'nil'",
    tabsetPanel(
      tabPanelAbout())
  ), # end conditionalPanel for nil
  #======================================================================
  # Start a new run
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'newrun'",
    tabsetPanel(
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
      tabPanelAbout()
    )
  ), # end conditionalPanel for newrun
  #======================================================================
  # View an existing run
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'viewrun'",
    tabsetPanel(
      tabPanel("Data",
               uiOutput("countryDataExistingChart")),
      tabPanel("Data (MWRA)",
               uiOutput("countryMWRADataExistingChart")),
      tabPanel("Results",
               uiOutput("resultsViewExistingChart")),
      tabPanel("Graph",
               uiOutput("resultsPlotExistingChart")),
      tabPanel("Targets",
               uiOutput("targetPanelExistingAll")),
      tabPanelAbout()
    )
  ), # end conditionalPanel for viewrun
  #======================================================================
  # Compare runs
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'compareruns'",
    tabsetPanel(
      tabPanel("Results",
               uiOutput("resultsViewComparisonChart")),
      tabPanel("Graph",
               uiOutput("resultsPlotComparisonPlot")),
      tabPanelAbout()
    )
  ) # end conditionalPanel for compareruns
) # end mainPanel
