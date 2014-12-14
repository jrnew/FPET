#----------------------------------------------------------------------
# mainPanelView.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
mainPanel(
  width = 9,
  progressInit(),
  uiOutput("headerPanelView"),
  #======================================================================
  # View an existing run
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'viewrun'",
    tabsetPanel(id = "tabsetPanelView",
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
      tabPanel("Progress",
               uiOutput("progressPanelExistingAll"))
    )
  ) # end conditionalPanel for viewrun
) # end mainPanel
