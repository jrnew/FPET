#----------------------------------------------------------------------
# sidebarPanelView.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
sidebarPanel(
  width = 3,
  #======================================================================
  # View an existing run
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'viewrun'",
    # Select run
    wellPanel(
      p(strong("1. Select existing run.")),
      uiOutput("selectRunnameExisting")
    ),
    # Select country to view
    wellPanel(
      p(strong("2. Select country/population and view data and results.")),
      uiOutput("selectISOView")
    )
  ) # end conditionalPanel for viewrun
)

