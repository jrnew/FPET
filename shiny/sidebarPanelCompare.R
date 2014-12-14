#----------------------------------------------------------------------
# sidebarPanelCompare.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
sidebarPanel(
  width = 3,
  #======================================================================
  # Compare runs
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'compareruns'",
    # Select run
    wellPanel(
      p(strong("1. Select runs to compare.")),
      uiOutput("selectRunnameCompare1"),
      uiOutput("selectRunnameCompare2")
    ),
    # Select country to compare
    wellPanel(
      p(strong("2. Select country/population to compare runs for.")),
      uiOutput("selectISOCompare")
    )
  ) # end conditionalPanel for compareruns
)
