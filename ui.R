#----------------------------------------------------------------------
# ui.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
shinyUI(pageWithSidebar(
  source("shiny/headerPanel.R", local = T)$value,
  source("shiny/sidebarPanel.R", local = T)$value,
  source("shiny/mainPanel.R", local = T)$value
))
