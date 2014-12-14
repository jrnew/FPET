#----------------------------------------------------------------------
# ui.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
tabPanelAbout <- source("about.R")$value
shinyUI(navbarPage(
  title = "Family Planning Estimation Tool",
  windowTitle = "Family Planning Estimation Tool",
  id = "chooseAction",
  header = tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css")),
  tabPanelAbout(),
  tabPanel(title = "View run", value = "viewrun",
           sidebarLayout(
             source("shiny/sidebarPanelView.R", local = T)$value,
             source("shiny/mainPanelView.R", local = T)$value)),
  tabPanel(title = "Start run", value = "newrun",
           sidebarLayout(
             source("shiny/sidebarPanelNew.R", local = T)$value,
             source("shiny/mainPanelNew.R", local = T)$value)),
  tabPanel(title = "Compare runs", value = "compareruns",
           sidebarLayout(
             source("shiny/sidebarPanelCompare.R", local = T)$value,
             source("shiny/mainPanelCompare.R", local = T)$value))
))
