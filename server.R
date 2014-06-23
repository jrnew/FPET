#----------------------------------------------------------------------
# server.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
options(shiny.maxRequestSize = -1) # no limit on data upload size
shinyServer(function(input, output, session) {
  source("shiny/serverMain.R", local = T)  
})
