#----------------------------------------------------------------------
# sidebarPanelNew.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
sidebarPanel(
  width = 3,
  #======================================================================
  # Do a new run
  #======================================================================
  conditionalPanel(
    condition = "input.chooseAction == 'newrun'",
    wellPanel(
      radioButtons("chooseDatabase", strong("1A. Select database."),
                   c("Default" = "default", "Other" = "other")),
      conditionalPanel(condition = "input.chooseDatabase == 'other'",
                       fileInput("datafile", "Upload file:", multiple = FALSE,
                                 accept = c("text/csv", "text/comma-separated-values")),
                       br("Note: Your data needs to be in the same format as the ",
                          a("sample data.", href = "sampledata.csv", target = "_blank"), 
                          "Refer to the",
                          a("UNPD database", href = "http://www.un.org/esa/population/publications/WCU2012/MainFrame.html", target = "_blank"),
                           "for more details."))
    ),
    #----------------------------------------------------------------------
    wellPanel( # change JR, 20140401
      radioButtons("chooseMWRADatabase", strong("1B. Select MWRA database."),
                   c("Default" = "default", "Other" = "other")),
      conditionalPanel(condition = "input.chooseMWRADatabase == 'other'",
                       fileInput("MWRAfile", "Upload file:", multiple = FALSE,
                                 accept = c("text/csv", "text/comma-separated-values")),
                       br("Note: Your data needs to be in the same format as the ",
                          a("sample MWRA data.", href = "sampleMWRAdata.csv", target = "_blank")))
    ),
    #----------------------------------------------------------------------
    # Select country/population
    wellPanel(
      p(strong("2. Select country/population and view data.")),
      uiOutput("selectISO"),  
      uiOutput("selectISOCountry") # change JR, 20140409
    ),
    #----------------------------------------------------------------------
    # User-inputed settings
    wellPanel(
      p(strong("3. Input overall run settings.")),
      fluidRow(div(class = "span4", textInput("runname", "Run name:", "test")),
               div(class = "span8", shinyalert("duplicateRunAlert", click.hide = FALSE, auto.close.after = 5))), # change JR, 20140418
      checkboxInput("shortRun", "Do short run", FALSE),
      sliderInput(inputId = "estyears", label = "Estimate years:",
                  min = 1970, max = 2035, step = 1, 
                  value = c(1990, 2020),
                  # format = "####"
                  sep = ""
                  ),
      fluidRow(div(class = "span3", actionButton("startRun", "Start run!", styleclass = "primary")), # change JR, 20140418
               div(class = "span8", shinyalert("startRunAlert", click.hide = FALSE, auto.close.after = 5))) # change JR, 20140418
    )
  ) # end conditionalPanel for newrun
)
