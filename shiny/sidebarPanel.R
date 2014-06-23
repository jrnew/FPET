#----------------------------------------------------------------------
# sidebarPanel.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
sidebarPanel(
  width = 3,
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css")
  ),
  # Google analytics
  HTML("<script>",
       "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){",
       "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),",
       "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)",
       "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');",  
       "ga('create', 'UA-42087579-2', 'shinyapps.io');",
       "ga('send', 'pageview');",
       "</script>"
  ),
  helpText("Estimate family planning indicators for any country."),
  wellPanel(
    radioButtons("chooseAction", strong("Choose action:"),
                 choices = c("None" = "nil",
                             "View an existing run" = "viewrun",
                             "Start a new run" = "newrun",
                             "Compare runs" = "compareruns"))
  ),
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
      checkboxInput("shortRun", "Do short run", TRUE),
      fluidRow(div(class = "span3", actionButton("startRun", "Start run!", styleclass = "primary")), # change JR, 20140418
               div(class = "span8", shinyalert("startRunAlert", click.hide = FALSE, auto.close.after = 5))) # change JR, 20140418
    )
  ), # end conditionalPanel for newrun
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
  )
  , # end conditionalPanel for viewrun
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
