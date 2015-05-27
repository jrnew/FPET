#----------------------------------------------------------------------
# about.R
# Jin Rou New, 2013-2014
#----------------------------------------------------------------------
function() {
  tabPanel(title = "About", value = "nil",
           p("The ", strong("Family Planning Estimation Tool"), " (FPET) is a web application ",
             "that is a country-specific implementation of the estimation approach for ",
             "contraceptive prevalence and unmet need for family planning used by the ",
             "United Nations Population Division (UNPD).",
             "Refer to UNPD's ",
             a("World Contraceptive Use 2014", 
               href = "http://www.un.org/en/development/desa/population/publications/dataset/contraception/wcu2014.shtml", target = "_blank"),
             " for the most recent database and estimates."),
           p("Instructions for use of FPET are available at: ",
             a("http://www.track20.org/pages/resources/all-resources/track20_tools.", href = "http://www.track20.org/pages/resources/all-resources/track20_tools", target = "_blank")),           
           p("The global implementation of the estimation approach is described in:",
             "Alkema, L. et al. (2013). National, regional and global rates and trends in contraceptive prevalence and unmet need for family planning between 1990 and 2015: a systematic and comprehensive analysis. Lancet, 2013, 381(9878):1642-1652. Available at ",
             a("http://www.thelancet.com/journals/lancet/article/PIIS0140-6736(12)62204-1/abstract.", href = "http://www.thelancet.com/journals/lancet/article/PIIS0140-6736(12)62204-1/abstract", target = "_blank")),
           p("This app was created by Jin Rou New and Leontine Alkema (Department of Statistics & Applied Probability, National University of Singapore).",
             "For any questions, feedback or suggestions, please contact JR New at jrnew[at]nus.edu.sg."),
           p("Please use the latest version of Google Chrome, Mozilla Firefox or Internet Explorer to run FPET; there may be problems with other browsers/versions."),
           p(em("Suggested citation: New, JR and Alkema, L (2014). Family Planning Estimation Tool (FPET). Available at http://fpet.track20.org/")),
           br(), br(), 
           p("Last updated: 22 Apr 2015",
             br("Created in ",
                a("R", 
                  href = "http://www.r-project.org/", target = "_blank"),
                "| Powered by ",
                a("Shiny", 
                  href = "http://www.rstudio.com/shiny/", target = "_blank")),
             align = "center")
  )
}
