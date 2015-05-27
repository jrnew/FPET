#----------------------------------------------------------------------
# main_shinyapp.R
#----------------------------------------------------------------------
# 1. Install required software.
#    a. Install R: http://www.r-project.org/
#    b. Install JAGS: https://sourceforge.net/projects/mcmc-jags/files/ (Download JAGS-3.4.0.exe (31.7 MB))
#    c. Install Rtools: http://cran.r-project.org/bin/windows/Rtools/ (Skip this step for Mac OS)
#    d. Download and unzip this file: https://dl.dropboxusercontent.com/u/38155136/FPET.zip 
#    e. Open up R, and run this script: https://dl.dropboxusercontent.com/u/38155136/FPET.R 

# (Run the following lines if this is the first time you are using FPET.)
# pkgs <- c("shiny", "devtools", "MCMCpack", "doParallel", "rjags", "R2jags", 
#           "lattice", "abind", "msm", "proto", 
#           "plyr", "ggplot2", "reshape2", "foreach")
# install.packages(pkgs, repos = "http://cran.r-project.org")
# sapply(pkgs, library, character.only = TRUE)
# devtools::install_github("shiny-incubator", "rstudio")
# devtools::install_github("ShinySky", "AnalytixWare")

# 3. Run code below. App will open in browser.
rm(list = ls())
library(shiny)
work.dir <- "~/Dropbox/CP1countryruns/FPET"
setwd(work.dir)
runApp(getwd(), port = 1234)

# TO DOS!!!
# Fix wonky sizes of all spans
# https://groups.google.com/forum/#!topic/shiny-discuss/3m7cq96mcBY
# Fix accented characters for say Enquete, Fecondite
# Allow run to happen with unused SS data and display SS data on plot.
#----------------------------------------------------------------------
# devtools::install_github('rstudio/shinyapps')
library(shinyapps)
library(RPushbullet)
shinyapps::setAccountInfo(name="jrnew", token="A45DC21747C8D970327E9FE8CCABB461", 
                          secret="Sufbe3CH7XxvbMf1qHvrocVvlgAnrn+Wi4CbVUMF")
# sessionInfo()
# options(shinyapps.http.trace = TRUE) # for log to trace error
deployApp()
Y
pbPost(type = "note", 
       title = "Hoorah!", 
       body = "FPET app deployed successfully!",
       deviceind = 1)

# shinyapps::configureApp("FPET", size="xxlarge") # change instance type of app
# debug
# shinyapps::appDependencies()
# packageDescription('shinyIncubator')
#----------------------------------------------------------------------
# END
#----------------------------------------------------------------------
# Data tables
# https://github.com/ThomasSiegmund/D3TableFilter
# https://github.com/jrowen/rhandsontable

# Interactive visualisations
# http://hafen.github.io/rbokeh/

# runApp("C:/Program Files/SkyScorer/Apps/SkyScorer/Shiny")
# ShinySky
# require(shinysky)
# shinysky::run.shinysky.example()
# Examples for reference:
# - http://spark.rstudio.com/uafsnap/temp_wind_events/

# Developer's notes:
# - Comment out sink() and closeAllConnections() in runmcmc.R
#----------------------------------------------------------------------
#https://groups.google.com/forum/#!msg/shiny-discuss/l8ZcauoOtuw/0pYyU5slVgsJ

# Idea: Combine with gVis package for more visualisation awesomeness!
# gVis package has functionality of selecting/deselecting data points,
# but displaying data series might not be possible?