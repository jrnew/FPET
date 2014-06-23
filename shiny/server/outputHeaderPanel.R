#----------------------------------------------------------------------
# outputHeaderPanel.R
# Outputs for header panel
# Jin Rou New, 2013
#----------------------------------------------------------------------
output$progressBar <- renderUI({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
  })
})
  
output$headerPanel <- renderUI({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 0)
    if (input$chooseAction == "nil") {
      h5("Please choose an action.")
    } else if (input$chooseAction == "newrun") {
      h5(textOutput("runnameNew"))
    } else if (input$chooseAction == "viewrun") {
      h5(textOutput("runnameExisting"))
    } else if (input$chooseAction == "compareruns") {
      h5(textOutput("runnamesCompare"))
    }
  })
})

output$runnameNew <- renderText({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 20)
    if (is.null(input$isoselect)) {
      paste("Please select country.")
    } else if (input$isoselect == "???") {
      paste("Please select country/population.")
    } else {
      paste0("Run name: ", getRunName())
    }
  })
})

output$runnameExisting <- renderText({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 20)
    if (is.null(input$runnameExisting)) {
      paste("Please select run.")
    } else if (input$runnameExisting == "NULL") {
      paste("Please select run.")
    } else if (input$isoselectview == "???") {
      paste0("Runname: ", ifelse(input$runnameExisting == getRunnameUNPD()$run.name,
                                 getRunnameUNPD()$name, input$runnameExisting), 
             " - Please select country/population.")
    } else {
      paste0("Runname: ", ifelse(input$runnameExisting == getRunnameUNPD()$run.name,
                                 getRunnameUNPD()$name, input$runnameExisting),
             " - ", input$isoselectview)
    }
  })
})

output$runnamesCompare <- renderText({
  withProgress(session, min=0, max=100, expr={
    setProgress(message = 'Loading', detail = 'Please wait...',
                value = 20)
    if (is.null(input$runnameCompare1) | is.null(input$runnameCompare2)) {
      paste("Please select runs.")
    } else if (input$runnameCompare1 == "NULL" | input$runnameCompare2 == "NULL") {
      paste("Please select runs.")
    } else if (input$isoselectcompare == "???") {
      paste0("Compare: ", 
             ifelse(input$runnameCompare1 == getRunnameUNPD()$run.name,
                    getRunnameUNPD()$name, input$runnameCompare1),
             " vs ", 
             ifelse(input$runnameCompare2 == getRunnameUNPD()$run.name,
                    getRunnameUNPD()$name, input$runnameCompare2),
             " - Please select country.")
    } else {
      paste0("Compare: ", 
             ifelse(input$runnameCompare1 == getRunnameUNPD()$run.name,
                    getRunnameUNPD()$name, input$runnameCompare1),
             " vs ", 
             ifelse(input$runnameCompare2 == getRunnameUNPD()$run.name,
                    getRunnameUNPD()$name, input$runnameCompare2),
             " - ", input$isoselectcompare)
    }
  })
})
