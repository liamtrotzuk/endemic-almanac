################################################################################
# module calendar_pre
#
# Author: Liam Trotzuk
# Created: 2020-10-20
################################################################################

# Module UI ---------------------------------------------------------------

calendar_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("module_ui"))
}

# Module server logic -----------------------------------------------------

calendar_error <- function(input,output,session) {
  
  error_msg <- reactive("please enter a valid 5-digit US zip code.")

  output$module_ui <- renderUI({
    
    req(error_msg())
    
    htmlTemplate(
      filename = "www/modules/calendar/error.html",
      error_msg_text = as.character(error_msg())
    )
  })
}