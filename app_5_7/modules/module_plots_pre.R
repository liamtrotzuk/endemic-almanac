################################################################################
# module calendar_pre
#
# Author: Liam Trotzuk
# Created: 2020-10-20
################################################################################

# Module UI ---------------------------------------------------------------

plots_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("module_ui"))
}

# Module server logic -----------------------------------------------------

plots_pre <- function(input,output,session) {
  
  load(file = "data/PLOT_initial_tempmin.rda")
  
  load(file = "data/PLOT_initial_precip.rda")
  
  REACT_plot_precip <- reactive(renderPlotly(PLOT_precip))
  
  REACT_plot_temp <- reactive(renderPlotly(PLOT_tempmin))
  
  output$module_ui <- renderUI({
    
    htmlTemplate(
      filename = "www/modules/plots/index.html",
      precip_plot_for_item_HTML = REACT_plot_precip(),
      temp_plot_for_item_HTML = REACT_plot_temp()
    )
  })
  
}