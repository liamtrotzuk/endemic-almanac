################################################################################
# module calendar_pre
#
# Author: Liam Trotzuk
# Created: 2020-10-20
################################################################################

# Module UI ---------------------------------------------------------------

map_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("module_ui"))
}

# Module server logic -----------------------------------------------------

map_main <- function(input,output,session,REACT_DF_Zip_Code_State) {
  
  DF_MapLeaflet_A <- read_csv('data/leaflet_input_initial.csv')
  
  DF_GEO_Tigris_Data <- tigris::states(cb = T)
  
  DF_MapLeaflet_B <- geo_join(DF_GEO_Tigris_Data,DF_MapLeaflet_A,"STUSPS","State_Native_Symbol") %>% 
    filter(!is.na(ID))
  
  reg_pal <- colorNumeric("Oranges",DF_MapLeaflet_B$regular_flag)
  
  endangered_pal <- colorNumeric("Reds",DF_MapLeaflet_B$endangered_flag)
  
  MAP_main_final <- leaflet(DF_MapLeaflet_B) %>%  
    addProviderTiles(providers$Stamen.Toner) %>% 
    addPolygons(stroke = FALSE, 
                fillColor = ~reg_pal(regular_flag), 
                color = "transparent",
                highlight = highlightOptions(weight = 5,
                                             fillOpacity = 0.5,
                                             color = "white",
                                             opacity = 1,
                                             bringToFront = TRUE),
                label = ~regular_flag_label,
                group = "Native Plants",
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>% 
    addMarkers(REACT_DF_Zip_Code_State()[[1,7]], 
               REACT_DF_Zip_Code_State()[[1,6]],
               icon = icons(iconUrl = "images/map-marker-3.svg",
                            iconWidth = 30,
                            iconHeight = 30,
                            iconAnchorX = 15,
                            iconAnchorY = 29)) %>% 
    setView(REACT_DF_Zip_Code_State()[[1,7]], REACT_DF_Zip_Code_State()[[1,6]], zoom = 4)
  
  REACTIVE_MAP_Main <- reactive(MAP_main_final)
  
  output$module_ui <- renderUI({
    
    htmlTemplate(
      filename = "www/modules/map/index.html",
      main_map_box = renderLeaflet(REACTIVE_MAP_Main())
    )
  })

}