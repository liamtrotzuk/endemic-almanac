################################################################################
# module calendar
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

calendar_main <- function(input,output,session,REACT_DF_Existing_Arranged_Final,REACT_DF_Zip_Code_State) {
  
  # Package UI --------------------------------------------------------------
  
  output$module_ui <- renderUI({
    req(REACT_DF_Existing_Arranged_Final())
    
    LST_calendar_pre <- split(REACT_DF_Existing_Arranged_Final(), seq(nrow(REACT_DF_Existing_Arranged_Final())))
    
    htmlTemplate(
      filename = "www/modules/calendar/index.html",
      county = REACT_DF_Zip_Code_State()[[1,9]],
      items = calendar_items_ui(LST_calendar_pre),
      captions = calendar_captions_ui(LST_calendar_pre)
    )
  })
  
  calendar_items_ui <- function(LST_calendar_pre) {
    tagList(
      lapply(LST_calendar_pre, function(x) {
        htmlTemplate(
          filename = "www/modules/calendar/item.html", 
          growth_month = x$Growth_Month_NAME, 
          common_name = x$Common_N,
          scientific_name = x$Scientific_Name,
          available_to_buy = x$Commercial_Avail,
          threatened_in_my_state = x$TE_Indicator_Final,
          flowering = x$Flowering,
          fire_resistance = x$Fire_Resistance,
          fire_tolerance = x$Fire_Tolerance,
          moisture_use = x$Moisture_Use,
          drought_tolerance = x$Drought_Tolerance,
          edible_for_people = x$Edible_Human,
          edible_for_animals = x$Edible_Animal,
          berries_nuts_seeds = x$Berry_Nut_Seed,
          genus = x$Genus,
          species = x$Species,
          image_link = paste0("https:",gsub("((\\.JPG)|(\\.jpg)|(\\.png)|(\\.PNG)).*","\\1",str_remove((xml_attrs((read_html(x$Image_Link)%>%html_nodes("img"))[[1]])[["src"]]),"/thumb"))),
          #
          active_class = ifelse(x$first, "active", "")
          )
      }),
      tags$script(src = "js/main.js")
    )
  }
  
  calendar_captions_ui <- function(LST_calendar_pre) {
    tagList(
      sapply(LST_calendar_pre, function(x) {
        htmlTemplate(
          filename = "www/modules/calendar/caption.html", 
          growth_month = x$Growth_Month_NAME,
          growth_month_glyph = tags$i(class = x$Growth_Month_GLYPH),
          slide = x$slide - 1,
          active_class = ifelse(x$first, 'class="active"', "")
        )
      })
    )
  }
}