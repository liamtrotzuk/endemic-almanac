################################################################################
# Server logic of the app
#
# Author: Liam Trotzuk  
# Created: 2020-10-10
################################################################################

server <- function(input, output, session) {
  
  # load namespace from session
  ns <- session$ns
  
  DF_Geo_Adjusted_B <- read_csv('data/GEO_Adjusted.csv')
  
  callModule(calendar_pre,
             "calendar")
  
  callModule(plots_pre,
             "plots_output")
  
  callModule(map_pre,
             "map_output")
  
  # Data reactives ----------------------------------------------------------
  
  observeEvent(input$main_button, {
    
    input_zip_code_entry_numeric = as.numeric(input$zip_code_entry)
    
    if (input_zip_code_entry_numeric %in% DF_Geo_Adjusted_B$postal_code)

    {
      
      show_modal_spinner(
        spin = "cube-grid",
        color = "#000000",
        text = "creating your calendar...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      #run main table calculations in here
      
      DF_Plants_Adjusted_G <- read_csv('data/PLANTS_adjusted.csv')
      DF_County_Climate_Characteristics <- read_csv('data/County_Climate_Main.csv')
      DF_Precip_Adjusted <- read_csv('data/Precip_Adjusted.csv')
      DF_Temp_Min_Adjusted <- read_csv('data/Temp_Min_Adjusted.csv')
      
      ###### 2 - filtering tables
      
      ### 2A - filter down zip code table
      DF_zip_code_state <- DF_Geo_Adjusted_B %>% 
        dplyr::filter(postal_code == input_zip_code_entry_numeric) %>% 
        dplyr::mutate(county_state = paste0(county_full,", ",state_abb),
                      county_abb_padded = str_pad(county_abb,3,pad="0"))
      
      ### 2B - filter down climate tables
      DF_County_Climate_Characteristics_Adjusted <- DF_County_Climate_Characteristics %>% 
        dplyr::filter(County_Num == DF_zip_code_state[[1,2]] & State_Num == DF_zip_code_state[[1,5]])
      
      DF_County_Precip_Min_Adjusted <- DF_Precip_Adjusted %>% 
        dplyr::filter(County_Num == DF_zip_code_state[[1,2]] & State_Num == DF_zip_code_state[[1,5]])
      
      DF_County_Temp_Min_Adjusted <- DF_Temp_Min_Adjusted %>% 
        dplyr::filter(County_Num == DF_zip_code_state[[1,2]] & State_Num == DF_zip_code_state[[1,5]])
      
      ### 2C - filter down SOIL table
      DF_SOIL_Adjusted_A_SQL <- paste0("SELECT LAOVERLAP.areasymbol,MAPUNIT.mukey,
                                       MAPUNIT.muacres,
                                       CHORIZON.hzname, 
                                       CHORIZON.hzdept_r,
                                       CHORIZON.ph1to1h2o_r
                                       FROM LAOVERLAP 
                                       INNER JOIN MAPUNIT ON LAOVERLAP.lkey = MAPUNIT.lkey
                                       INNER JOIN COMPONENT ON MAPUNIT.mukey = COMPONENT.mukey
                                       INNER JOIN CHORIZON ON COMPONENT.cokey = CHORIZON.cokey
                                       WHERE LAOVERLAP.areasymbol = ","'",DF_zip_code_state[[1,4]],DF_zip_code_state[[1,10]],"'")
      
      #on the (rare) occasion that the SoilDB is down, use try() to check and insert stand-in SOIL value make sure the end-user doesn't get an error
      DF_SOIL_Adjusted_A = try(SDA_query(DF_SOIL_Adjusted_A_SQL))
      
      if (class(DF_SOIL_Adjusted_A) != "try-error" & class(DF_SOIL_Adjusted_A) != "NULL") {
        DF_SOIL_Adjusted_B <- DF_SOIL_Adjusted_A %>% 
          dplyr::filter(hzdept_r == 0,
                        !is.na(ph1to1h2o_r),
                        !is.na(muacres)) %>% 
          distinct() %>%
          select(-hzname,
                 -hzdept_r)
        
        DF_SOIL_Adjusted_C_Reference <- DF_SOIL_Adjusted_B %>%
          dplyr::filter(!is.na(muacres)) %>% 
          select(mukey,muacres) %>% 
          distinct() %>% 
          dplyr::mutate(total_acreage = sum(muacres))
        
        DF_SOIL_Adjusted <- DF_SOIL_Adjusted_B %>% 
          dplyr::group_by(areasymbol,
                          mukey) %>% 
          dplyr::summarize(muacres = mean(muacres),
                           ph1to1h2o_r = mean(ph1to1h2o_r)) %>% 
          left_join(DF_SOIL_Adjusted_C_Reference,
                    by = c('mukey')) %>% 
          dplyr::mutate(acreage_percent = muacres.x/total_acreage) %>% 
          dplyr::mutate(weighted_average_value = acreage_percent*ph1to1h2o_r) %>% 
          dplyr::group_by(areasymbol) %>% 
          dplyr::summarize(average_county_soil_ph = sum(weighted_average_value)) %>% 
          as.data.frame() }
      
      else { 
        
        DF_SOIL_Table_FINAL_AVG_Pre <- read_csv('data/SOIL_averages_by_area_symbol.csv')
        
        DF_SOIL_Adjusted <- DF_SOIL_Table_FINAL_AVG_Pre %>% 
          filter(!is.na(average_county_soil_ph)) %>% 
          summarize(average_county_soil_ph = mean(average_county_soil_ph)) %>% 
          mutate(areasymbol = 'placeholder_areasymbol') %>% 
          select(areasymbol,
                 average_county_soil_ph)
        
      }
      
      ### 2D - have to assign values to environment inputs
      if (input$select_growth_environment == "Urban (window box, small garden)")
      {environment_input = 1}
      else
      {if (input$select_growth_environment == "Suburban (front lawn, backyard)")
      {environment_input = 2}
        else
        {environment_input = 3}}
      
      ### 2E - final main filter of PLANTS table
      DF_PLANTS_State_Adjusted_Pre <- DF_Plants_Adjusted_G %>% 
        dplyr::filter(State_Native_Symbol == DF_zip_code_state[[1,4]]
                      & Precip_Min <= DF_County_Climate_Characteristics_Adjusted[[1,3]]
                      & Temp_Min <= DF_County_Climate_Characteristics_Adjusted[[1,5]]
                      & Growing_Environment <= environment_input)
      
      ### 2F - FINAL main filter - if environment input is 'Urban', don't apply SOIL filter - 
      # probably gonna be in a window box or a small garden with pre-purchased soil
      
      if (environment_input == 1) {
        
        DF_PLANTS_State_Adjusted_Main <- DF_PLANTS_State_Adjusted_Pre
        
      }
      
      else {
        
        DF_PLANTS_State_Adjusted_Main <- DF_PLANTS_State_Adjusted_Pre %>% 
          dplyr::filter(ph_Min <= DF_SOIL_Adjusted[[1,2]]
                        & ph_Max >= DF_SOIL_Adjusted[[1,2]])
        
      }
      
      ### 2.5 - dumb bug in R won't allow you to declare column names w spaces so generating using camel case, then selecting/renaming
      DF_Existing_Table <- data.frame(Growth_Month_NUM = numeric(),
                                      Growth_Month_NAME = character(),
                                      Growth_Month_GLYPH = character(),
                                      Genus = character(),
                                      Species = character(),
                                      Common_N = character(),
                                      Scientific_Name = character(),
                                      Commercial_Avail = character(),
                                      TE_Indicator_Final = character(),
                                      Flowering = character(),
                                      Fire_Resistance = character(),
                                      Fire_Tolerance = character(),
                                      Drought_Tolerance = character(),
                                      Moisture_Use = character(),
                                      Edible_Human = character(),
                                      Edible_Animals = character(),
                                      Berry_Nut_Seed = character(),
                                      Img_Link = character(),
                                      Precip_Min = numeric(),
                                      Temp_Min = numeric())
      
      ####### 3 - everything of computational interest starts here - creating chart
      
      ### 3A - make a counting table to figure out which months have the most plants that grow to start the loop on those months
      DF_Month_Counting_Table <- DF_PLANTS_State_Adjusted_Main %>% 
        dplyr::select(Growth_Month_NUM,
                      Common_N) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(Count = 1) %>% 
        dplyr::group_by(Growth_Month_NUM) %>% 
        dplyr::summarize(Count_P = sum(Count)) %>% 
        dplyr::arrange(Count_P,
                       Growth_Month_NUM) %>% 
        as.data.frame()
      
      ### 3B - loop generates main table
      for (x in DF_Month_Counting_Table$Growth_Month_NUM) {
        
        DF_Month_Pre <- DF_PLANTS_State_Adjusted_Main %>% 
          anti_join(DF_Existing_Table %>% select(Common_N),
                    by = c('Common_N'))
        
        if (x %in% DF_Month_Pre$Growth_Month_NUM) {
          
          DF_Month <- DF_Month_Pre %>% 
            dplyr::filter(Growth_Month_NUM == x) %>% 
            dplyr::arrange(dplyr::desc(!!rlang::sym(paste(as.character(input$select_desired_characteristics),"RANK")))) %>% 
            dplyr::mutate(Scientific_Name = paste(Genus,Species),
                          Image_Link = paste0("https://species.wikimedia.org/wiki/",as.character(Genus),"_",as.character(Species))) %>%
            select(Growth_Month_NUM,
                   Growth_Month_NAME,
                   Growth_Month_GLYPH,
                   Genus,
                   Species,
                   Common_N,
                   Scientific_Name,
                   Commercial_Avail,
                   TE_Indicator_Final,
                   Flowering,
                   Fire_Resistance,
                   Fire_Tolerance,
                   Drought_Tolerance,
                   Moisture_Use,
                   Edible_Human,
                   Edible_Animal,
                   Berry_Nut_Seed,
                   Image_Link,
                   Precip_Min,
                   Temp_Min) %>% 
            head(3) %>% 
            sample_n(1)
          
          DF_Existing_Table %>% rbind(DF_Month) -> DF_Existing_Table
          
        }
        
        else {
          
          next
          
        }
        
      }
      
      #arrange main table in order
      DF_Existing_Arranged <- DF_Existing_Table %>% dplyr::arrange(Growth_Month_NUM)
      
      # turning every table required by modules to REACTIVE types to pass them into modules
      REACT_DF_Existing_Arranged_Final <- reactive( 
        {DF_Existing_Arranged %>%
            mutate(slide = seq(1, n()),
                   first = 1 == slide)
        } )
      
      REACT_DF_Zip_Code_State <- reactive(DF_zip_code_state)
      REACT_DF_Plants_Adjusted <- reactive(DF_Plants_Adjusted_G)
      REACT_DF_County_Precip_Min_Adjusted <- reactive(DF_County_Precip_Min_Adjusted)
      REACT_DF_County_Temp_Min_Adjusted <- reactive(DF_County_Temp_Min_Adjusted)
      
      callModule(calendar_main,
                 "calendar",
                 REACT_DF_Existing_Arranged_Final,
                 REACT_DF_Zip_Code_State)
      
      callModule(plots_main,
                 "plots_output",
                 REACT_DF_Existing_Arranged_Final,
                 REACT_DF_County_Precip_Min_Adjusted,
                 REACT_DF_County_Temp_Min_Adjusted)
      
      callModule(map_main,
                 "map_output",
                 REACT_DF_Zip_Code_State)
      
      remove_modal_spinner()
      
      }
  
    else
      
    {callModule(calendar_error,
                "calendar")}
})
  
}