################################################################################
# UI of the app
#
# Author: Liam Trotzuk
# Created: 2020-10-10
################################################################################
htmlTemplate(
  
  filename = "www/index.html",
  
  ### images ###
  sunflower_logo = tags$img(src = 'images/sunflower-icon.png',
                            class = "img-fluid",
                            width = "50",
                            height = "50"),
  
  usda_logo = tags$img(src = "images/USDA_short.png",
                       class="img-fluid", 
                       id="footer-img", 
                       alt="Responsive image", 
                       width="300", 
                       height="300"),
  
  ncss_logo = tags$img(src = "images/ncss_stacked_color.png",
                       class="img-fluid",
                       id="footer-img",
                       alt="Responsive image",
                       width="300",
                       height="300"),
  
  noaa_logo = tags$img(src = "images/noaa.png",
                       class="img-fluid",
                       id="footer-img",
                       alt="Responsive image",
                       width="300",
                       height="300"),
  
  wikimedia_logo = tags$img(src = "images/wikimedia.png",
                            class="img-fluid",
                            id="footer-img",
                            alt="Responsive image",
                            width="300",
                            height="300"),
  
  nrcs_logo = tags$img(src = "images/NRCSlogo.png",
                       class="img-fluid",
                       id="footer-img",
                       alt="Responsive image",
                       width="300",
                       height="300"),
  
  zip_code_input = textInput("zip_code_entry",
                             "what's your zip code?",
                             value = "",
                             width = '170px'),
  
  select_growth_environment = radioButtons("select_growth_environment",
                                           "what type of environment do you live in?",
                                           c("Urban (window box, small garden)",
                                             "Suburban (front lawn, backyard)",
                                             "Rural (fields, woodland)"),
                                           inline = TRUE),
  
  select_desired_characteristics = radioButtons("select_desired_characteristics",
                                                      "what characteristic  are you most looking for in your plants?",
                                                      c("Available to Buy",
                                                        "Threatened in my State",
                                                        "Flowering (Pollinator-Friendly)",
                                                        "Fire Resistance",
                                                        "Fire Tolerance",
                                                        "Drought Tolerance",
                                                        "Moisture Use",
                                                        "Edible for People",
                                                        "Edible for Animals",
                                                        "Produces Berries/Nuts/Seeds"),
                                                      inline = TRUE),
  
  enter_button = actionButton("main_button","enter"),
  
  calendar_output = calendar_ui("calendar"),
  
  map_output = map_ui("map_output"),
  
  plots_output = plots_ui("plots_output")
  
)
