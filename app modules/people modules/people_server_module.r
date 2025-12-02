library(shiny)
library(tidyverse)

## Create a module for community tab server

community_server <- function(id, local_authority, gender){
  
  moduleServer(id = id, 
               module = function(input, output, session){
                 
                 ## Indicator selection
                 observeEvent(eventExpr = input$indicatorSelectInfo, 
                              handlerExpr = {
                                showModal(modalDialog(
                                  title = NULL,
                                  "Select an indicator to see how it correlates with healthy life expectancy in the adjacent chart",
                                  easyClose = TRUE,
                                  footer = NULL,
                                  size = "m"))
                              })
                 
                 
                 ## Loneliness stat
                 loneliness_stat <- reactive({
                   
                   community_data_laua %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(`Often/always feel lonely (%)`) %>% 
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 output$loneliness_stat <- renderUI({
                   
                   HTML(str_c(str_replace_na(string = loneliness_stat(),
                                             replacement = "-"),
                              "%"))
                   
                 })
                 
                 output$loneliness_text <- renderUI({
                   
                   HTML(str_c('of people in <b>',
                              local_authority(),
                              '</b> often or always feel lonely'))
                   
                 })
                 
                 ## Neighbourhood belonging stat
                 belonging_stat <- reactive({
                   
                   community_data_laua %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(`Feel strongly belong in neighbourhood (%)`) %>% 
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 output$belonging_stat <- renderUI({
                   
                   HTML(str_c(str_replace_na(string = belonging_stat(),
                                             replacement = "-"),
                              '%'))
                   
                 })
                 
                 output$belonging_text <- renderUI({
                   
                   HTML(str_c("of people in <b>",
                              local_authority(),
                              "</b> feel strongly that they belong in their neighbourhood"))
                   
                 })
                 
                 ## Chat to neighbours stat
                 chatting_stat <- reactive({
                   
                   community_data_laua %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(`Chat to neighbours at least once per month (%)`) %>% 
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 output$chatting_stat <- renderUI({
                   
                   HTML(str_c(str_replace_na(string = chatting_stat(),
                                             replacement = "-"),
                              '%'))
                 })
                 
                 output$chatting_text <- renderUI({
                   
                   HTML(str_c('of people in <b>',
                              local_authority(),
                              '</b> chat to their neighbours at least once per month'))
                   
                 })
                 
                 ## Support if needed stat
                 support_stat <- reactive({
                   
                   community_data_laua %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(`Have people there for them if they need help (%)`) %>% 
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 output$support_stat <- renderUI({
                   
                   HTML(str_c(str_replace_na(string = support_stat(),
                                             replacement = "-"),
                              '%'))
                   
                 })
                 
                 output$support_text <- renderUI({
                   
                   HTML(str_c('of people in <b>',
                              local_authority(),
                              '</b> have someone there for them if they need help'))
                   
                 })
                 
                 output$buffer1 <- renderUI({
                   
                 })
                 
                 output$buffer2 <- renderUI({
                   
                 })
                 
                 output$buffer3 <- renderUI({
                   
                 })
                 
                 output$buffer4 <- renderUI({
                   
                 })
                 
                 
                 output$buffer5 <- renderUI({
                   
                 })
                 
                 output$buffer6 <- renderUI({
                   
                 })
                 
                 output$buffer7 <- renderUI({
                   
                 })
                 
                 ## Healthy life expectancy scatter chart data
                 
                 hle_chart_data <- reactive({
                   
                   if(gender() == "Men"){
                     
                     community_data_laua %>%  
                       select(`Local authority name`,
                              "indicator" = .data[[input$chart_selector]],
                              `Healthy Life Expectancy (HLE): 2021-23 (Males)`,
                              colour) %>% 
                       mutate(colour = case_when(`Local authority name` == local_authority() ~ mid_red,
                                                 TRUE ~ colour)) %>%
                       drop_na(indicator,
                               `Healthy Life Expectancy (HLE): 2021-23 (Males)`)
                     
                   } else if(gender() == "Women"){
                     
                     community_data_laua %>%  
                       select(`Local authority name`,
                              "indicator" = .data[[input$chart_selector]],
                              `Healthy Life Expectancy (HLE): 2021-23 (Females)`,
                              colour) %>% 
                       mutate(colour = case_when(`Local authority name` == local_authority() ~ mid_red,
                                                 TRUE ~ colour)) %>%
                       drop_na(indicator,
                               `Healthy Life Expectancy (HLE): 2021-23 (Females)`)
                     
                   }
                   
                 })
                 
                 ## Healthy life expectancy scatter chart title
                 
                 hle_chart_indicator <- reactive({
                   
                   if(input$chart_selector == community_chart_variables[1]){
                     "people who often/always feel lonely"
                   } else if(input$chart_selector == community_chart_variables[2]){
                     "people who feel strongly that they belong in their neighbourhood"
                   } else if(input$chart_selector == community_chart_variables[3]){
                     "people who chat to their neighbours at least once per month"
                   } else if(input$chart_selector == community_chart_variables[4]){
                     "people who have someone there for them if they need help"
                   } 
                   
                 })
                 
                 hle_chart_title_text <- reactive({
                   if(input$chart_selector == community_chart_variables[1]){
                     "people who often or always feel lonely"
                   } else{
                     hle_chart_indicator()
                   }
                 })
                 
                 hle_chart_title <- reactive(
                   str_c("Healthy life expectancy for ",
                         str_to_lower(gender()),
                         " at birth (2021–23) by ",
                         hle_chart_title_text(),
                         ": England")
                 )
                 
                 output$hle_chart_title <- renderText({
                   
                   hle_chart_title()
                   
                 })
                 
                 ## Healthy life expectancy scatter chart
                 
                 output$hle_chart <- renderHighchart({
                   
                   if(gender() == "Women"){
                     
                     highchart() %>%
                       
                       hc_plotOptions(scatter = list(opacity = 0.7,
                                                     marker = (list(enabled = TRUE,
                                                                    symbol = "circle",
                                                                    radius = 6)))) %>%
                       
                       hc_title(text = "",
                                align = "left",
                                style = list(color = blue,
                                             fontWeight = "400")) %>%
                       
                       hc_xAxis(title = list(text = str_to_sentence(string = hle_chart_indicator()),
                                             style = list(fontSize = "0.6rem")),
                                labels = list(format = "{value:.0f}%",
                                              style = list(fontSize = "0.5rem")),
                                min = min(hle_chart_data()$indicator,
                                          na.rm = TRUE),
                                max = max(hle_chart_data()$indicator,
                                          na.rm = TRUE),
                                plotLines = list(list(color = light_blue,
                                                      dashStyle = "dash",
                                                      value = community_england_data[[input$chart_selector]],
                                                      width = 1,
                                                      label = list(text = "England",
                                                                   rotation = 0,
                                                                   align = "left",
                                                                   y = 10,
                                                                   x = 4,
                                                                   style = list(color = light_blue,
                                                                                fontSize = "0.6rem",
                                                                                fontWeight = "normal"))))) %>%
                       
                       hc_yAxis(title = list(text = "Healthy life expectancy for women (years)",
                                             style = list(fontSize = "0.6rem")),
                                min = 50,
                                max = 75,
                                labels = list(style = list(fontSize = "0.5rem")),
                                plotLines = list(list(color = light_blue,
                                                      dashStyle = "dash",
                                                      value = community_england_data$`Healthy Life Expectancy (HLE): 2021-23 (Females)`,
                                                      width = 1,
                                                      label = list(text = "England",
                                                                   rotation = 0,
                                                                   align = "left",
                                                                   y = -6,
                                                                   x = 0,
                                                                   style = list(color = light_blue,
                                                                                fontSize = "0.6rem",
                                                                                fontWeight = "normal"))))) %>%
                       
                       ## Plotting Data
                       hc_add_series(data = hle_chart_data(),
                                     type = "scatter",
                                     name = "scatter_series",
                                     hcaes(x = indicator,
                                           y = `Healthy Life Expectancy (HLE): 2021-23 (Females)`,
                                           name = `Local authority name`,
                                           color = colour),
                                     showInLegend = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     zIndex = 1) %>%
                       
                       hc_add_series(data = filter(.data = hle_chart_data(),
                                                   `Local authority name` == local_authority()),
                                     type = "scatter",
                                     name = "selected_point",
                                     hcaes(x = indicator,
                                           y = `Healthy Life Expectancy (HLE): 2021-23 (Females)`,
                                           name = `Local authority name`,
                                           color = mid_red),
                                     showInLegend = FALSE,
                                     includeInDataExport = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     zIndex = 2,
                                     dataLabels = list(enabled = TRUE, 
                                                       format = '{point.name}',
                                                       allowOverlap = TRUE,
                                                       style = list(color = black,
                                                                    fontSize = "0.6rem"))) %>% 
                       
                       hc_tooltip(headerFormat = "",
                                  pointFormat = str_c("<b>{point.name}</b><br> ",
                                                      str_to_sentence(string = hle_chart_indicator()),
                                                      ": <b>{point.x:.1f}% </b><br> Healthy life expectancy (women): <b>{point.y:.1f} years </b>"),
                                  style = list(fontSize = "0.6rem")) %>%
                       
                       hc_exporting(enabled = TRUE,
                                    buttons = list(contextButton = list(menuItems = list('downloadSVG',
                                                                                         'downloadPNG',
                                                                                         'downloadCSV'))),
                                    chartOptions = list(title = list(text = hle_chart_title(),
                                                                     align = "left",
                                                                     style = list(fontSize = "18px")),
                                                        caption = list(text = ""),
                                                        chart = list(width = 800,
                                                                     height = 600,
                                                                     backgroundColor = "white",
                                                                     style = list(fontSize = "14px"))))
                     
                   } else if(gender() == "Men"){
                     
                     highchart() %>%
                       
                       hc_plotOptions(scatter = list(opacity = 0.7,
                                                     marker = (list(enabled = TRUE,
                                                                    symbol = "circle",
                                                                    radius = 6)))) %>%
                       
                       hc_title(text = "",
                                align = "left",
                                style = list(color = blue,
                                             fontWeight = "400")) %>%
                       
                       hc_xAxis(title = list(text = str_to_sentence(string = hle_chart_indicator()),
                                             style = list(fontSize = "0.6rem")),
                                labels = list(format = "{value:.0f}%",
                                              style = list(fontSize = "0.5rem")),
                                min = min(hle_chart_data()$indicator,
                                          na.rm = TRUE),
                                max = max(hle_chart_data()$indicator,
                                          na.rm = TRUE),
                                plotLines = list(list(color = light_blue,
                                                      dashStyle = "dash",
                                                      value = community_england_data[[input$chart_selector]],
                                                      width = 1,
                                                      label = list(text = "England",
                                                                   rotation = 0,
                                                                   align = "left",
                                                                   y = 10,
                                                                   x = 4,
                                                                   style = list(color = light_blue,
                                                                                fontSize = "0.6rem",
                                                                                fontWeight = "normal"))))) %>%
                       
                       hc_yAxis(title = list(text = "Healthy life expectancy for men (years)",
                                             style = list(fontSize = "0.6rem")),
                                min = 50,
                                max = 75,
                                labels = list(style = list(fontSize = "0.5rem")),
                                plotLines = list(list(color = light_blue,
                                                      dashStyle = "dash",
                                                      value = community_england_data$`Healthy Life Expectancy (HLE): 2021-23 (Males)`,
                                                      width = 1,
                                                      label = list(text = "England",
                                                                   rotation = 0,
                                                                   align = "left",
                                                                   y = -6,
                                                                   x = 0,
                                                                   style = list(color = light_blue,
                                                                                fontSize = "0.6rem",
                                                                                fontWeight = "normal"))))) %>%
                       
                       ## Plotting Data
                       hc_add_series(data = hle_chart_data(),
                                     type = "scatter",
                                     name = "scatter_series",
                                     hcaes(x = indicator,
                                           y = `Healthy Life Expectancy (HLE): 2021-23 (Males)`,
                                           name = `Local authority name`,
                                           color = colour),
                                     showInLegend = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     zIndex = 1) %>%
                       
                       hc_add_series(data = filter(.data = hle_chart_data(),
                                                   `Local authority name` == local_authority()),
                                     type = "scatter",
                                     name = "selected_point",
                                     hcaes(x = indicator,
                                           y = `Healthy Life Expectancy (HLE): 2021-23 (Males)`,
                                           name = `Local authority name`,
                                           color = mid_red),
                                     showInLegend = FALSE,
                                     includeInDataExport = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     zIndex = 2,
                                     dataLabels = list(enabled = TRUE, 
                                                       format = '{point.name}',
                                                       allowOverlap = TRUE,
                                                       style = list(color = black,
                                                                    fontSize = "0.6rem"))) %>% 
                       
                       hc_tooltip(headerFormat = "",
                                  pointFormat = str_c("<b>{point.name}</b><br> ",
                                                      str_to_sentence(string = hle_chart_indicator()),
                                                      ": <b>{point.x:.1f}% </b><br> Healthy life expectancy (men): <b>{point.y:.1f} years </b>"),
                                  style = list(fontSize = "0.6rem")) %>%
                       
                       hc_exporting(enabled = TRUE,
                                    buttons = list(contextButton = list(menuItems = list('downloadSVG',
                                                                                         'downloadPNG',
                                                                                         'downloadCSV'))),
                                    chartOptions = list(title = list(text = hle_chart_title(),
                                                                     align = "left",
                                                                     style = list(fontSize = "18px")),
                                                        caption = list(text = ""),
                                                        chart = list(width = 800,
                                                                     height = 600,
                                                                     backgroundColor = "white",
                                                                     style = list(fontSize = "14px"))))
                     
                   }
                   
                 })
                 
                 ## Comparison Stats
                 
                 indicator_max_la <- reactive({
                   
                   community_data_laua %>% 
                     slice_max(.data[[input$chart_selector]],
                               n = 1, 
                               with_ties = FALSE) %>% 
                     pull(`Local authority name`)
                   
                 })
                 
                 indicator_max_value <- reactive({
                   
                   community_data_laua %>%
                     filter(`Local authority name` == indicator_max_la()) %>%
                     pull(input$chart_selector) %>%
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 indicator_min_la <- reactive({
                   
                   community_data_laua %>% 
                     slice_min(.data[[input$chart_selector]],
                               n = 1, 
                               with_ties = FALSE) %>% 
                     pull(`Local authority name`)
                   
                 })
                 
                 indicator_min_value <- reactive({
                   
                   community_data_laua %>%
                     filter(`Local authority name` == indicator_min_la()) %>%
                     pull(input$chart_selector) %>%
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 indicator_rank <- reactive({
                   
                   community_data_laua %>%
                     arrange(desc(.data[[input$chart_selector]])) %>%
                     mutate(rank = row_number()) %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(rank)
                   
                 })
                 
                 output$max_stat <- renderUI({
                   
                   HTML(str_c(indicator_max_value(),
                              '%'))
                   
                 })
                 
                 output$max_text <- renderUI({
                   
                   HTML(str_c('<b>',
                                indicator_max_la(),
                                '</b> has the highest proportion of ',
                                hle_chart_indicator()))
                   
                 })
                 
                 output$min_stat <- renderUI({
                   
                   HTML(str_c(indicator_min_value(),
                              '%'))
                   
                 })
                 
                 output$min_text <- renderUI({
                   
                   HTML(str_c('<b>',
                                indicator_min_la(),
                                '</b> has the lowest proportion of ',
                                hle_chart_indicator()))
                 })
                 
                 output$rank_stat <- renderUI({
                   
                   last_digit <- str_sub(string = indicator_rank(),
                                         start = -1L,
                                         end = -1L)
                   
                   if(indicator_rank() %in% c("11",
                                        "12",
                                        "13",
                                        "111",
                                        "112",
                                        "113")){
                     ordinal <- "th"
                   } else if(last_digit == "1"){
                     ordinal <- "st"
                   } else if(last_digit == "2"){
                     ordinal <- "nd"
                   } else if(last_digit == "3"){
                     ordinal <- "rd"
                   } else{
                     ordinal <- "th"
                   }
                   
                   HTML(str_c(indicator_rank(),
                              '<sup>',
                              ordinal,
                              '</sup>/151'))
                 })
                 
                 output$rank_text <- renderUI({
                   
                   HTML(str_c(str_to_sentence(hle_chart_indicator()),
                              ' rank of <b>',
                              local_authority(),
                              '</b>'))
                   
                 })
                 
                 ## HLE by single-person households Title
                 
                 hle_singlehouseholds_title <- reactive(
                   str_c("Single-person households by life expectancy: neighbourhoods (MSOAs) in ",
                         local_authority())
                 )
                 
                 output$hle_singlehouseholds_title <- renderText({
                   hle_singlehouseholds_title()
                 })
                 
                 ## HLE by single-person households Legend
                 
                 output$legend <- renderUI({
                   
                   legend <- tags$ul(class = "jointlegend")
                   colors <- map5
                   legend$children <- lapply(seq_len(length(colors)), function(color) {
                     tags$li(
                       class = "legend-item legend-color",
                       style = paste0(
                         "background-color:", colors[color]
                       ),
                     )
                   })
                   legend$children <- tagList(
                     tags$li(
                       class = "legend-item legend-label left-label",
                       HTML("Fewest single-person households")
                     ),
                     legend$children,
                     tags$li(
                       class = "legend-item legend-label right-label",
                       HTML("Most single-person households")
                     )
                   )
                   tagList(
                     tags$span(class = "legend-title",
                               "England (March 2021)"),
                     legend
                   )
                   
                 })
                 
                 
                 
                 ## HLE by single-person households Map
                 
                 output$hle_singlehouseholds_map <- renderHighchart({
                   
                   msoa_codes <- community_data_msoa %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(MSOA11CD)
                   
                   hle_map <- highchart(type = "map") %>% 
                     
                     hc_title(text = "") %>% 
                     
                     hc_colorAxis(dataClasses = list(list(color = map5[1],
                                                          from = 0.5,
                                                          to = 2.5,
                                                          name = "1st quintile"),
                                                     list(color = map5[2],
                                                          from = 2.5,
                                                          to = 4.5,
                                                          name = "2nd quintile"),
                                                     list(color = map5[3],
                                                          from = 4.5,
                                                          to = 6.5,
                                                          name = "3rd quintile"),
                                                     list(color = map5[4],
                                                          from = 6.5,
                                                          to = 8.5,
                                                          name = "4th quintile"),
                                                     list(color = map5[5],
                                                          from = 8.5,
                                                          to = 10.5,
                                                          name = "5th quintile"))) %>%
                     
                     hc_legend(enabled = FALSE) %>% 
                     
                     hc_mapView(projection = list(name = "WebMercator")) %>% 
                     
                     hc_add_event_series(event = "click")
                   
                   if(gender() == "Men"){
                     
                     df <- community_data_msoa %>% 
                       filter(`Local authority name` == local_authority()) %>% 
                       select(MSOA11CD,
                              "value" = index,
                              msoa11hclnm,
                              "le" = `Male life expectancy (2016-2020 based)`)
                     
                     for(i in 1:nrow(df)){
                       df_temp <- df %>% 
                         slice(i)
                       
                       msoa_code <- df_temp %>% 
                         pull(MSOA11CD)
                       
                       map_boundaries_temp <- msoa_boundaries[[msoa_code]]
                       
                       msoa_name <- df_temp %>% 
                         pull(msoa11hclnm)
                       
                       if(selected_msoa() == msoa_name){
                         hle_map <- hle_map %>% 
                           
                           hc_add_series(mapData = map_boundaries_temp, 
                                         data = df_temp, 
                                         joinBy = "MSOA11CD",
                                         name = df_temp$msoa11hclnm,
                                         nullColor = grey,
                                         nullInteraction = TRUE,
                                         borderColor = mid_red,
                                         borderWidth = 2,
                                         index = 500)
                       } else{
                         hle_map <- hle_map %>% 
                           
                           hc_add_series(mapData = map_boundaries_temp, 
                                         data = df_temp, 
                                         joinBy = "MSOA11CD",
                                         name = df_temp$msoa11hclnm,
                                         nullColor = grey,
                                         nullInteraction = TRUE,
                                         borderColor = black,
                                         borderWidth = 1,
                                         index = i)
                       }
                     }
                     
                     hle_map %>% 
                       
                       hc_tooltip(useHTML = TRUE,
                                  headerFormat = "",
                                  pointFormat = "<b>{point.msoa11hclnm}</b><br>
                                               Life expectancy (men): {point.le:.1f} years<br>
                                               Single-person households decile: {point.value:.0f}",
                                  nullFormat = "<b>{point.msoa11hclnm}</b><br>
                                               Life expectancy (men): {point.le:.1f} years<br>
                                               Single-person households decile: NA")
                     
                   } else if(gender() == "Women"){
                     
                     df <- community_data_msoa %>% 
                       filter(`Local authority name` == local_authority()) %>% 
                       select(MSOA11CD,
                              "value" = index,
                              msoa11hclnm,
                              "le" = `Female life expectancy (2016-2020 based)`)
                     
                     for(i in 1:nrow(df)){
                       df_temp <- df %>% 
                         slice(i)
                       
                       msoa_code <- df_temp %>% 
                         pull(MSOA11CD)
                       
                       map_boundaries_temp <- msoa_boundaries[[msoa_code]]
                       
                       msoa_name <- df_temp %>% 
                         pull(msoa11hclnm)
                       
                       if(selected_msoa() == msoa_name){
                         hle_map <- hle_map %>% 
                           
                           hc_add_series(mapData = map_boundaries_temp, 
                                         data = df_temp, 
                                         joinBy = "MSOA11CD",
                                         name = df_temp$msoa11hclnm,
                                         nullColor = grey,
                                         nullInteraction = TRUE,
                                         borderColor = mid_red,
                                         borderWidth = 2,
                                         index = 500)
                       } else{
                         hle_map <- hle_map %>% 
                           
                           hc_add_series(mapData = map_boundaries_temp, 
                                         data = df_temp, 
                                         joinBy = "MSOA11CD",
                                         name = df_temp$msoa11hclnm,
                                         nullColor = grey,
                                         nullInteraction = TRUE,
                                         borderColor = black,
                                         borderWidth = 1,
                                         index = i)
                       }
                     }
                     
                     hle_map %>% 
                       hc_tooltip(useHTML = TRUE,
                                  headerFormat = "",
                                  pointFormat = "<b>{point.msoa11hclnm}</b><br>
                                               Life expectancy (women): {point.le:.1f} years<br>
                                               Single-person households decile: {point.value:.0f}",
                                  nullFormat = "<b>{point.msoa11hclnm}</b><br>
                                               Life expectancy (women): {point.value:.1f} years<br>
                                               Single-person households decile: NA")
                   }
                   
                 })
                 
                 
                 
                 ## Create a reactive for clicking between map and chart
                 
                 selected_msoa <- reactiveVal(value = "")
                 
                 observeEvent(eventExpr = input$hle_singlehouseholds_map_click$name,
                              handlerExpr = {
                                
                                selected_msoa(input$hle_singlehouseholds_map_click$name)
                                
                              })
                 
                 observeEvent(eventExpr = input$hle_singlehouseholds_chart_click$name,
                              handlerExpr = {
                                
                                selected_msoa(input$hle_singlehouseholds_chart_click$name)
                                
                              })
                 
                 # Create a dataframe for series based on clicked MSOA
                 selected_msoa_df <- reactive({
                   
                   if(is.null(selected_msoa()) == TRUE){
                     community_data_msoa %>% 
                       filter(`MSOA code` == "drop")
                     
                   } else{
                     
                     community_data_msoa %>% 
                       filter(`Local authority name` == local_authority()) %>% 
                       mutate(colour = case_when(msoa11hclnm == selected_msoa() ~ mid_red,
                                                        TRUE ~ rgb(0,0,0,0)),
                              label = case_when(msoa11hclnm == selected_msoa() ~ msoa11hclnm,
                                                TRUE ~ ""))
                     
                   }
                   
                 })
                 
                 
                 ## HLE by single-person households Chart
                 
                 hle_singlehouseholds_chart_data <- reactive({
                   
                   community_data_msoa %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     filter(msoa11hclnm != selected_msoa())
                   
                 })
                 
                 
                 output$hle_singlehouseholds_chart <- renderHighchart({
                   
                   if(gender() == "Men"){
                     
                     highchart() %>%
                       
                       hc_chart(marginBottom = "65") %>% 
                       
                       hc_plotOptions(scatter = list(animation = FALSE)) %>% 
                       
                       hc_xAxis(title = list(text = "Single-person households decile",
                                             style = list(fontSize = "0.6rem")),
                                categories = unique(community_data_msoa$`Single-person households decile`),
                                labels = list(style = list(fontSize = "0.5rem",
                                                           textOverflow = "none"),
                                              format = "{text}",
                                              rotation = 0),
                                max = 9) %>%
                       
                       hc_yAxis(title = list(text = "Life expectancy for men (years)",
                                             style = list(fontSize = "0.6rem")),
                                labels = list(format = "{value:.0f}",
                                              style = list(fontSize = "0.5rem")),
                                plotLines = list(list(width = 1.5,
                                                      color = light_blue,
                                                      value = median(x = community_data_msoa$`Male life expectancy (2016-2020 based)`, 
                                                                     na.rm = TRUE),
                                                      zIndex = 5,
                                                      label = list(align = "center",
                                                                   rotation = 270,
                                                                   style = list(color = light_blue,
                                                                                fontSize = "0.6rem"),
                                                                   text = "England median",
                                                                   textAlign = "left",
                                                                   x = -1)))) %>%
                       
                       ## Add scatter series of greyed out
                       hc_add_series(data = community_greys,
                                     name = "Greyed out",
                                     type = "scatter",
                                     jitter = list(x = 0.25,
                                                   y = 0),
                                     hcaes(x = index - 1,
                                           y = `Male life expectancy (2016-2020 based)`),
                                     color = grey,
                                     marker = list(symbol = "circle", 
                                                   radius = 2),
                                     enableMouseTracking = FALSE,
                                     includeInDataExport = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     showInLegend = FALSE) %>%
                       
                       ## Add scatter series of msoas
                       hc_add_series(data = hle_singlehouseholds_chart_data(),
                                     name = "Life expectancy (men)",
                                     type = "scatter",
                                     jitter = list(x = 0.25,
                                                   y = 0),
                                     hcaes(x = index - 1,
                                           y = `Male life expectancy (2016-2020 based)`,
                                           name = msoa11hclnm,
                                           color = colour),
                                     marker = list(symbol = "circle", 
                                                   radius = 3),
                                     tooltip = list(headerFormat = "<span><b>{point.key}</b> <br>",
                                                    pointFormat = "{series.name}: <b>{point.y:.1f} years"),
                                     states = list(inactive = list(enabled = FALSE)),
                                     showInLegend = FALSE) %>%
                       
                       ## Add scatter series of selected MSOA
                       hc_add_series(data = selected_msoa_df(),
                                     dataLabels = list(enabled = TRUE,
                                                       allowOverlap = TRUE,
                                                       format = '{point.label}',
                                                       style = list(color = black,
                                                                    fontSize = "0.6rem")),
                                     name = "Life expectancy (men)",
                                     type = "scatter",
                                     jitter = list(x = 0.25,
                                                   y = 0),
                                     hcaes(x = index - 1,
                                           y = `Male life expectancy (2016-2020 based)`,
                                           name = msoa11hclnm,
                                           color = colour,
                                           label = label),
                                     marker = list(symbol = "circle", 
                                                   radius = 5),
                                     tooltip = list(headerFormat = "<span><b>{point.key}</b> <br>",
                                                    pointFormat = "{series.name}: <b>{point.y:.1f} years"),
                                     includeInDataExport = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     showInLegend = FALSE) %>%
                       
                       hc_tooltip(style = list(fontSize = "0.6rem")) %>% 
                       
                       hc_add_event_point(series = "series",
                                          event = "click") %>% 
                       
                       hc_exporting(enabled = TRUE,
                                    buttons = list(contextButton = list(menuItems = list('downloadSVG',
                                                                                         'downloadPNG',
                                                                                         'downloadCSV'))),
                                    chartOptions = list(title = list(text = hle_singlehouseholds_title(),
                                                                     align = "left",
                                                                     style = list(fontSize = "18px")),
                                                        caption = list(text = ""),
                                                        chart = list(width = 800,
                                                                     height = 600,
                                                                     backgroundColor = "white",
                                                                     style = list(fontSize = "14px"))))
                     
                   } else if(gender() == "Women"){
                     
                     highchart() %>%
                       
                       hc_chart(marginBottom = "65") %>% 
                       
                       hc_plotOptions(column = list(dataLabels = list(align = "top", 
                                                                      rotation = 270, 
                                                                      y = -5))) %>% 
                       
                       hc_xAxis(title = list(text = "Single-person households decile",
                                             style = list(fontSize = "0.6rem")),
                                categories = unique(community_data_msoa$`Single-person households decile`),
                                labels = list(style = list(fontSize = "0.5rem",
                                                           textOverflow = "none"),
                                              format = "{text}",
                                              rotation = 0),
                                max = 9) %>%
                       
                       hc_yAxis(title = list(text = "Life expectancy for women (years)",
                                             style = list(fontSize = "0.6rem")),
                                labels = list(format = "{value:.0f}",
                                              style = list(fontSize = "0.5rem")),
                                plotLines = list(list(width = 1.5,
                                                      color = light_blue,
                                                      dashStyle = "dash",
                                                      value = median(x = community_data_msoa$`Female life expectancy (2016-2020 based)`, 
                                                                     na.rm = TRUE),
                                                      zIndex = 5,
                                                      label = list(align = "center",
                                                                   rotation = 270,
                                                                   style = list(color = light_blue,
                                                                                fontSize = "0.6rem"),
                                                                   text = "England median",
                                                                   textAlign = "left",
                                                                   x = -1)))) %>%
                       
                       ## Add scatter series of greyed out
                       hc_add_series(data = community_greys,
                                     name = "Greyed out",
                                     type = "scatter",
                                     jitter = list(x = 0.25,
                                                   y = 0),
                                     hcaes(x = index - 1,
                                           y = `Female life expectancy (2016-2020 based)`),
                                     color = grey,
                                     marker = list(symbol = "circle", 
                                                   radius = 2),
                                     enableMouseTracking = FALSE,
                                     includeInDataExport = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     showInLegend = FALSE) %>%
                       
                       ## Add scatter series of msoas
                       hc_add_series(data = hle_singlehouseholds_chart_data(),
                                     name = "Life expectancy (women)",
                                     type = "scatter",
                                     jitter = list(x = 0.25,
                                                   y = 0),
                                     hcaes(x = index - 1,
                                           y = `Female life expectancy (2016-2020 based)`,
                                           name = msoa11hclnm,
                                           color = colour),
                                     marker = list(symbol = "circle", 
                                                   radius = 3),
                                     tooltip = list(headerFormat = "<span><b>{point.key}</b> <br>",
                                                    pointFormat = "{series.name}: <b>{point.y:.1f} years"),
                                     states = list(inactive = list(enabled = FALSE)),
                                     showInLegend = FALSE) %>%
                       
                       ## Add scatter series of selected MSOA
                       hc_add_series(data = selected_msoa_df(),
                                     dataLabels = list(enabled = TRUE,
                                                       allowOverlap = TRUE,
                                                       format = '{point.label}',
                                                       style = list(color = black,
                                                                    fontSize = "0.6rem")),
                                     name = "Life expectancy (women)",
                                     type = "scatter",
                                     jitter = list(x = 0.25,
                                                   y = 0),
                                     hcaes(x = index - 1,
                                           y = `Female life expectancy (2016-2020 based)`,
                                           name = msoa11hclnm,
                                           color = colour,
                                           label = label),
                                     marker = list(symbol = "circle", 
                                                   radius = 5),
                                     tooltip = list(headerFormat = "<span><b>{point.key}</b> <br>",
                                                    pointFormat = "{series.name}: <b>{point.y:.1f} years"),
                                     includeInDataExport = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     showInLegend = FALSE) %>%
                       
                       hc_tooltip(style = list(fontSize = "0.6rem")) %>%
                       
                       hc_add_event_point(series = "series",
                                          event = "click") %>% 
                       
                       hc_exporting(enabled = TRUE,
                                    buttons = list(contextButton = list(menuItems = list('downloadSVG',
                                                                                         'downloadPNG',
                                                                                         'downloadCSV'))),
                                    chartOptions = list(title = list(text = hle_singlehouseholds_title(),
                                                                     align = "left",
                                                                     style = list(fontSize = "18px")),
                                                        caption = list(text = ""),
                                                        chart = list(width = 800,
                                                                     height = 600,
                                                                     backgroundColor = "white",
                                                                     style = list(fontSize = "14px"))))
                     
                   }
                   
                   
                 })
                 
                 
               })
  
  
}