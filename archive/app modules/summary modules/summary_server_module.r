library(shiny)
library(tidyverse)

## Create a module for education tab server

education_server <- function(id, local_authority, gender){
  
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
                 
                 
                 ## No qualifications stat
                 noquals_stat <- reactive({
                   
                   education_data_laua %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(`Residents with no qualifications (%)`) %>% 
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 output$noquals_stat <- renderUI({
                   
                   HTML(str_c(str_replace_na(string = noquals_stat(),
                                             replacement = "-"),
                              '%'))
                   
                 })
                 
                 output$noquals_text <- renderUI({
                   
                   HTML(str_c('of people in <b>',
                              local_authority(),
                              '</b> have no qualifications'))
                   
                 })
                 
                 ## Free school meals stat
                 fsm_stat <- reactive({
                   
                   education_data_laua %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(`Free school meals (%)`) %>% 
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 output$fsm_stat <- renderUI({
                   
                   HTML(str_c(str_replace_na(string = fsm_stat(),
                                             replacement = "-"),
                              '%'))
                   
                 })
                 
                 output$fsm_text <- renderUI({
                   
                   HTML(str_c('of pupils in <b>',
                              local_authority(),
                              '</b> receive free school meals'))
                   
                 })
                 
                 ## Special Educational Needs stat
                 sen_stat <- reactive({
                   
                   education_data_laua %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(`SEN support or EHC plan (%)`) %>% 
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 output$sen_stat <- renderUI({
                   
                   HTML(str_c(str_replace_na(string = sen_stat(),
                                             replacement = "-"),
                              '%'))
                 })
                 
                 output$sen_text <- renderUI({
                   
                   HTML(str_c('of pupils in <b>',
                              local_authority(),
                              '</b> have SEN support or an EHC plan'))
                   
                 })
                 
                 ## Persistent absence stat
                 absence_stat <- reactive({
                   
                   education_data_laua %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(`Pupils persistently absent (%)`) %>% 
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 output$absence_stat <- renderUI({
                   
                   HTML(str_c(str_replace_na(string = absence_stat(),
                                             replacement = "-"),
                              '%'))
                   
                 })
                 
                 output$absence_text <- renderUI({
                   
                   HTML(str_c('of pupils in <b>',
                              local_authority(),
                              '</b> miss at least 10% of their school classes'))
                   
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
                 
                 ## Education tab - Highest qualifications Box Title
                 
                 output$highestqual_title <- renderText({
                   
                   str_c("What is the highest qualification level of residents in ",
                         local_authority(),
                         "?")
                   
                 })
                 
                 ## Under 50 Chart
                 
                 under50_chart_data <- reactive({
                   
                   under50_df %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     drop_na()
                   
                 })
                 
                 output$under50_chart <- renderHighchart({
                   
                   highchart() %>%
                     
                     hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE, 
                                                                 inside = FALSE, 
                                                                 format = "{y:.1f}%",
                                                                 overflow = "allow",
                                                                 crop = FALSE,
                                                                 x = -5,
                                                                 style = list(fontWeight = 400,
                                                                              fontSize = "0.5rem")),
                                               borderRadius = 5)) %>% 
                     
                     hc_title(text = "Highest level of qualification (Aged < 50 years old)",
                              align = "left",
                              style = list(color = blue, 
                                           fontWeight = "400",
                                           fontSize = "0.6rem")) %>% 
                     
                     hc_xAxis(categories = under50_chart_data()$`Qualification level`,
                              labels = list(style = list(textOverflow = "none",
                                                         whiteSpace = "nowrap",
                                                         fontSize = "0.5rem")),
                              title = list(text = "")) %>%
                     
                     hc_yAxis(visible = FALSE,
                              max = max(under50_chart_data()$`People (%)`) + 2.5) %>% 
                     
                     hc_add_series(data = under50_chart_data(),
                                   name = "Qualification",
                                   type = "bar",
                                   hcaes(x = `Qualification level`,
                                         y = `People (%)`,
                                         name = `Qualification level`),
                                   borderRadius = 0,
                                   showInLegend = FALSE) %>%
                     
                     hc_tooltip(headerFormat = str_c("<b>",
                                                     local_authority(),
                                                     "</b><br>"),
                                pointFormat = "{point.name}: <b>{point.y:.1f}%</b>",
                                style = list(fontSize = "0.6rem")) %>%
                     
                     hc_colors(light_blue) %>% 
                     
                     hc_exporting(enabled = TRUE,
                                  buttons = list(contextButton = list(menuItems = list('downloadSVG',
                                                                                       'downloadPNG',
                                                                                       'downloadCSV'))),
                                  chartOptions = list(caption = list(text = ""),
                                                      chart = list(width = 800,
                                                                   height = 600,
                                                                   backgroundColor = "white",
                                                                   style = list(fontSize = "14px"))))
                 })
                 
                 ## Over50 Chart
                 
                 over50_chart_data <- reactive({
                   
                   over50_df %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     drop_na()
                   
                 })
                 
                 output$over50_chart <- renderHighchart({
                   
                   highchart() %>%
                     
                     hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE, 
                                                                 inside = FALSE, 
                                                                 format = "{y:.1f}%",
                                                                 overflow = "allow",
                                                                 crop = FALSE,
                                                                 x = -5,
                                                                 style = list(fontWeight = 400,
                                                                              fontSize = "0.5rem")),
                                               borderRadius = 5)) %>% 
                     
                     hc_title(text = "Highest level of qualification (Aged 50+ years old)",
                              align = "left",
                              style = list(color = blue, 
                                           fontWeight = "400",
                                           fontSize = "0.6rem")) %>% 
                     
                     hc_xAxis(categories = over50_chart_data()$`Qualification level`,
                              labels = list(style = list(textOverflow = "none",
                                                         whiteSpace = "nowrap",
                                                         fontSize = "0.5rem")),
                              title = list(text = "")) %>%
                     
                     hc_yAxis(visible = FALSE,
                              max = max(over50_chart_data()$`People (%)`) + 2.5) %>% 
                     
                     hc_add_series(data = over50_chart_data(),
                                   name = "Qualification",
                                   type = "bar",
                                   hcaes(x = `Qualification level`,
                                         y = `People (%)`,
                                         name = `Qualification level`),
                                   borderRadius = 0,
                                   showInLegend = FALSE) %>%
                     
                     hc_tooltip(headerFormat = str_c("<b>",
                                                     local_authority(),
                                                     "</b><br>"),
                                pointFormat = "{point.name}: <b>{point.y:.1f}%</b>",
                                style = list(fontSize = "0.6rem")) %>%
                     
                     hc_colors(light_blue) %>% 
                     
                     hc_exporting(enabled = TRUE,
                                  buttons = list(contextButton = list(menuItems = list('downloadSVG',
                                                                                       'downloadPNG',
                                                                                       'downloadCSV'))),
                                  chartOptions = list(caption = list(text = ""),
                                                      chart = list(width = 800,
                                                                   height = 600,
                                                                   backgroundColor = "white",
                                                                   style = list(fontSize = "14px"))))
                 })
                 
                 
                 ## Healthy life expectancy scatter chart data
                 
                 hle_chart_data <- reactive({
                   
                   if(gender() == "Men"){
                     
                     education_data_laua %>%  
                       select(`Local authority name`,
                              "indicator" = .data[[input$chart_selector]],
                              `Healthy Life Expectancy (HLE): 2021-23 (Males)`,
                              colour) %>% 
                       mutate(colour = case_when(`Local authority name` == local_authority() ~ mid_red,
                                                 TRUE ~ colour)) %>%
                       drop_na(indicator,
                               `Healthy Life Expectancy (HLE): 2021-23 (Males)`)
                     
                   } else if(gender() == "Women"){
                     
                     education_data_laua %>%  
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
                   
                   if(input$chart_selector == education_chart_variables[1]){
                     "residents with no qualifications"
                   } else if(input$chart_selector == education_chart_variables[2]){
                     "pupils attaining Level 2 qualifications (e.g. GCSE grades 9-4/A*-C) by age 19"
                   } else if(input$chart_selector == education_chart_variables[3]){
                     "pupils attaining Level 3 qualifications (e.g. AS/A-Level) by age 19"
                   } else if(input$chart_selector == education_chart_variables[4]){
                     "residents with Level 4+ qualifications"
                   } else if(input$chart_selector == education_chart_variables[5]){
                     "pupils progressing to Level 4+ (e.g. undergraduate degree) study"
                   } else if(input$chart_selector == education_chart_variables[6]){
                     "pupils persistently absent (miss 10% of their school classes)"
                   }
                   
                 })
                 
                 hle_chart_title <- reactive(
                   str_c("Healthy life expectancy for ",
                         str_to_lower(gender()),
                         " at birth (2021–23) by ",
                         hle_chart_indicator(),
                         ": England")
                 )
                 
                 output$hle_chart_title <- renderText({
                   
                   hle_chart_title()
                   
                 })
                 
                 ## Healthy life expectancy scatter chart
                 
                 hle_chart_xAxis <- reactive({
                   
                   if(input$chart_selector == education_chart_variables[1]){
                     "Residents with no qualifications"
                   } else if(input$chart_selector == education_chart_variables[2]){
                     "Pupils attaining Level 2 qualifications (e.g. GCSE grades 9-4/A*-C) by age 19"
                   } else if(input$chart_selector == education_chart_variables[3]){
                     "Pupils attaining Level 3 qualifications (e.g. AS/A-Level) by age 19"
                   } else if(input$chart_selector == education_chart_variables[4]){
                     "Residents with Level 4+ qualifications"
                   } else if(input$chart_selector == education_chart_variables[5]){
                     "Pupils progressing to Level 4+ (e.g. undergraduate degree) study"
                   } else if(input$chart_selector == education_chart_variables[6]){
                     "Pupils persistently absent (miss 10% of their school classes)"
                   }
                   
                 })
                 
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
                       
                       hc_xAxis(title = list(text = hle_chart_xAxis(),
                                             style = list(fontSize = "0.6rem")),
                                labels = list(format = "{value:.0f}%",
                                              style = list(fontSize = "0.5rem")),
                                min = min(hle_chart_data()$indicator,
                                          na.rm = TRUE),
                                max = max(hle_chart_data()$indicator,
                                          na.rm = TRUE),
                                plotLines = list(list(color = light_blue,
                                                      dashStyle = "dash",
                                                      value = education_england_data[[input$chart_selector]],
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
                                                      value = education_england_data$`Healthy Life Expectancy (HLE): 2021-23 (Females)`,
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
                                                      hle_chart_xAxis(),
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
                       
                       hc_xAxis(title = list(text = hle_chart_xAxis(),
                                             style = list(fontSize = "0.6rem")),
                                labels = list(format = "{value:.0f}%",
                                              style = list(fontSize = "0.5rem")),
                                min = min(hle_chart_data()$indicator,
                                          na.rm = TRUE),
                                max = max(hle_chart_data()$indicator,
                                          na.rm = TRUE),
                                plotLines = list(list(color = light_blue,
                                                      dashStyle = "dash",
                                                      value = education_england_data[[input$chart_selector]],
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
                                                      value = education_england_data$`Healthy Life Expectancy (HLE): 2021-23 (Males)`,
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
                                                      hle_chart_xAxis(),
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
                   
                   education_data_laua %>% 
                     slice_max(.data[[input$chart_selector]],
                               n = 1, 
                               with_ties = FALSE) %>% 
                     pull(`Local authority name`)
                   
                 })
                 
                 indicator_max_value <- reactive({
                   
                   education_data_laua %>%
                     filter(`Local authority name` == indicator_max_la()) %>%
                     pull(input$chart_selector) %>%
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 indicator_min_la <- reactive({
                   
                   education_data_laua %>% 
                     slice_min(.data[[input$chart_selector]],
                               n = 1, 
                               with_ties = FALSE) %>% 
                     pull(`Local authority name`)
                   
                 })
                 
                 indicator_min_value <- reactive({
                   
                   education_data_laua %>%
                     filter(`Local authority name` == indicator_min_la()) %>%
                     pull(input$chart_selector) %>%
                     round(x = .,
                           digits = 1)
                   
                 })
                 
                 indicator_rank <- reactive({
                   
                   education_data_laua %>%
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
                              '</b> has the most ',
                              hle_chart_indicator()))
                   
                 })
                 
                 output$min_stat <- renderUI({
                   
                   HTML(str_c(indicator_min_value(),
                              '%'))
                   
                 })
                 
                 output$min_text <- renderUI({
                   
                   HTML(str_c('<b>',
                              indicator_min_la(),
                              '</b> has the fewest ',
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
                   
                   HTML(str_c(hle_chart_xAxis(),
                              ' rank of <b>',
                              local_authority(),
                              '</b>'))
                   
                 })
                 
                 ## HLE by Qualifications Title
                 
                 hle_quals_title <- reactive(
                   str_c("Residents with Level 3+ qualifications (e.g. AS/A-Level) by life expectancy: neighbourhoods (MSOAs) in ",
                         local_authority())
                 )
                 
                 output$hle_quals_title <- renderText({
                   hle_quals_title()
                 })
                 
                 ## HLE by Qualifications Legend
                 
                 output$legend <- renderUI({
                   
                   legend <- tags$ul(class = "jointlegend")
                   colors <- rev(map5)
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
                       HTML("Fewest Level 3+ qualifications")
                     ),
                     legend$children,
                     tags$li(
                       class = "legend-item legend-label right-label",
                       HTML("Most Level 3+ qualifications")
                     )
                   )
                   tagList(
                     tags$span(class = "legend-title",
                               "England (March 2021)"),
                     legend
                   )
                   
                 })
                 
                 
                 
                 ## HLE by Qualifications Map
                 
                 output$hle_quals_map <- renderHighchart({
                   
                   msoa_codes <- education_data_msoa %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     pull(MSOA11CD)
                   
                   hle_map <- highchart(type = "map") %>% 
                     
                     hc_title(text = "") %>% 
                     
                     hc_colorAxis(dataClasses = list(list(color = map5[5],
                                                          from = 0.5,
                                                          to = 2.5,
                                                          name = "1st quintile"),
                                                     list(color = map5[4],
                                                          from = 2.5,
                                                          to = 4.5,
                                                          name = "2nd quintile"),
                                                     list(color = map5[3],
                                                          from = 4.5,
                                                          to = 6.5,
                                                          name = "3rd quintile"),
                                                     list(color = map5[2],
                                                          from = 6.5,
                                                          to = 8.5,
                                                          name = "4th quintile"),
                                                     list(color = map5[1],
                                                          from = 8.5,
                                                          to = 10.5,
                                                          name = "5th quintile"))) %>%
                     
                     hc_legend(enabled = FALSE) %>% 
                     
                     hc_mapView(projection = list(name = "WebMercator")) %>% 
                     
                     hc_add_event_series(event = "click")
                   
                   if(gender() == "Men"){
                     
                     df <- education_data_msoa %>% 
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
                                               Level 3+ qualifications decile: {point.value:.0f}",
                                  nullFormat = "<b>{point.msoa11hclnm}</b><br>
                                               Life expectancy (men): {point.le:.1f} years<br>
                                               Level 3+ qualifications decile: NA")
                     
                   } else if(gender() == "Women"){
                     
                     df <- education_data_msoa %>% 
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
                                               Level 3+ qualifications decile: {point.value:.0f}",
                                  nullFormat = "<b>{point.msoa11hclnm}</b><br>
                                               Life expectancy (women): {point.value:.1f} years<br>
                                               Level 3+ qualifications decile: NA")
                   }
                   
                 })
                 
                 
                 
                 ## Create a reactive for clicking between map and chart
                 
                 selected_msoa <- reactiveVal(value = "")
                 
                 observeEvent(eventExpr = input$hle_quals_map_click$name,
                              handlerExpr = {
                                
                                selected_msoa(input$hle_quals_map_click$name)
                                
                              })
                 
                 observeEvent(eventExpr = input$hle_quals_chart_click$name,
                              handlerExpr = {
                                
                                selected_msoa(input$hle_quals_chart_click$name)
                                
                              })
                 
                 # Create a dataframe for series based on clicked MSOA
                 selected_msoa_df <- reactive({
                   
                   if(is.null(selected_msoa()) == TRUE){
                     education_data_msoa %>% 
                       filter(`MSOA code` == "drop")
                     
                   } else{
                     
                     education_data_msoa %>% 
                       filter(`Local authority name` == local_authority()) %>% 
                       mutate(colour = case_when(msoa11hclnm == selected_msoa() ~ mid_red,
                                                        TRUE ~ rgb(0,0,0,0)),
                              label = case_when(msoa11hclnm == selected_msoa() ~ msoa11hclnm,
                                                TRUE ~ "")) %>% 
                       arrange(`index`)
                     
                   }
                   
                 })
                 
                 
                 ## HLE by Qualifications Chart
                 
                 hle_quals_chart_data<- reactive({
                   
                   education_data_msoa %>% 
                     filter(`Local authority name` == local_authority()) %>% 
                     filter(msoa11hclnm != selected_msoa())
                   
                 })
                 
                 
                 output$hle_quals_chart <- renderHighchart({
                   
                   if(gender() == "Men"){
                     
                     highchart() %>%
                       
                       hc_plotOptions(scatter = list(animation = FALSE)) %>% 
                       
                       hc_xAxis(title = list(text = "Level 3+ qualifications decile",
                                             style = list(fontSize = "0.6rem")),
                                categories = unique(education_data_msoa$`Level 3+ decile`),
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
                                                      value = median(x = education_data_msoa$`Male life expectancy (2016-2020 based)`, 
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
                       hc_add_series(data = education_greys,
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
                                     includeInDateExport = FALSE,
                                     states = list(inactive = list(enabled = FALSE)),
                                     showInLegend = FALSE) %>%
                       
                       ## Add scatter series of msoas
                       hc_add_series(data = hle_quals_chart_data(),
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
                                    chartOptions = list(title = list(text = hle_quals_title(),
                                                                     align = "left",
                                                                     style = list(fontSize = "18px")),
                                                        caption = list(text = ""),
                                                        chart = list(width = 800,
                                                                     height = 600,
                                                                     backgroundColor = "white",
                                                                     style = list(fontSize = "14px"))))
                     
                   } else if(gender() == "Women"){
                     
                     highchart() %>%
                       
                       hc_plotOptions(column = list(dataLabels = list(align = "top", 
                                                                      rotation = 270, 
                                                                      y = -5))) %>% 
                       
                       hc_xAxis(title = list(text = "Level 3+ qualifications decile",
                                             style = list(fontSize = "0.6rem")),
                                categories = unique(education_data_msoa$`Level 3+ decile`),
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
                                                      value = median(x = education_data_msoa$`Female life expectancy (2016-2020 based)`, 
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
                       hc_add_series(data = education_greys,
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
                       hc_add_series(data = hle_quals_chart_data(),
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
                                    chartOptions = list(title = list(text = hle_quals_title(),
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