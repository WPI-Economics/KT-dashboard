library(shiny)

## Create a module for community ui

community_ui <- function(id){
  tabPanel(title = "Community",
           
           ## Work Stats Row
           fluidRow(id = NS(namespace = id,
                            id = "keystat_row"),
                    
                    column(width = 12,
                           
                           splitLayout(cellWidths = c("2%", "46%", "4%", "46%", "2%"),
                                       
                                       uiOutput(outputId = NS(namespace = id,
                                                              id = "buffer1")),
                                       
                                       box(class = "keystat_box",
                                           
                                           width = NULL,
                                           
                                           splitLayout(cellWidths = c("30%", "70%"),
                                                       
                                                       uiOutput(outputId = NS(namespace = id,
                                                                              id = "loneliness_stat"),
                                                                class = "keystat_highlight"),
                                                       
                                                       uiOutput(outputId = NS(namespace = id,
                                                                              id = "loneliness_text"),
                                                                class = "keystat"))),
                                       
                                       uiOutput(outputId = NS(namespace = id,
                                                              id = "buffer2")),
                                       
                                       box(class = "keystat_box",
                                           
                                           width = NULL,
                                           
                                           splitLayout(cellWidths = c("30%", "70%"),
                                                       
                                                       uiOutput(outputId = NS(namespace = id,
                                                                              id = "belonging_stat"),
                                                                class = "keystat_highlight"),
                                                       
                                                       uiOutput(outputId = NS(namespace = id,
                                                                              id = "belonging_text"),
                                                                class = "keystat"))),
                                       
                                       uiOutput(outputId = NS(namespace = id,
                                                              id = "buffer3"))),
                           
                           uiOutput(outputId = NS(namespace = id,
                                                  id = "buffer4"),
                                    style = "height: 1vh;"),
                           
                           splitLayout(cellWidths = c("2%", "46%", "4%", "46%", "2%"),
                                       
                                       uiOutput(outputId = NS(namespace = id,
                                                              id = "buffer5")),
                                       
                                       box(class = "keystat_box",
                                           
                                           width = NULL,
                                           
                                           splitLayout(cellWidths = c("30%", "70%"),
                                                       
                                                       uiOutput(outputId = NS(namespace = id,
                                                                              id = "chatting_stat"),
                                                                class = "keystat_highlight"),
                                                       
                                                       uiOutput(outputId = NS(namespace = id,
                                                                              id = "chatting_text"),
                                                                class = "keystat"))),
                                       
                                       uiOutput(outputId = NS(namespace = id,
                                                              id = "buffer6")),
                                       
                                       box(class = "keystat_box",
                                           
                                           width = NULL,
                                           
                                           splitLayout(cellWidths = c("30%", "70%"),
                                                       
                                                       uiOutput(outputId = NS(namespace = id,
                                                                              id = "support_stat"),
                                                                class = "keystat_highlight"),
                                                       
                                                       uiOutput(outputId = NS(namespace = id,
                                                                              id = "support_text"),
                                                                class = "keystat"))),
                                       
                                       uiOutput(outputId = NS(namespace = id,
                                                              id = "buffer7"))))),
           
           ## Transport Health Links Row
           fluidRow(id = NS(namespace = id,
                            id = "chart_row"),
                    column(id = NS(namespace = id,
                                   id = "hle_la_chart_column"),
                           width = 8,
                           
                           ## Box for health links chart
                           box(id = NS(namespace = id,
                                       id = "hle_chart_box"),
                               
                               title = textOutput(outputId = NS(namespace = id,
                                                                id = "hle_chart_title")),
                               
                               solidHeader = TRUE,
                               
                               width = NULL,
                               
                               highchartOutput(outputId = NS(namespace = id,
                                                             id = "hle_chart"),
                                               height = "500px"))),
                    
                    column(id = NS(namespace = id,
                                   id = "hle_chart_text_column"),
                           
                           width = 4,
                           
                           ## Radio Button Selector for Chart
                           box(id = NS(namespace = id,
                                       id = "hle_chart_selector"),
                               
                               title = span("Select an indicator",
                                            ## actionLink that gives a popup of the helpText
                                            tags$sup(actionLink(inputId = NS(namespace = id,
                                                                             id = "indicatorSelectInfo"),
                                                                style = "display: inline; font-size: 20px; color: #FFFFFF",
                                                                label = NULL, 
                                                                icon = icon(name = "circle-info",
                                                                            lib = "font-awesome")))),
                               
                               solidHeader = TRUE,
                               
                               width = NULL,
                               
                               selectizeInput(inputId = NS(namespace = id,
                                                         id = "chart_selector"),
                                            
                                            label = NULL,
                                            
                                            choices = community_chart_variables,
                                            
                                            selected = "Often/always feel lonely (%)")),
                           
                           box(class = "compstat_box",
                               
                               width = NULL,
                               
                               
                               splitLayout(cellWidths = c("34%", "66%"),
                                           
                                           uiOutput(outputId = NS(namespace = id,
                                                                  id = "max_stat"),
                                                    class = "compstat_highlight"),
                                           
                                           uiOutput(outputId = NS(namespace = id,
                                                                  id = "max_text"),
                                                    class = "compstat"))),
                           
                           box(class = "compstat_box",
                               
                               width = NULL,
                               
                               
                               splitLayout(cellWidths = c("34%", "66%"),
                                           
                                           uiOutput(outputId = NS(namespace = id,
                                                                  id = "min_stat"),
                                                    class = "compstat_highlight"),
                                           
                                           uiOutput(outputId = NS(namespace = id,
                                                                  id = "min_text"),
                                                    class = "compstat"))),
                           
                           box(class = "compstat_box",
                               
                               width = NULL,
                               
                               
                               splitLayout(cellWidths = c("34%", "66%"),
                                           
                                           uiOutput(outputId = NS(namespace = id,
                                                                  id = "rank_stat"),
                                                    class = "compstat_highlight"),
                                           
                                           uiOutput(outputId = NS(namespace = id,
                                                                  id = "rank_text"),
                                                    class = "compstat"))))),
           
           ## Life expectancy by single-person households row
           fluidRow(id = NS(namespace = id,
                            id = "map_row"),
                    
                    box(title = textOutput(outputId = NS(namespace = id,
                                                         id = "hle_singlehouseholds_title")),
                        
                        solidHeader = TRUE,
                        
                        width = 12,
                        
                        splitLayout(id = NS(namespace = id,
                                            id = "hle_singlehouseholds_box"),
                                    
                                    cellWidths = c("36%", "64%"),
                                    
                                    highchartOutput(outputId = NS(namespace = id,
                                                                  id = "hle_singlehouseholds_map")),
                                    
                                    highchartOutput(outputId = NS(namespace = id,
                                                                  id = "hle_singlehouseholds_chart"))),
                        
                        uiOutput(outputId = NS(namespace = id,
                                               id = "legend")))))
  
}
