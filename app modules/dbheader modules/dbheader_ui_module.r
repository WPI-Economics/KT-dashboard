library(shiny)

## Create a module for dashboard header ui

dbheader_ui <- function(id){
  
  fluidRow(id = NS(namespace = id,
                   id = "row"),
           
           box(id = NS(namespace = id,
                       id = "box"),
               
               width = 12,
               
               splitLayout(id = NS(namespace = id,
                                   id = "split_layout"),
                           
                           cellWidths = c("25%", "50%", "10%", "15%"),
                           
                           img(src = "WPI Economics_Logo_White.svg",
                               id = NS(namespace = id,
                                       id = "logo")),
                           
                           uiOutput(outputId = NS(namespace = id,
                                                    id = "title_text")),
                           
                           uiOutput(outputId = NS(namespace = id,
                                                  id = "title_buffer")),
                           
                           img(src = "KT_UK_Primary_White_RGB.svg",
                               height = "72px",
                               id = NS(namespace = id,
                                       id = "icon")))))
}
