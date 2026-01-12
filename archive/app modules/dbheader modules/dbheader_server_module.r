library(shiny)
library(tidyverse)

## Create a module for dashboard header server

dbheader_server <- function(id){
  
  moduleServer(id = id, 
               module = function(input, output, session){
                 
                 ## Create a text output using the select input
                 
                 output$title_text <- renderUI({
                   
                   #original header HTML("<div>Local authority dashboard: What builds good health?</div>")
                   HTML('<div>ECONOMIC VALUE:<br> CALCULATOR 2025</div>') #replacement
                 })
                 
                 ## Buffer
                 output$title_buffer <- renderUI({
                   
                 })
                 
               })
  
}