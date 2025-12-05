library(tidyverse)
library(sf)
library(shiny)
library(shinydashboard)
library(waiter)
library(htmltools)
library(highcharter)
library(geojsonio)
library(bslib)
library(bsicons)

#set the default value box behaviour globally
# value_box_theme(
#   min_height = "auto",   # let height shrink to content
#   padding = "0.5rem"     # adjust internal padding
# )

# ui <- 
ui <- 
  page_fillable(
    tags$h1("ECONOMIC VALUE CALCULATOR 2025", id = "main-title"),
    page_navbar(
      title = "Dashboard Header",   # HEADER
      theme = bs_theme(version = 5),
      
      # ---- TAB summary ----
      nav_panel(
        "Summary",
        layout_sidebar(
          sidebar = sidebar(
            value_box("SMetric A", value = "321"),
            value_box("SMetric B", value = "654"),
            value_box("SMetric C", value = "987"),
            value_box("SMetric D", value = "202"),
            value_box("SMetric E", value = "303")
          ),
          layout_column_wrap(
            width = 1/1,
          
            value_box(title = "Summary total",value = "120,000", showcase = bs_icon("bar-chart"),theme = "red", fill = TRUE),
          
          
          card(fill = FALSE,
            card_header("CROSS SECTION CHART"),
            plotOutput("plot3")
            )
          )
        )
      ),
      
      # ---- TAB VALUE ----
      nav_panel(
        "£ value",
        layout_sidebar(
          sidebar = sidebar(
            value_box(title = "Economic value",value = "123bn", showcase = bs_icon("bar-chart"),
                      theme = "purple"),
            value_box(title = "Reduced re-offending",value = "123bn",showcase = bs_icon("bar-chart"),
                      theme = "yellow"),
            value_box(title = "DWP/Health admin",value = "123bn",showcase = bs_icon("bar-chart"),
                      theme = "red"),
            value_box(title = "Volunteers",value = "123bn",showcase = bs_icon("bar-chart"),
                      theme = "orange"),
            value_box(title = "Wellbeing",value = "123bn",showcase = bs_icon("bar-chart"),theme = "grey")
          ),
          # Main body
          layout_column_wrap(
            width = 1/1,
            
              value_box(title = "Total Social Return on Investment",value = "£3.8bn", showcase = bs_icon("bar-chart"),theme = "blue", fill = TRUE),
            
          card(fill = FALSE,
            card_header("Chart"),
            plotOutput("plot1")
            )
          )
        )
      ),
      
      # ---- TAB PEOPLE ----
      nav_panel(
        "People",
        layout_sidebar(
          sidebar = sidebar(
            value_box("Metric A", value = "321"),
            value_box("Metric B", value = "654"),
            value_box("Metric C", value = "987"),
            value_box("Metric D", value = "202"),
            value_box("Metric E", value = "303")
          ),
          layout_column_wrap(
            width = 1/1,
            
            value_box(title = "People total",value = "120,000", showcase = bs_icon("bar-chart"),theme = "blue", fill = TRUE),
                      
          card(fill = FALSE,
            card_header("Chart"),
            plotOutput("plot2")
            )
          )
        )
      ),
      
      # ---- TAB glossary ----
      nav_panel(
        "Glossary",
        card(
          card_header("Glossary"),
          card_body("Glossary content goes here")
        )
      ),
      
      # ---- TAB data sources ----
      nav_panel(
        "Data Sources",
        card(
          card_header("Data Sources"),
          card_body("List of sources, links, etc.")
        )
      )
    ))

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    plot(cars)
  })
  output$plot2 <- renderPlot({
    plot(pressure)
  })
  output$plot3 <- renderPlot({
    hist(mtcars$mpg)
  })
}

shinyApp(ui, server)
