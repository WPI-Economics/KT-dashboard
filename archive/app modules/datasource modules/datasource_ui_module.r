library(shiny)

## Create a module for datasource tab ui

datasource_ui <- function(id){
  
  tabPanel(title = "Sources",
           
           ## Health inequalities - data sources
           fluidRow(
             box(id = NS(namespace = id,
                         id = "datasource_health_box"),
                 
                 title = "Inequalities",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "datasource_health_copy")))),
           
           ## Work - data sources
           fluidRow(
             box(id = NS(namespace = id,
                         id = "datasource_work_box"),
                 
                 title = "Work",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "datasource_work_copy")))),
           
           ## Money - data source
           fluidRow(
             box(id = NS(namespace = id,
                         id = "datasource_money_box"),
                 
                 title = "Money",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "datasource_money_copy")))),
           
           ## Housing - data source
           fluidRow(
             box(id = NS(namespace = id,
                         id = "datasource_housing_box"),
                 
                 title = "Housing",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "datasource_housing_copy")))),
           
           ## Transport - data source
           fluidRow(
             box(id = NS(namespace = id,
                         id = "datasource_transport_box"),
                 
                 title = "Transport",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "datasource_transport_copy")))),
           
           ## Education - data source
           fluidRow(
             box(id = NS(namespace = id,
                         id = "datasource_education_box"),
                 
                 title = "Education",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "datasource_education_copy")))),
           
           ## Community - data source
           fluidRow(
             box(id = NS(namespace = id,
                         id = "datasource_community_box"),
                 
                 title = "Community",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "datasource_community_copy")))),
           
           ## Surroundings - data source
           fluidRow(
             box(id = NS(namespace = id,
                         id = "datasource_surroundings_box"),
                 
                 title = "Surroundings",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "datasource_surroundings_copy")))))
}