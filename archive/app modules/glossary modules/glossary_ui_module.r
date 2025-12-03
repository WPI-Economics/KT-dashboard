library(shiny)

## Create a module for glossary tab ui

glossary_ui <- function(id){
  
  tabPanel(title = "Glossary",
           
           ## General terminology - glossary
           fluidRow(
             box(id = NS(namespace = id,
                         id = "glossary_general_box"),
                 
                 title = "General terminology",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_general_copy")))),
           
           ## Health inequalities - glossary
           fluidRow(
             box(id = NS(namespace = id,
                         id = "glossary_health_box"),
                 
                 title = "Health inequalities",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_health_copy")))),
           
           ## Work - glossary
           fluidRow( 
             box(id = NS(namespace = id,
                         id = "glossary_work_box"),
                 
                 title = "Work",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_work_copy")))),
           
           ## Money - glossary
           fluidRow( 
             box(id = NS(namespace = id,
                         id = "glossary_money_box"),
                 
                 title = "Money",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_money_copy")))),
           
           ## Housing - glossary
           fluidRow( 
             box(id = NS(namespace = id,
                         id = "glossary_housing_box"),
                 
                 title = "Housing",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_housing_copy")))),
           
           ## Transport - glossary
           fluidRow( 
             box(id = NS(namespace = id,
                         id = "glossary_transport_box"),
                 
                 title = "Transport",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_transport_copy")))),
           
           ## Education - glossary
           fluidRow( 
             box(id = NS(namespace = id,
                         id = "glossary_education_box"),
                 
                 title = "Education",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_education_copy")))),
           
           ## Community - glossary
           fluidRow( 
             box(id = NS(namespace = id,
                         id = "glossary_community_box"),
                 
                 title = "Community",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_community_copy")))),
           
           ## Surroundings - glossary
           fluidRow( 
             box(id = NS(namespace = id,
                         id = "glossary_surroundings_box"),
                 
                 title = "Surroundings",
                 
                 solidHeader = TRUE,
                 
                 width = 12,
                 
                 uiOutput(outputId = NS(namespace = id,
                                        id = "glossary_surroundings_copy")))))
}