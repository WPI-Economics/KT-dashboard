library(tidyverse)
library(sf)
library(shiny)
library(shinydashboard)
library(waiter)
library(htmltools)
library(highcharter)
library(geojsonio)


### Read in modules

#### CSS/Themes
source("3. plots/thf_colours.r")

## Data Module
source("app modules/data_module.r")

#### Dashboard Header Modules
source("app modules/dbheader modules/dbheader_ui_module.r")
source("app modules/dbheader modules/dbheader_server_module.r")

#### Health Modules
source("app modules/health modules/health_modules.r")

#### Work Modules
source("app modules/work modules/work_modules.r")

#### Money Modules
source("app modules/money modules/money_modules.r")

#### Housing Modules
source("app modules/housing modules/housing_modules.r")

#### Transport Modules
source("app modules/transport modules/transport_modules.r")

#### Education Modules
source("app modules/education modules/education_modules.r")

#### Community Modules
source("app modules/community modules/community_modules.r")

#### Surroundings Modules
source("app modules/surroundings modules/surroundings_modules.r")

#### About Modules
source("app modules/datasource modules/datasource_modules.r")
source("app modules/glossary modules/glossary_modules.r")




### Create UI

ui <- dashboardPage(
  
  ## Disabled Dashboard Header
  dashboardHeader(disable = TRUE),
  
  ## Disabled Dashboard Sidebar
  dashboardSidebar(disable = TRUE),
  
  ## Content of Dashboard Body
  dashboardBody(id = "dbbody",
                
                useWaiter(),
                
                waiterPreloader(html = tagList(
                  spin_loaders(34, color = salmon_red),
                  
                  h4("Dashboard loading..."))),
                
                autoWaiter(id = c("intro_text",
                                  "health-le_stat",
                                  "health-hle_stat",
                                  "health-hle_la_chart",
                                  "health-max_stat",
                                  "health-min_stat",
                                  "health-hle_imd_chart",
                                  "health-hle_imd_map",
                                  "work-employment_stat",
                                  "work-unemployment_stat",
                                  "work-inactive_sick_stat",
                                  "work-inactive_other_stat",
                                  "work-industries_chart",
                                  "work-occupations_chart",
                                  "work-hle_chart",
                                  "work-max_stat",
                                  "work-min_stat",
                                  "work-hle_uc_map",
                                  "work-hle_uc_chart",
                                  "money-income_stat",
                                  "money-child_poverty_stat",
                                  "money-low_pay_stat",
                                  "money-benefits_stat",
                                  "money-hle_chart",
                                  "money-max_stat",
                                  "money-min_stat",
                                  "money-hle_income_map",
                                  "money-hle_income_chart",
                                  "housing-rental_stat",
                                  "housing-rent_pct_stat",
                                  "housing-nondecent_social_stat",
                                  "housing-nondecent_private_stat",
                                  "housing-hle_chart",
                                  "housing-max_stat",
                                  "housing-min_stat",
                                  "housing-hle_houdep_map",
                                  "housing-hle_houdep_chart",
                                  "transport-active_stat",
                                  "transport-nondriving_stat",
                                  "transport-distance_stat",
                                  "transport-fatalities_stat",
                                  "transport-hle_chart",
                                  "transport-max_stat",
                                  "transport-min_stat",
                                  "transport-hle_active_map",
                                  "transport-hle_active_chart",
                                  "education-noquals_stat",
                                  "education-fsm_stat",
                                  "education-sen_stat",
                                  "education-absence_stat",
                                  "education-under45_chart",
                                  "education-over45_chart",
                                  "education-hle_chart",
                                  "education-max_stat",
                                  "education-min_stat",
                                  "education-hle_quals_map",
                                  "education-hle_quals_chart",
                                  "community-loneliness_stat",
                                  "community-belonging_stat",
                                  "community-chatting_stat",
                                  "community-support_stat",
                                  "community-hle_chart",
                                  "community-max_stat",
                                  "community-min_stat",
                                  "community-hle_singlehouseholds_map",
                                  "community-hle_singlehouseholds_chart",
                                  "surroundings-airquality_stat",
                                  "surroundings-crime_stat",
                                  "surroundings-greenspace_stat",
                                  "surroundings-harms_stat",
                                  "surroundings-hle_chart",
                                  "surroundings-max_stat",
                                  "surroundings-min_stat",
                                  "surroundings-hle_greenspace_map",
                                  "surroundings-hle_greenspace_chart"),
                           html = spin_loaders(34, color = salmon_red)),
                
                ## Link to CSS file and google analytics
                tags$head(
                  includeHTML("www/google_tag_manager_head.html"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
                
                tags$body(
                  includeHTML("www/google_tag_manager_body.html")),
                
                ## Box for Feedback survey
                uiOutput(outputId = "survey_box"),
                
                ## Dashboard Header Row
                dbheader_ui(id = "dbheader"),
                
                ## Intro Text and Selector Row
                fluidRow(id = "selector_row",
                         ## Box for Topic Intro text
                         column(width = 6,
                                
                                box(class = "intro_row",
                                    
                                    title = "Introduction",
                                    
                                    solidHeader = TRUE,
                                    
                                    width = NULL,
                                    
                                    uiOutput(outputId = "intro_text"))),
                         
                         ## Local authority selector
                         column(width = 6,
                                
                                box(class = "intro_row",
                                    
                                    id = "selector_box",
                                    
                                    title = "Filter the data here:",
                                    
                                    solidHeader = TRUE,
                                    
                                    width = NULL,
                                    
                                    selectizeInput(inputId = "local_authority",
                                                   label = "Choose a local authority to explore",
                                                   choices = health_data_laua$`Local authority name`,
                                                   options = list(placeholder = "Type or select here",
                                                                  onInitialize = I('function() { this.setValue(""); }'))),
                                    
                                    uiOutput(outputId = "buffer"),
                                    
                                    radioButtons(inputId = "gender",
                                                 label = span("Life expectancy measures for:",
                                                              ## actionLink that gives a popup of the helpText
                                                              tags$sup(actionLink(inputId = "sexSelectInfo",
                                                                                  style = "display: inline; font-size: 12px; color: #000000",
                                                                                  label = NULL,
                                                                                  icon = icon(name = "circle-info",
                                                                                              lib = "font-awesome")))),
                                                 inline = TRUE,
                                                 choices = c("Women", "Men"),
                                                 selected = "Women")))),
                
                ## Dashboard Body Tabs
                fluidRow(id = "content_row",
                         tabBox(id = "tabset",
                                
                                width = 12,
                                
                                
                                health_ui(id = "health"),
                                
                                work_ui(id = "work"),
                                
                                money_ui(id = "money"),
                                
                                housing_ui(id = "housing"),
                                
                                transport_ui(id = "transport"),
                                
                                education_ui(id = "education"),
                                
                                community_ui(id = "community"),
                                
                                surroundings_ui(id = "surroundings"),
                                
                                datasource_ui(id = "datasource"),
                                
                                glossary_ui(id = "glossary")))))


### Create server

server <- function(input, output, session, local_authority) {
  
  ## Survey box
  output$survey_box <- renderUI({
    HTML('<span>BETA:</span> This is a new tool – your feedback will help us improve it')
  })
  
  ## Modal for sex information box
  observeEvent(eventExpr = input$sexSelectInfo, 
               handlerExpr = {
                 showModal(modalDialog(
                   title = NULL,
                   "This will change the sex that life expectancy or healthy life expectancy measures refer to for all charts and statistics across all tabs",
                   easyClose = TRUE,
                   footer = NULL,
                   size = "m"))
               })
  
  ## Dashboard Header
  
  dbheader_server(id = "dbheader")
  
  ## Buffer for selectors
  
  output$buffer <- renderUI({
    
  })
  
  ## Selected Local Authority Reactive
  selected <- reactive({
    
    if(input$local_authority == ""){
      health_data_laua$`Local authority name`[[1]]
    } else {
      input$local_authority
    }
  })
  
  sex <- reactive({
    input$gender
  })
  
  ## Intro Text based on Selected Tab
  output$intro_text <- renderUI({
    if(input$tabset == "Inequalities"){
      
      HTML(str_c('Good health is important for both individuals and society as a whole, but not everyone has the same opportunities for good health.<br><br>Research has shown that differences in social and economic circumstances can lead to deep inequalities in health outcomes.<br><br>Click here for more analysis and insight on <a style="color: #dd0031; font-weight: 600" href="https://www.health.org.uk/evidence-hub/health-inequalities" target="_blank" rel="noopener noreferrer">health inequalities.</a>'))
      
    } else if(input$tabset == "Work"){
      
      HTML(str_c('Employment, or the lack of it, can have considerable influence on health and wellbeing. Poor health can limit people’s ability to have and sustain work.<br><br>The nature of people’s work matters for health, but also impacts other factors that influence health, such as having sufficient income and forming social connections.<br><br>Click here for more analysis and insight on how <a style="color: #dd0031; font-weight: 600" href="https://www.health.org.uk/evidence-hub/work" target="_blank" rel="noopener noreferrer">work can affect health and health inequalities.</a>'))
      
    } else if(input$tabset == "Money"){
      
      HTML(str_c('There is a well-established link between money and resources – such as income or wealth – and variations in health. We all need financial resources for most areas of life, including those that influence our health. Poverty – and persistent poverty especially – is associated with worse health.<br><br>Click here for more analysis and insight on how <a style="color: #dd0031; font-weight: 600" href="https://www.health.org.uk/evidence-hub/money-and-resources" target="_blank" rel="noopener noreferrer">money and resources can affect health and health inequalities.</a>'))
      
    } else if(input$tabset == "Housing"){
      
      HTML(str_c('A stable and secure home is one of the foundations of a good life. The condition and nature of homes, including factors such as stability, space, tenure, quality and cost, can have a big impact on people’s lives, influencing their wellbeing and health.<br><br>Click here for more analysis and insight on how <a style="color: #dd0031; font-weight: 600" href="https://www.health.org.uk/evidence-hub/housing" target="_blank" rel="noopener noreferrer">housing can affect health and health inequalities.</a>'))
      
    } else if(input$tabset == "Transport"){
      
      HTML(str_c('Transport can affect health directly: there are health benefits linked to active travel (walking and cycling as part of routine travel) and negative health impacts linked to air pollution.<br><br>It can also affect health indirectly through supporting other building blocks of good health, such as providing access to public services and someone’s place of work.<br><br>Click here for more analysis and insight on how <a style="color: #dd0031; font-weight: 600" href="https://www.health.org.uk/evidence-hub/transport" target="_blank" rel="noopener noreferrer">transport can affect health and health inequalities.</a>'))
      
    } else if(input$tabset == "Education"){
      
      HTML(str_c('Education plays an important role in shaping people’s health. Education influences income and employment, which in turn provide the resources people need to maintain good health, such as nutritious food and safe housing.<br><br>Click here for more analysis and insight on how <a style="color: #dd0031; font-weight: 600" href="https://www.health.org.uk/evidence-hub/education" target="_blank" rel="noopener noreferrer">education can affect health and health inequalities.</a>'))
      
    } else if(input$tabset == "Community"){
      
      HTML(str_c('Family, friends and communities are the cornerstone of our everyday lives. The relationships we form, the support we have, and the interactions we experience can influence our health in a range of ways.<br><br>Click here for more analysis and insight on how <a style="color: #dd0031; font-weight: 600" href="https://www.health.org.uk/evidence-hub/ffc" target="_blank" rel="noopener noreferrer">family, friends and community can affect health and health inequalities.</a>'))
      
    } else if(input$tabset == "Surroundings"){
      
      HTML(str_c('Where we live can shape our health, including how long we can expect to live. Inequalities between local areas restrict people’s opportunities to live a healthy life, from the air we breathe to the goods available to buy locally.<br><br>Click here for more analysis and insight on how <a style="color: #dd0031; font-weight: 600" href="https://www.health.org.uk/evidence-hub/our-surroundings" target="_blank" rel="noopener noreferrer">our surroundings can affect health and health inequalities.</a>'))
      
    } else if(input$tabset %in% c("Sources", "Glossary")){
      
      HTML(str_c('<div style="display:inline-block;width:30%;vertical-align:top;"><img src="WPIe_logo.png" style="max-width:100%;vertical-align:top;padding-right:5px"></div>',
                 '<div style="display:inline-block;width:70%;vertical-align:top;">This dashboard was created by <a href="https://wpieconomics.com/" target="_blank" rel="noopener noreferrer">WPI Economics</a> for the Health Foundation using publicly available local authority data. If you have any queries or feedback on this dashboard, please contact the Health Foundation <a href="mailto: evidencehub@health.org.uk" target="_blank" rel="noopener noreferrer">here</a>. If you have any questions on the data sources used, please contact WPI Economics <a href="mailto: info@wpieconomics.com" target="_blank" rel="noopener noreferrer">here</a>.</div>'))
      
    }
    
  })
  
  ## Health
  
  health_server(id = "health",
                local_authority = selected,
                gender = sex)
  
  ## Work
  
  work_server(id = "work",
              local_authority = selected,
              gender = sex)
  
  ## Money
  
  money_server(id = "money",
               local_authority = selected,
               gender = sex)
  
  ## Housing
  
  housing_server(id = "housing",
                 local_authority = selected,
                 gender = sex)
  
  ## Transport
  
  transport_server(id = "transport",
                   local_authority = selected,
                   gender = sex)
  
  ## Education
  
  education_server(id = "education",
                   local_authority = selected,
                   gender = sex)
  
  ## Community
  
  community_server(id = "community",
                   local_authority = selected,
                   gender = sex)
  
  ## Surroundings
  
  surroundings_server(id = "surroundings",
                      local_authority = selected,
                      gender = sex)
  
  ## Data source
  
  datasource_server(id = "datasource")
  
  ## Glossary
  
  glossary_server(id = "glossary")
  
}

shinyApp(ui = ui,
         server = server)
