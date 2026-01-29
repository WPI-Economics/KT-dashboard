library(dplyr)
library(shiny)
library(htmltools)
library(highcharter)
library(bslib)
library(bsicons)
library(sass)
library(downloadthis)
library(shinyWidgets)


#read in data, colours and highchart chart

source("datasetup_charts.r")

my_theme <- bs_theme(version = 5,
                     fg = kt_colors[5],
                     bg = kt_colors[6],
                     primary = "#FFFFFF",
                     secondary = "#FFFFFF",
                     base_font = "Helvetica") %>% 
  bs_add_rules(
    sass::sass_file("www/styles.scss")
  )


########################################################################
########################################################################
#####################         UI         ###############################
########################################################################
########################################################################

ui <- 
  
  page_fillable(
    
    theme = my_theme,
    
    
    div(class = "container-md",
        
        div(class = "container-md",
            id = "main-header",
            img(src = "logo_left2.svg", id = "main-logo", class = "header-logo"),
            tags$h1("SOCIAL VALUE DASHBOARD 2026", id = "main-title"),
            img(src = "logo_right.svg", id = "main-logo", class = "header-logo")
        ),
        
        tags$script(HTML("
  Shiny.addCustomMessageHandler('addSelectedClass', function(id) {
    $('#' + id).addClass('selected');
  });

  Shiny.addCustomMessageHandler('removeSelectedClass', function(id) {
    $('#' + id).removeClass('selected');
  });

  Shiny.addCustomMessageHandler('resetSelectize', function(id) {
    var el = $('#' + id)[0];
    if (el && el.selectize) {
      el.selectize.clearOptions();
    }
  });
"))
        ,
        #intro box
        # Intro row
        fluidRow(
          column(
            width = 12,
            bslib::card(
              id = "intro_box",
              uiOutput("intro_box_copy")
            )
          )
        ),
        
        page_navbar(
          title = "",   # HEADER
          id = "summary-navbar",
          
          # ---- TAB summary ----
          nav_panel(
            
            "Summary",
            
            #removed as we don't need a sidebar for the summary
            # layout_sidebar(
            #   sidebar = sidebar(
            #     value_box("SMetric A", value = "321", height = "10em",),
            #     value_box("SMetric B", value = "654", height = "10em",),
            #     value_box("SMetric C", value = "987", height = "10em",),
            #     value_box("SMetric D", value = "202", height = "10em",),
            #     value_box("SMetric E", value = "303", height = "10em",)
            #   ),
            
            layout_column_wrap(
              width = 1/1,
              max_height = 350,
              min_height = 350,
              # --- FILTER ROW ---
              card(
                class = "filter-card",
                #style = "padding: 0;",
                
                card_body(
                  
                  layout_column_wrap(
                    width = 1/3,
                    selectizeInput("filter1", "Group", choices = unique(df$group_type), 
                                   options = list( 
                                     persist = FALSE, 
                                     create = FALSE )),
                    selectizeInput("filter2", "Subgroup", 
                                   choices = NULL, 
                                   options = list( 
                                     persist = FALSE, 
                                     create = FALSE )),
                    
                    selectizeInput("filter2b", "Comparison (chart only)", 
                                   choices = NULL, 
                                   options = list( 
                                     persist = FALSE, 
                                     create = FALSE ,
                                     sortField = list(list(field = "text", direction = "asc"))
                                   ))
                  ) #end column wrap
                ) #end card body
              ), #end card
              card_body(
               
                layout_column_wrap(
                  width = 1/2,
                  
                  #first box interactiveso in server
                  uiOutput("t1_totalbox"),
                  
                  #this one all static so all here
                  value_box( 
                    title = "Social value represents total savings from five dimensions: Economic, Wellbeing, Volunteering, DWP/Health admin, and Reduced re-offending",
                            value = NULL, #pipe this in from the data
                            showcase = NULL,
                            theme = value_box_theme(bg = kt_colors[1]),
                            fill = TRUE)
                )),
              
              
              card(fill = FALSE,
                   #card_header("Title"),
                   highchartOutput("highchart_plot1")
              )
            )
            #)
          ),
          
          # ---- TAB 2Time Series ----
          nav_panel(
            "Time Series",
            layout_sidebar(
              sidebar = sidebar(
                
                uiOutput("t2_sidebox1"),
                uiOutput("t2_sidebox2"),
                uiOutput("t2_sidebox3"),
                uiOutput("t2_sidebox4"),
                uiOutput("t2_sidebox5")
              ),
              
              
              
              # Main body
              layout_column_wrap(
                width = 1/1,
                max_height = 350,
                min_height = 350,
                # --- FILTER ROW ---
                card(
                  class = "filter-card",
                  card_body(
                    style = "height: 80px;",
                    layout_column_wrap(
                      width = 1/3,
                      selectizeInput("filter3", "Group", choices = unique(df$group_type), 
                                     options = list( 
                                       persist = FALSE, 
                                       create = FALSE )),
                      selectizeInput("filter4", "Subgroup", choices = NULL, 
                                     options = list(
                                       persist = FALSE, create = FALSE )) ,
                      
                      selectizeInput("filter4b", "Comparison (chart only)", 
                                     choices = NULL, 
                                     options = list( 
                                       persist = FALSE, 
                                       create = FALSE ,
                                       sortField = list(list(field = "text", direction = "asc"))
                                     ))
                    )
                  )
                ),
                
                #red total box
                div(
                  class = "sv-summary-row", 
                  uiOutput("t1_totalbox2")
                ),
                
                # #time filter
                div(
                  class = "fy-slider-row",
                 sliderTextInput("fy_range",
                 "",
                 choices = unique(df$`Cohort years`),
                 selected = c(fy_levels[1], fy_levels[length(fy_levels)]),
                 grid = FALSE,
                 width = "100%"),
                
                card(fill = FALSE,
                     #card_header("Title"),
                     highchartOutput("highchart_plot2")
                     )
                )
              )
            )
          ),
          
          
          
          # ---- TAB Glossary ----
          nav_panel(
            "Glossary",
            card(
              card_header("Glossary"),
              card_body(HTML("<h3>Social value:</h3><p>This is the sum of the savings across the various dimensions</p><br>
                    <h3>Economic value:</h3><p>description here</p><br>
                    <h3>Wellbeing:</h3><p>description here</p><br>
                    <h3>Volunteer value:</h3><p>description here</p><br>
                    <h3>DWP/Health admin:</h3><p>description here</p><br>
                    <h3>Reduced re-offending:</h3><p>description here</p><br>
                    "))
            )
          ),
          
          # ---- TAB Data Sources ----
          nav_panel(
            "Data Sources",
            card(
              card_header("Data Sources"),
              card_body(HTML("<h3>Kings Trust Sources:</h3>
                             <p><b>Partipants</B> extracted xx/xx/xx, covering 2015/16 to 2024/25</p>
                             <p><b>Outcomes</B> extracted xx/xx/xx, covering 2015/16 to 2024/25</p>
                             <p><b>My Journey</B> extracted xx/xx/xx, covering 2015/16 to 2024/25</p>
                             <p><b>Health and Social care outcomes</B> extracted xx/xx/xx, covering 2015/16 to 2024/25</p>
                             <p><b>Volunteers</B> extracted xx/xx/xx, covering 2015/16 to 2024/25</p>
                             
                             <h3>External sources</h3>
                             <p><b>Annual Population Survey, ONS</b> Used for deadweight calculations
                             <p><b>Labour productivity (GVA), ONS</b> Used for GVA calculations
                             <p><b>Wellbeing, ONS</b> Used for deadweight calculations
                             <p><b>Consumer prices index (housing), ONS</b> Used for deflators
                             "))
            )
          )
        ))
  )


################################################################################################################################################
################################################################################################################################################
#####################         SERVER     #######################################################################################################
################################################################################################################################################
########################################################################

server <- function(input, output, session) {
  
  
  #################
  ################# filter stuff tab1
  #################
  
  # Shared filter state across tabs
  state <- reactiveValues(
    parent = NULL,
    subgroup = NULL
  )
  
  # Tab 1: when filter1 changes
  observeEvent(input$filter1, {
    state$parent <- input$filter1
    
    if (is.null(input$filter1) || input$filter1 == "") {
      session$sendCustomMessage("resetSelectize", "filter2")
      updateSelectizeInput(session, "filter2", choices = NULL, server = TRUE)
      state$subgroup <- NULL
      return()
    }
    
    subset_choices <- unique(df$group[df$group_type == input$filter1])
    
    session$sendCustomMessage("resetSelectize", "filter2")
    
    updateSelectizeInput(
      session, "filter2",
      choices  = subset_choices,
      selected = subset_choices[1],
      server   = TRUE
    )
    
    updateSelectizeInput(
      session, "filter2b",
      choices  = c("-", subset_choices),
      selected = "-",
      server   = TRUE
    )
    
    state$subgroup <- subset_choices[1]
  })
  
  # Tab 1: when filter2 changes
  observeEvent(input$filter2, {
    state$subgroup <- input$filter2
    
    req(input$filter1)
    
    subset_choices <- unique(df$group[df$group_type == input$filter1])
    comparison_choices <- subset_choices[subset_choices != input$filter2]
    
    # Reset filter2b before updating 
    session$sendCustomMessage("resetSelectize", "filter2b")   
    updateSelectizeInput(
      session, "filter2b",
      choices  = c("-", comparison_choices),
      selected = "-",
      server   = TRUE
    )
  })
  
  
  
  #################
  ################# filter stuff tab2
  #################
  
  
  # financial year slider
  
  year_lookup <- df %>% 
    distinct(`Cohort years`) %>% 
    arrange(`Cohort years`) %>% 
    mutate(year_index = row_number())
  
  selected_years <- reactive({
    req(input$fy_range)
    
    # make sure we’re comparing characters
    endpoints <- as.character(input$fy_range)
    
    idx <- year_lookup$year_index[year_lookup$`Cohort years` %in% endpoints]
    
    idx_min <- min(idx)
    idx_max <- max(idx)
    
    year_lookup$`Cohort years`[year_lookup$year_index >= idx_min &
                                 year_lookup$year_index <= idx_max]
  })
  
  
  number_years_seleted <- reactive({length(selected_years())})
  

  # Shared filter state across tabs
  state2 <- reactiveValues(
    parent = NULL,
    subgroup = NULL
  )
  
  # Tab 1: when filter1 changes
  observeEvent(input$filter3, {
    state2$parent <- input$filter3
    
    if (is.null(input$filter3) || input$filter3 == "") {
      session$sendCustomMessage("resetSelectize", "filter4")
      updateSelectizeInput(session, "filter4", choices = NULL, server = TRUE)
      state2$subgroup <- NULL
      return()
    }
    
    subset_choices2 <- unique(df$group[df$group_type == input$filter3])
    
    session$sendCustomMessage("resetSelectize", "filter4")
    
    updateSelectizeInput(
      session, "filter4",
      choices  = subset_choices2,
      selected = subset_choices2[1],
      server   = TRUE
    )
    
    updateSelectizeInput(
      session, "filter4b",
      choices  = c("-", subset_choices2),
      selected = "-",
      server   = TRUE
    )
    
    state2$subgroup <- subset_choices2[1]
  })
  
  # Tab 1: when filter2 changes
  observeEvent(input$filter4, {
    state2$subgroup <- input$filter4
    
    req(input$filter3)
    
    subset_choices2 <- unique(df$group[df$group_type == input$filter3])
    comparison_choices2 <- subset_choices2[subset_choices2 != input$filter4]
    
    # Reset filter2b before updating 
    session$sendCustomMessage("resetSelectize", "filter4b")   
    updateSelectizeInput(
      session, "filter4b",
      choices  = c("-", comparison_choices2),
      selected = "-",
      server   = TRUE
    )
    
  })
  
  # Tab 1: when filter2 changes
  observeEvent(input$filter4, {
    state2$subgroup <- input$filter4
  })
  
  #the copy for the umm intro box!
  output$intro_box_copy <- renderUI({
   tagList(HTML("<span class='intro_copy'> This dashboard presents the headline results of the King's Trust Social Returns on Investment (SROI) economic analysis undertaken in collaboration with WPI Economics. <br>
         The summary tab below gives results aggregated over the ten year period from 2015/16 to 2024/25 split by 5 components. Use the filters to select a sub group of interest.<br>
         The `Time Series` tab gives results over time. Use the blue buttons on the left in that tab to add one of the five components to the chart and also use the filters to select sub-groups of interest.<br>"),
   div(
     style = "text-align: center; margin-top: 10px;",
    download_this(
      .data = df,
      output_name = "sroi_data", 
      output_extension = ".xlsx", 
      button_label = "Download data", 
      button_type = "default"
    ))
    )
  })
  
  
  
  #selected subgroup value to use in data filter
  output$t1_totalbox <- renderUI({
    
    selected <- state$subgroup 
    filtered <- df_ten_yr[df_ten_yr$group %in% selected, ] 
    total <- sum(filtered$`Total savings`, na.rm = TRUE)
    
    value_box(title = "10 year social value",
              #value = custom_number_format(df_ten_yr$`Total savings`[df_ten_yr$group == "-"]), #pipe this in from the data
              value = custom_number_format(total),
              showcase = bs_icon("clipboard-data"),
              theme = value_box_theme(bg = kt_colors[1]), 
              
              fill = TRUE)
  })
  
  #selected subgroup value to use in data filter tab2 version
  output$t1_totalbox2 <- renderUI({
    
    # data logic
    selected <- state2$subgroup
    filtered <- df[df$group %in% selected, ]
    filtered <- filtered[filtered$`Cohort years` %in% selected_years(), ]
    total <- sum(filtered$`Total savings`, na.rm = TRUE)
    
    value_box(title = paste0("Total ","social value over ", number_years_seleted(), " years", " from ",selected_years()[1] , " to ",last(selected_years()) ), #make the 
              #value = custom_number_format(df_ten_yr$`Total savings`[df_ten_yr$group == "-"]), #pipe this in from the data
              value = custom_number_format(total),
              showcase = bs_icon("clipboard-data"),
              theme = value_box_theme(bg = kt_colors[1]),
              fill = TRUE)
  })
  
  ###############
  ############### clickable cards as selectors
  ###############
  
  
  output$t2_sidebox1 <- renderUI({
    
    req(state2$subgroup)
    req(selected_years())
    
    # data logic
    selected <- state2$subgroup
    filtered <- df[df$group %in% selected, ]
    filtered <- filtered[filtered$`Cohort years` %in% selected_years(), ]
    total <- sum(filtered$`Economic value (GVA)`, na.rm = TRUE)
    

    # build the UI
    ui <- tags$div(
      id = "select_econ_value_box",
      class = "value-box-button",
      onclick = "Shiny.setInputValue('select_econ_value', Math.random())",
      value_box(
        title = paste0("Economic (",number_years_seleted(), "yr)"),
        showcase = bs_icon("bank", size = "1.5rem"),
        value = custom_number_format(total),
        height = "7em",
        theme = value_box_theme(bg = kt_colors[2])
      )
    )
    
    # re-apply the selected class AFTER the DOM is updated
    session$onFlushed(function() {
      val = isolate(selected_metric_id())
      if (!is.null(val) && val == "select_econ_value") {
        session$sendCustomMessage("addSelectedClass", "select_econ_value_box")
      }
    }, once = FALSE)
    
    ui
  })
  
  
  
  output$t2_sidebox2 <- renderUI({
    #the data logic
    selected <- state2$subgroup
    filtered <- df[df$group %in% selected, ]
    filtered <- filtered[filtered$`Cohort years` %in% selected_years(), ]
    total <- sum(filtered$"Reduced re-offending", na.rm = TRUE)
    #the box
    ui <- tags$div(
      id = "select_off_value_box",
      class = "value-box-button",
      onclick = "Shiny.setInputValue('select_off_value', Math.random())",
      value_box(
        title = paste0("Re-offending (",number_years_seleted(), "yr)"),
        showcase = bs_icon("sign-stop", size = "1.5rem"),
        value = custom_number_format(total),
        height = "7em",
        theme = value_box_theme(bg = kt_colors[2])  #"red"
      )
    )
    
    # re-apply the selected class AFTER the DOM is updated
    session$onFlushed(function() {
      val = isolate(selected_metric_id())
      if (!is.null(val) && val == "select_off_value") {
        session$sendCustomMessage("addSelectedClass", "select_off_value_box")
      }
    }, once = FALSE)
    
    ui
  })
  
  output$t2_sidebox3 <- renderUI({
    #the data logic
    selected <- state2$subgroup
    filtered <- df[df$group %in% selected, ]
    filtered <- filtered[filtered$`Cohort years` %in% selected_years(), ]
    total <- sum(filtered$"DWP/health admin", na.rm = TRUE)
    #the box
    ui <- tags$div(
      id = "select_dwp_value_box",
      class = "value-box-button",
      onclick = "Shiny.setInputValue('select_dwp_value', Math.random())",
      value_box(
        title = paste0("DWP/health (",number_years_seleted(), "yr)"),
        showcase = bs_icon("pencil", size = "1.5rem"),
        value = custom_number_format(total),
        height = "7em",
        theme = value_box_theme(bg = kt_colors[2]) #"red"
      )
    )
    # re-apply the selected class AFTER the DOM is updated
    session$onFlushed(function() {
      val = isolate(selected_metric_id())
      if (!is.null(val) && val == "select_dwp_value") {
        session$sendCustomMessage("addSelectedClass", "select_dwp_value_box")
      }
    }, once = FALSE)
    
    ui
    
  })
  
  
  output$t2_sidebox4 <- renderUI({
    #the data logic
    selected <- state2$subgroup
    filtered <- df[df$group %in% selected, ]
    filtered <- filtered[filtered$`Cohort years` %in% selected_years(), ]
    total <- sum(filtered$"Wellbeing", na.rm = TRUE)
    #the box
    ui <- tags$div(
      id = "select_well_value_box",
      class = "value-box-button",
      onclick = "Shiny.setInputValue('select_well_value', Math.random())",
      value_box(
        title = paste0("Wellbeing (",number_years_seleted(), "yr)"),
        showcase = bs_icon("sun", size = "1.5rem"),
        value = custom_number_format(total),
        height = "7em",
        theme = value_box_theme(bg = kt_colors[2]) #"white"
      )
    )
    # re-apply the selected class AFTER the DOM is updated
    session$onFlushed(function() {
      val = isolate(selected_metric_id())
      if (!is.null(val) && val == "select_well_value") {
        session$sendCustomMessage("addSelectedClass", "select_well_value_box")
      }
    }, once = FALSE)
    
    ui
  })
  
  output$t2_sidebox5 <- renderUI({
    #the data logic
    selected <- state2$subgroup
    filtered <- df[df$group %in% selected, ]
    filtered <- filtered[filtered$`Cohort years` %in% selected_years(), ]
    total <- sum(filtered$"Volunteer value", na.rm = TRUE)
    #the box
    ui <- tags$div(
      id = "select_vol_value_box",
      class = "value-box-button",
      onclick = "Shiny.setInputValue('select_vol_value', Math.random())",
      value_box(
        title = paste0("Volunteering (",number_years_seleted(), "yr)"),
        showcase = bs_icon("person-arms-up", size = "1.5rem"),
        value = custom_number_format(total),
        height = "7em",
        theme = value_box_theme(bg = kt_colors[2]) #"orange"
      )
    )
    # re-apply the selected class AFTER the DOM is updated
    session$onFlushed(function() {
      val = isolate(selected_metric_id())
      if (!is.null(val) && val == "select_vol_value") {
        session$sendCustomMessage("addSelectedClass", "select_vol_value_box")
      }
    }, once = FALSE)
    
    ui
  })
  
  
  #  1 Define the mapping (static, non-reactive)
  metric_map <- c(
    select_econ_value = colnames(df_ten_yr)[4], #the indexed colnames in case they change
    select_off_value  = colnames(df_ten_yr)[5],
    select_dwp_value  = colnames(df_ten_yr)[6],
    select_well_value = colnames(df_ten_yr)[7],
    select_vol_value  = colnames(df_ten_yr)[9]
  )
  
  # 2 Define the reactive state
  selected_metric <- reactiveVal("Dummy")  # default
  selected_metric_id <- reactiveVal(NULL)
  # 3 Observe all value-box buttons
  
  observeEvent(input$select_econ_value, {
    selected_metric(metric_map[["select_econ_value"]])
    selected_metric_id("select_econ_value")
  })
  
  observeEvent(input$select_off_value, {
    selected_metric(metric_map[["select_off_value"]])
    selected_metric_id("select_off_value")
  })
  
  observeEvent(input$select_dwp_value, {
    selected_metric(metric_map[["select_dwp_value"]])
    selected_metric_id("select_dwp_value")
  })
  
  observeEvent(input$select_well_value, {
    selected_metric(metric_map[["select_well_value"]])
    selected_metric_id("select_well_value")
  })
  
  observeEvent(input$select_vol_value, {
    selected_metric(metric_map[["select_vol_value"]])
    selected_metric_id("select_vol_value")
  })
  
  
  # different css for clicked and not clicked sidebar
  observe({
    # remove selected class from all
    lapply(c(
      "select_econ_value_box",
      "select_off_value_box",
      "select_dwp_value_box",
      "select_well_value_box",
      "select_vol_value_box"
    ), function(id) {
      session$sendCustomMessage("removeSelectedClass", id)
    })
    
    # add selected class to the active one 
    if (!is.null(selected_metric_id())) {
      active_id <- paste0(selected_metric_id(), "_box") 
      session$sendCustomMessage("addSelectedClass", active_id) }
  })
  
  
  
  ###############
  ############### reactive data for tab 1 chart
  ###############
  
  data_highchart1 <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df_ten_yr %>% #filter(`Cohort years` == "2024/25") %>% 
      select(-c(`Cohort count`,`Total savings`)) %>% 
      filter(group == input$filter2, # <<<< INTERACTIVE INPUT HERE
             
             
      ) %>% 
      pivot_longer(
        cols = 3:ncol(.),
        values_to = "values",
        names_to = "names"
      ) %>% 
      arrange(match(`names`, df_total$names)) 
    
  })
  
  data_highchart1_comparison <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df_ten_yr %>% #filter(`Cohort years` == "2024/25") %>% 
      select(-c(`Cohort count`,`Total savings`)) %>% 
      filter(group == input$filter2b, # <<<< INTERACTIVE INPUT HERE
             
      ) %>% 
      pivot_longer(
        cols = 3:ncol(.),
        values_to = "values",
        names_to = "names"
      ) %>% 
      arrange(match(`names`, df_total$names)) 
  })
  
  
  ###############
  ############### Tab1 XS chart
  ###############
  
  output$highchart_plot1 <- 
    
    renderHighchart(expr = {#chart
      highchart1 <- highchart() %>% 
        hc_chart(type = "column", spacingRight = 80) %>%
        
        hc_xAxis(categories = df_total$names, #substitute this for the selected interactive filter input
                 title = list(text = "")
                 
        ) %>% 
        
        hc_plotOptions(
          column = list(
            animation = FALSE,
            borderRadius = 5,
            grouping = TRUE   # put series side by side
            #stacking = "normal" # use stack to align them
          )
        ) %>%  
        
        
        # BACK BAR (TOTAL)
        # hc_add_series(
        #   name = "All",
        #   data = df_total$values ,
        #   type = "column",
        #   stack = "Main",
        # 
        #   # Shared width logic
        #   pointPadding = 0,
        #   groupPadding = 0.2,
        #   maxPointWidth = 120,
        #   pointPlacement = 0,
        # 
        #   borderWidth = 0,
        #   color = kt_colors[11], #kt_colors[6],
        #   visible = F,
        #   zIndex = 1
        #   #showInLegend = FALSE
        # ) %>%
        
        # FRONT BAR (SUB-GROUP)
        hc_add_series(
          name = unique(data_highchart1()$group), #"All", ##<<<< interactive value 
          data = data_highchart1()$values, ##<<<< interactive value 
          type = "column",
          stack = "Main",
          
          # Must match the total series here
          # pointPadding = 0,
          groupPadding = 0.1,
          maxPointWidth = 120,
          #pointPlacement = 0,
          
          borderWidth = 0,
          color = kt_colors[1],
          zIndex = 2
        ) %>%
        
        
        
        
        hc_yAxis(#title = list(text = "£"),
          labels = list(
            formatter = JS(" function() { return '£' + Highcharts.numberFormat(this.value / 1e6, 0, '.', ',') + 'M</b>'; } ") 
          )) %>% 
        
        hc_exporting(enabled = FALSE) %>% 
        hc_tooltip(
          useHTML = TRUE, 
          formatter = JS(" function() { return '£' + Highcharts.numberFormat(this.y / 1e6, 0, '.', ',') + 'M</b>'; } ") 
        ) %>% 
        hc_add_theme(kt_theme) %>% 
        hc_exporting(enabled = TRUE,
                     buttons = list(
                       contextButton = list(
                         menuItems = c("downloadSVG","downloadPNG", "downloadXLS"))))
      
      
      
      # Only gets added if comparison filter is selected
      if(!is.null(input$filter2b) && input$filter2b != "-"){
        highchart1  <- highchart1 %>%
          # COMPARISON BAR (SUB-GROUP)
          
          hc_add_series(
            name = unique(data_highchart1_comparison()$group), #"All", ##<<<< interactive value
            data = data_highchart1_comparison()$values, ##<<<< interactive value
            type = "column",
            stack = "comparison",
            
            # Must match the total series here
            # pointPadding = 0,
            groupPadding = 0.1,
            maxPointWidth = 120,
            #pointPlacement = 0,
            
            borderWidth = 0,
            color = kt_colors[2],
            zIndex = 2
          ) }
      
      highchart1
      
    } 
    )
  
  ###############
  ############### reactive data for tab 2 chart
  ###############
  
  data_highchart_aspect_sub <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% 
      filter(group == input$filter4) %>%  # <<<< INTERACTIVE INPUT HERE
      filter(`Cohort years` %in% selected_years()) %>% 
      select(c(`Cohort years`, group, group_type, `Cohort count`, selected_column = all_of(selected_metric())))
  })
  
  #aspect for comparison
  data_highchart_aspect_sub2 <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% 
      filter(group == input$filter4b) %>%  # <<<< INTERACTIVE INPUT HERE
      filter(`Cohort years` %in% selected_years()) %>% 
      select(c(`Cohort years`, group, group_type, `Cohort count`, selected_column = all_of(selected_metric())))
  })
  
  #subgroup data
  data_highchart_total_sub <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% 
      filter(group == input$filter4) %>%  # <<<< INTERACTIVE INPUT HERE
      filter(`Cohort years` %in% selected_years()) %>% 
      select(c(`Cohort years`, group, group_type, `Cohort count`, `Total savings`))
  })
  
  #comparison group data
  data_highchart_total_sub2 <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% 
      filter(group == input$filter4b) %>%  # <<<< INTERACTIVE INPUT HERE
      filter(`Cohort years` %in% selected_years()) %>% 
      select(c(`Cohort years`, group, group_type, `Cohort count`, `Total savings`))
  })
  
  #All (when we have constant series behind the bar)
  data_highchart_aspect_all <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df_all %>% 
      select(c(`Cohort years`, group, group_type, `Cohort count`, 
               selected_column = all_of(selected_metric())))
  })
  
  ###############
  ############### Tab2 TS chart
  ###############
  
  output$highchart_plot2 <- 
    
    renderHighchart(expr = {
      
      # this sets up a object for the data labels
      # Interactive with the aspect/element of SROI only - always shows the all/total no subgroups
      #cht_data <- df_all$`Economic value (GVA)`
      cht_data <- data_highchart_aspect_all() %>% pull(selected_column)
      
      # Create a list of points with dataLabels only on the last one, showing series.name
      cht_series <- lapply(seq_along(cht_data), function(i) {
        if (i == length(cht_data)) {
          list(
            y = cht_data[i],
            dataLabels = list(
              enabled = TRUE,
              align = "left",
              y = 15,
              crop = F,
              overflow = "allow",
              format = "{series.name}"
            )
          )
        } else {
          list(y = cht_data[i])
        }
      })
      
      
      
      # this sets up a object for the data labels for the second series - USER INTERACTIVE
      cht_data_sub <- data_highchart_aspect_sub() %>% pull(selected_column)
      
      # Create a list of points with dataLabels only on the last one, showing series.name
      cht_series_sub <- lapply(seq_along(cht_data_sub), function(i) {
        if (i == length(cht_data)) {
          list(
            y = cht_data_sub[i],
            dataLabels = list(
              enabled = TRUE,
              align = "right",
              y = 15,
              crop = F,
              overflow = "allow",
              format = "{series.name}"
            )
          )
        } else {
          list(y = cht_data_sub[i])
        }
      })
      
      
      # this sets up a object for the data labels for the comparison series - USER INTERACTIVE
      cht_data_sub2 <- data_highchart_aspect_sub2() %>% pull(selected_column)
      
      # Create a list of points with dataLabels only on the last one, showing series.name
      cht_series_sub2 <- lapply(seq_along(cht_data_sub2), function(i) {
        if (i == length(cht_data)) {
          list(
            y = cht_data_sub2[i],
            dataLabels = list(
              enabled = TRUE,
              align = "right",
              y = 15,
              crop = F,
              overflow = "allow",
              format = "{series.name}",
              style = list( fontWeight = "normal") # ← unbold the label
            )
          )
        } else {
          list(y = cht_data_sub2[i])
        }
      })
      
      
      highchart2 <- highchart() %>% 
        hc_chart(type = "column"#, 
                 #spacingRight = 80
        ) %>%
        hc_title(text = "", align = "left", 
                 
                 style = list(fontSize ="24px",#color = green.pair[1], 
                              fontFamily = "Arial", fontWeight = "400" )) %>% 
        hc_exporting(enabled = F) %>% 
        
        hc_xAxis(categories = data_highchart_aspect_sub()$`Cohort years`, #substitute this for the selected interactive filter input
                 title = list(text = "")) %>% 
        hc_yAxis(#title = list(text = "£"),
          labels = list(
            formatter = JS(" function() { return '£' + Highcharts.numberFormat(this.value / 1e6, 0, '.', ',') + 'M</b>'; } ") 
          )) %>% 
        hc_plotOptions(
          column = list(
            borderRadius = 5,
            animation = FALSE,
            grouping = TRUE) ) %>% 
        hc_tooltip(
          useHTML = TRUE, 
          formatter = JS(" function() { return '£' + Highcharts.numberFormat(this.y / 1e6, 0, '.', ',') + 'M</b>'; } ") 
        ) %>% 
        hc_add_theme(kt_theme) %>% 
        hc_exporting(enabled = TRUE,
                     buttons = list(
                       contextButton = list(
                         menuItems = c("downloadSVG","downloadPNG", "downloadXLS"))))
      
      
      
      
      # # #bar total (CONSTANT)
      # hc_add_series(name= "Total SROI",
      #               data = df_all$`Total savings`,
      #               type = "column",
      #               stack = "Main",
      # 
      #               # Shared width logic
      #               pointPadding = 0,
      #               groupPadding = 0.2,
      #               maxPointWidth = 120,
      #               pointPlacement = 0,
      #               visible = F,
      #               color = ifelse(input$filter3 == "all" , kt_colors[1], kt_colors[11]), #red
      #               zIndex = 1) %>%
      # 
      # 
      # 
      # #line component value
      #   hc_add_series(data = cht_series, #make this interactive from the side boxes
      #                 type = "line",
      #                 name = ifelse(selected_metric() == "Dummy","",  paste0(selected_metric(), ":<br>All ")), #how to get this out of metric_map??
      #                 marker = list(symbol = 'circle'),
      #                 pointPlacement = "on",
      #                 color = kt_colors[5],
      #                 visible = F,
      #                 zIndex = 50,
      #                 dataLabels = list(enabled = F))
      
      
      #condition so that sub groups don't render until a sub group selected
      if (input$filter4 != "all") {
        highchart2 <- highchart2 %>% 
          
          #bar sub-group
          hc_add_series(name= paste0("Total SROI: ", unique(data_highchart_total_sub()$group)),
                        data = data_highchart_total_sub()$`Total savings`, #make this interactive from the side boxes
                        type = "column",
                        stack = "Main",
                        color = kt_colors[1], #lihght red
                        
                        # Shared width logic
                        #pointPadding = 0,
                        groupPadding = 0.2,
                        maxPointWidth = 120,
                        #pointPlacement = 0,
                        #position = list(offsetY = -25),
                        
                        zIndex = 2
          ) %>%
          
          
          #line component value sub-group
          hc_add_series(data = cht_series_sub, #make this interactive from the side boxes
                        type = "line",
                        name = ifelse(str_detect(selected_metric(),"Dummy"), 
                                      "",  
                                      paste0(selected_metric(),":<br>", 
                                             unique(data_highchart_aspect_sub()$group))), #make this interactive from the side boxes
                        color = kt_colors[11],
                        
                        zIndex = 51,
                        marker = list(symbol = 'circle'),
                        dataLabels = list(enabled = F))}
      
      
      
      highchart2
      
      #condition so that comparison don't render until selected
      if (input$filter4b != "-") {
        highchart2 <- highchart2 %>% 
          
          #bar sub-group
          hc_add_series(name= paste0("Total SROI: ", unique(data_highchart_total_sub2()$group)),
                        data = data_highchart_total_sub2()$`Total savings`, #make this interactive from the side boxes
                        type = "column",
                        stack = "Comparison",
                        color = kt_colors[2], #lihght red
                        
                        # Shared width logic
                        #pointPadding = 0,
                        groupPadding = 0.2,
                        maxPointWidth = 120,
                        # pointPlacement = 0,
                        #position = list(offsetY = -25),
                        
                        zIndex = 2
          ) %>%
          
          
          #line component value sub-group
          hc_add_series(data = cht_series_sub2, #make this interactive from the side boxes
                        type = "line",
                        name = ifelse(str_detect(selected_metric(),"Dummy"), 
                                      "",  
                                      paste0(selected_metric(),":<br>", unique(data_highchart_aspect_sub2()$group))), #make this interactive from the side boxes
                        color = kt_colors[7],
                        
                        zIndex = 51,
                        marker = list(symbol = 'circle'),
                        dataLabels = list(enabled = F))}
      
      
      
      highchart2
      
    } 
    )
  
  
}

shinyApp(ui, server)
