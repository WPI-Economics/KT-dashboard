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
            
            a(
              href = "https://wpieconomics.com/",
              target = "_blank",
            img(src = "logo_right.svg", id = "main-logo", class = "header-logo")
            )
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
                uiOutput("t2_sidebox4"),
                uiOutput("t2_sidebox5"),
                uiOutput("t2_sidebox3"),
                uiOutput("t2_sidebox2")
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
                  uiOutput("t2_totalbox")
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
              card_body(HTML("<h3>Social Value</h4><p>This is the sum of the savings across the five dimensions (Economic, Wellbeing, Volunteering, reduced re-offending and savings to DWP/health admin.</p><br>
                    <h3>Economic value</h4><p><b>Gross Value Added (GVA) to the economy.</b><br> This is derived from employment and salaries outcomes modelled from KT outcomes data using a factor multiplier to convert salaries into Gross Value Added. This is adjusted for deadweight (i.e. what would have happened anyway without KT input), adjusted for inflation (CPIH) so all years are comparable, and also adjusted for displacement and attribution. This measure is only applicable to those aged 18+</p><br>
                    <h3>Wellbeing</h4><p><b>The economic £ value of improvments to the wellbeing of KT participants.</b><br> This is modelled from KT Health and Social Care and My Journey data. Wellbeing change is converted into economic £ values via the HM treasury guidance on valuation of Wellbeing-adjusted life years (WELLBY), and are adjusted for attribution. The results apply to all ages (including under 18s)</p><br>
                    <h3>Volunteer value</h4><p><b>The economic value of volunteering.</b><br> This sums together value derived from KT participants volunteering activity, and volunteers and mentors that help KT participants. Volunteering counts in the KT data are too low to support regression modelling, so we calculate an overall value using living wage for 13 hours per week for three months. External volunteers are valued at 48 hours per year with £25 per hour for mentors and Living Wage for others.</p><br>
                    <h3>DWP/Health admin</h4><p><b>The savings to the exchequer of reducing admin for DWP and health serivices.</b><br> These apply fixed saving values for ESA and JSA taken from Greater Manchester Combined Authority Cost Benefit Analysis (CBA) model and apply it to those that have gained employment in our modelling</p><br>
                    <h3>Reduced re-offending</h4><p><b>The savings to the economy from reduced re-offending.</b><br> For KT participants with an offending background, this meausure estimates the change in the number of offences seperately for males and females from KT programme participation and converts into £ value to the econcomy using the GMCA unit cost data for offences.</p><br>
                    
                    For further details of methods and sources please see the SROI technical report.
                             "))
            )
          ),
          
          # ---- TAB Data Sources ----
          nav_panel(
            "Data Sources",
            card(
              card_header("Data Sources"),
              card_body(HTML("<h3>Kings Trust Sources:</h3>
                             <p><b>Participants</B> extracted 22-08-2025, covering 01-04-2019 to 31-03-2025</p>
                             <p><b>Outcomes Survey</B> extracted 10-07-2025, covering 20-01-2022 to 16-05-2025</p>
                             <p><b>HSC Outcomes</B> extracted 20-05-2025, covering 02-10-2023 to 19-05-2025</p>
                             <p><b>My Journey</B> extracted 20-05-2025, covering 01-04-2019 to 30-04-2025</p>
                             <p><b>Wellbeing Survey HSC data</B> extracted 24-06-2025, covering 01-04-2024 to 24-06-2025</p>
                             <p><b>The Prince's Trust Wellbeing Survey</B> extracted 27-05-2025, 01-03-2023</p>
                             <p><b>Health and Social care outcomes</B> extracted 20-05-2025, covering 02-10-2023 to 19-05-2025</p>
                             <p><b>Volunteer data</B> extracted 10-06-2025, as at 19-06-2025</p>
                             <p><b>Number of Volunteers Supporting Young People</B> extracted 25-06-2025, 2018-2024</p>
                             <h3>External sources</h3>
                             <p><b>Annual Population Survey, ONS</b> Used for deadweight calculations
                             <p><b>Labour productivity (GVA), ONS</b> Used for GVA calculations
                             <p><b>Wellbeing, ONS</b> Used for deadweight calculations
                             <p><b>Consumer prices index (housing), ONS</b> Used for deflators
                             <p><b>CBA Unit Costs Database, Greater Manchester Combined Authority</b> Used for unit costs of offending and DWP/health admin savings
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
  
  #dynamic count of years selected for text boxes
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
         The `Time Series` tab gives results over time. Use the blue buttons on the left in that tab to add one of the five components to the chart and also use the filters to select sub-groups of interest, and the year slider to select a range of years.<br>"),
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
              p(paste0(state$parent, 
                       ": ", 
                state$subgroup)),
              showcase = bs_icon("clipboard-data"),
              theme = value_box_theme(bg = kt_colors[1]), 
              
              fill = TRUE)
  })
  
  #selected subgroup value to use in data filter tab2 version
  output$t2_totalbox <- renderUI({
    
    # data logic
    selected <- state2$subgroup
    filtered <- df[df$group %in% selected, ]
    filtered <- filtered[filtered$`Cohort years` %in% selected_years(), ]
    total <- sum(filtered$`Total savings`, na.rm = TRUE)
    
    value_box(title = paste0("Total ","social value over ", number_years_seleted(), " years", " from ",selected_years()[1] , " to ",last(selected_years()) ), #make the 
              #value = custom_number_format(df_ten_yr$`Total savings`[df_ten_yr$group == "-"]), #pipe this in from the data
              value = custom_number_format(total),
              p(paste0(state2$parent, 
                ": ", 
                state2$subgroup)),
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
      #onclick = "Shiny.setInputValue('select_econ_value', Math.random())",
      onclick = "Shiny.setInputValue('select_econ_value', 'econ', {priority: 'event'})",
      
      value_box(
        title = paste0("Economic (",number_years_seleted(), "yr)"),
        #showcase = bs_icon("bank", size = "1.5rem"),
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
      #onclick = "Shiny.setInputValue('select_off_value', Math.random())",
      onclick = "Shiny.setInputValue('select_off_value', 'off', {priority: 'event'})",
      
      value_box(
        title = paste0("Re-offending (",number_years_seleted(), "yr)"),
        #showcase = bs_icon("sign-stop", size = "1.5rem"),
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
      #onclick = "Shiny.setInputValue('select_dwp_value', Math.random())",
      onclick = "Shiny.setInputValue('select_dwp_value', 'dwp', {priority: 'event'})",
      
      value_box(
        title = paste0("DWP/health (",number_years_seleted(), "yr)"),
        #showcase = bs_icon("pencil", size = "1.5rem"),
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
      #onclick = "Shiny.setInputValue('select_well_value', Math.random())",
      onclick = "Shiny.setInputValue('select_well_value', 'well', {priority: 'event'})",
      
      value_box(
        title = paste0("Wellbeing (",number_years_seleted(), "yr)"),
        #showcase = bs_icon("sun", size = "1.5rem"),
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
      #onclick = "Shiny.setInputValue('select_vol_value', Math.random())",
      onclick = "Shiny.setInputValue('select_vol_value', 'vol', {priority: 'event'})",
      value_box(
        title = paste0("Volunteering (",number_years_seleted(), "yr)"),
        #showcase = bs_icon("person-arms-up", size = "1.5rem"),
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
  
  
  # #  1 Define the mapping (static, non-reactive)
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

  # observeEvent(input$select_econ_value, {
  #   selected_metric(metric_map[["select_econ_value"]])
  #   selected_metric_id("select_econ_value")
  # })
  
  observeEvent(input$select_econ_value, {
    metric_id <- "select_econ_value"
    metric_col <- metric_map[[metric_id]]
    
    if (isTRUE(selected_metric_id() == metric_id)) {
      # SECOND CLICK → unselect
      selected_metric(NULL)
      selected_metric_id(NULL)
      
      session$sendCustomMessage("removeSelectedClass", "select_econ_value_box")
      
    } else {
      # FIRST CLICK → select
      selected_metric(metric_col)
      selected_metric_id(metric_id)
      
      session$sendCustomMessage("addSelectedClass", "select_econ_value_box")
    }
  })
  
  
  observeEvent(input$select_off_value, {
    metric_id <- "select_off_value"
    metric_col <- metric_map[[metric_id]]
    
    if (isTRUE(selected_metric_id() == metric_id)) {
      # SECOND CLICK → unselect
      selected_metric(NULL)
      selected_metric_id(NULL)
      
      session$sendCustomMessage("removeSelectedClass", "select_off_value_box")
      
    } else {
      # FIRST CLICK → select
      selected_metric(metric_col)
      selected_metric_id(metric_id)
      
      session$sendCustomMessage("addSelectedClass", "select_off_value_box")
    }
  })
  
  
  
  observeEvent(input$select_dwp_value, {
    metric_id <- "select_dwp_value"
    metric_col <- metric_map[[metric_id]]
    
    if (isTRUE(selected_metric_id() == metric_id)) {
      # SECOND CLICK → unselect
      selected_metric(NULL)
      selected_metric_id(NULL)
      
      session$sendCustomMessage("removeSelectedClass", "select_dwp_value_box")
      
    } else {
      # FIRST CLICK → select
      selected_metric(metric_col)
      selected_metric_id(metric_id)
      
      session$sendCustomMessage("addSelectedClass", "select_dwp_value_box")
    }
  })
  
  
  
  observeEvent(input$select_well_value, {
    metric_id <- "select_well_value"
    metric_col <- metric_map[[metric_id]]
    
    if (isTRUE(selected_metric_id() == metric_id)) {
      # SECOND CLICK → unselect
      selected_metric(NULL)
      selected_metric_id(NULL)
      
      session$sendCustomMessage("removeSelectedClass", "select_well_value_box")
      
    } else {
      # FIRST CLICK → select
      selected_metric(metric_col)
      selected_metric_id(metric_id)
      
      session$sendCustomMessage("addSelectedClass", "select_well_value_box")
    }
  })
  
  
  
  observeEvent(input$select_vol_value, {
    metric_id <- "select_vol_value"
    metric_col <- metric_map[[metric_id]]
    
    if (isTRUE(selected_metric_id() == metric_id)) {
      # SECOND CLICK → unselect
      selected_metric(NULL)
      selected_metric_id(NULL)
      
      session$sendCustomMessage("removeSelectedClass", "select_vol_value_box")
      
    } else {
      # FIRST CLICK → select
      selected_metric(metric_col)
      selected_metric_id(metric_id)
      
      session$sendCustomMessage("addSelectedClass", "select_vol_value_box")
    }
  })
  
# 
#   observeEvent(input$select_off_value, {
#     selected_metric(metric_map[["select_off_value"]])
#     selected_metric_id("select_off_value")
#   })
# 
#   observeEvent(input$select_dwp_value, {
#     selected_metric(metric_map[["select_dwp_value"]])
#     selected_metric_id("select_dwp_value")
#   })
# 
#   observeEvent(input$select_well_value, {
#     selected_metric(metric_map[["select_well_value"]])
#     selected_metric_id("select_well_value")
#   })
# 
#   observeEvent(input$select_vol_value, {
#     selected_metric(metric_map[["select_vol_value"]])
#     selected_metric_id("select_vol_value")
#   })
#   
  
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
    req(selected_metric())
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% 
      filter(group == input$filter4) %>%  # <<<< INTERACTIVE INPUT HERE
      filter(`Cohort years` %in% selected_years()) %>% 
      select(c(`Cohort years`, group, group_type, `Cohort count`, selected_column = all_of(selected_metric())))
  })
  
  #aspect for comparison
  data_highchart_aspect_sub2 <- reactive({
    req(selected_metric())
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
    req(selected_metric())
    #set up the df to feed the chart. This will change depending on user inputs
    df_all %>% 
      select(c(`Cohort years`, group, group_type, `Cohort count`, 
               selected_column = all_of(selected_metric())))
  })
  
  ###############
  ############### Tab2 TS chart
  ###############
  
  
  
  output$highchart_plot2 <- renderHighchart({
    
    # --- base chart (bars only, no metric needed) ---
    highchart2 <- highchart() %>% 
      hc_chart(type = "column") %>%
      hc_title(text = "", align = "left",
               style = list(fontSize ="24px",
                            fontFamily = "Arial",
                            fontWeight = "400")) %>% 
      hc_exporting(enabled = FALSE) %>% 
      hc_xAxis(categories = data_highchart_total_sub()$`Cohort years`,
               title = list(text = "")) %>% 
      hc_yAxis(labels = list(
        formatter = JS(" function() { return '£' + Highcharts.numberFormat(this.value / 1e6, 0, '.', ',') + 'M</b>'; } ")
      )) %>% 
      hc_plotOptions(
        column = list(
          borderRadius = 5,
          animation = FALSE,
          grouping = TRUE
        )
      ) %>% 
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(" function() { return '£' + Highcharts.numberFormat(this.y / 1e6, 0, '.', ',') + 'M</b>'; } ")
      ) %>% 
      hc_add_theme(kt_theme) %>% 
      hc_exporting(enabled = TRUE,
                   buttons = list(
                     contextButton = list(
                       menuItems = c("downloadSVG","downloadPNG","downloadXLS")
                     )))
    
    # subgroup bars
    if (input$filter4 != "all") {
      highchart2 <- highchart2 %>%
        hc_add_series(
          name  = paste0("Total SROI: ", unique(data_highchart_total_sub()$group)),
          data  = data_highchart_total_sub()$`Total savings`,
          type  = "column",
          stack = "Main",
          color = kt_colors[1],
          groupPadding = 0.2,
          maxPointWidth = 120,
          zIndex = 2
        )
    }
    
    # comparison bars
    if (input$filter4b != "-") {
      highchart2 <- highchart2 %>%
        hc_add_series(
          name  = paste0("Total SROI: ", unique(data_highchart_total_sub2()$group)),
          data  = data_highchart_total_sub2()$`Total savings`,
          type  = "column",
          stack = "Comparison",
          color = kt_colors[2],
          groupPadding = 0.2,
          maxPointWidth = 120,
          zIndex = 2
        )
    }
    
    # --- only add lines if a metric is selected ---
    if (!is.null(selected_metric())) {
      
      # All
      cht_data  <- data_highchart_aspect_all() %>% pull(selected_column)
      cht_series <- lapply(seq_along(cht_data), function(i) {
        if (i == length(cht_data)) {
          list(
            y = cht_data[i],
            dataLabels = list(
              enabled  = TRUE,
              align    = "left",
              y        = 15,
              crop     = FALSE,
              overflow = "allow",
              format   = "{series.name}"
            )
          )
        } else {
          list(y = cht_data[i])
        }
      })
      
      # Subgroup
      cht_data_sub <- data_highchart_aspect_sub() %>% pull(selected_column)
      cht_series_sub <- lapply(seq_along(cht_data_sub), function(i) {
        if (i == length(cht_data_sub)) {
          list(
            y = cht_data_sub[i],
            dataLabels = list(
              enabled  = TRUE,
              align    = "right",
              y        = 15,
              crop     = FALSE,
              overflow = "allow",
              format   = "{series.name}"
            )
          )
        } else {
          list(y = cht_data_sub[i])
        }
      })
      
      # Comparison
      cht_data_sub2 <- data_highchart_aspect_sub2() %>% pull(selected_column)
      cht_series_sub2 <- lapply(seq_along(cht_data_sub2), function(i) {
        if (i == length(cht_data_sub2)) {
          list(
            y = cht_data_sub2[i],
            dataLabels = list(
              enabled  = TRUE,
              align    = "right",
              y        = 15,
              crop     = FALSE,
              overflow = "allow",
              format   = "{series.name}",
              style    = list(fontWeight = "normal")
            )
          )
        } else {
          list(y = cht_data_sub2[i])
        }
      })
      
      # add subgroup line
      if (input$filter4 != "all") {
        highchart2 <- highchart2 %>%
          hc_add_series(
            data = cht_series_sub,
            type = "line",
            name = paste0(selected_metric(), ":<br>",
                          unique(data_highchart_aspect_sub()$group)),
            color  = kt_colors[11],
            zIndex = 51,
            marker = list(symbol = "circle"),
            dataLabels = list(enabled = FALSE)
          )
      }
      
      # add comparison line
      if (input$filter4b != "-") {
        highchart2 <- highchart2 %>%
          hc_add_series(
            data = cht_series_sub2,
            type = "line",
            name = paste0(selected_metric(), ":<br>",
                          unique(data_highchart_aspect_sub2()$group)),
            color  = kt_colors[7],
            zIndex = 51,
            marker = list(symbol = "circle"),
            dataLabels = list(enabled = FALSE)
          )
      }
    }
    
    highchart2
  })
  
}

shinyApp(ui, server)
