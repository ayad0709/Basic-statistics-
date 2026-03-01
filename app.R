# File: app.R
# Purpose: Exhaustive Statistical Dashboard with Dynamic UI and Fixed MathJax
# Author: R Code Wizard

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(moments)

# Increase upload limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# 1. UI SETUP -------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "StatsWizard Exhaustive"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Management", tabName = "data_mgmt", icon = icon("database")),
      menuItem("Exhaustive Statistics", tabName = "stats", icon = icon("calculator"))
    ),
    hr(),
    div(style = "padding: 10px;",
        h4("1. Load Data"),
        fileInput("file_upload", "Upload File", accept = c(".csv", ".xlsx", ".xls")),
        checkboxInput("use_demo", "Use Demo Data (mtcars)", FALSE),
        uiOutput("sidebar_controls"),
        br(),
        actionButton("show_settings", " Stat Settings", icon = icon("cog"), class = "btn-block")
    )
  ),
  
  dashboardBody(
    # Initialize MathJax and add the JavaScript Fix for dynamic UI rendering
    withMathJax(),
    tags$head(
      tags$script(HTML("
        $(document).on('shiny:value', function(event) {
          if (event.name === 'exhaustive_stats_ui') {
            setTimeout(function() {
              if (window.MathJax) {
                MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
              }
            }, 300); // 300ms delay ensures HTML is fully rendered before MathJax scans
          }
        });
      ")),
      tags$style(HTML("
        .stat-card { border-top: 3px solid #3c8dbc; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .math-box { background: #fdfdfd; padding: 10px; border-radius: 4px; margin-bottom: 15px; border-left: 4px solid #3c8dbc; font-size: 0.95em; overflow-x: auto;}
        .group-header { padding: 10px; background: #ecf0f5; border-left: 5px solid #222d32; margin: 20px 0; font-weight: bold;}
        .stat-label { font-weight: 600; color: #444; }
        .stat-val { float: right; color: #005a87; font-family: monospace; font-size: 1.1em;}
      "))
    ),
    
    tabItems(
      tabItem(tabName = "data_mgmt",
              valueBoxOutput("data_status", width = 12),
              box(title = "Dataset Preview", width = 12, status="primary", DTOutput("raw_table"))),
      
      tabItem(tabName = "stats",
              uiOutput("exhaustive_stats_ui"))
    )
  )
)

# 2. SERVER LOGIC ---------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Exhaustive Stat Choices ---
  all_stats_options <- c(
    "Valid N" = "n", "Missing (NA)" = "n_na", 
    "Mean" = "mean", "Geometric Mean" = "geom_mean", "Harmonic Mean" = "harm_mean",
    "Median" = "median", "Mode (Estimate)" = "mode",
    "Std. Deviation" = "sd", "Variance" = "var", "Std. Error (SE)" = "se",
    "Coeff. of Variation (CV%)" = "cv", "Med. Abs. Deviation (MAD)" = "mad",
    "Interquartile Range (IQR)" = "iqr", "Minimum" = "min", 
    "1st Quartile (Q1)" = "q1", "3rd Quartile (Q3)" = "q3", "Maximum" = "max", 
    "Range" = "range", "Skewness" = "skew", "Kurtosis" = "kurt"
  )
  
  # --- Stat Selection Modal ---
  observeEvent(input$show_settings, {
    showModal(modalDialog(
      title = "Select Exhaustive Statistics to Display",
      checkboxGroupInput("selected_stats", "Choose Measures:",
                         choices = all_stats_options,
                         selected = c("n", "n_na", "mean", "median", "sd", "var", "se", "min", "max", "skew", "kurt")),
      footer = modalButton("Save & Close"),
      size = "m", easyClose = TRUE
    ))
  })
  
  # --- Reactive Data Loading ---
  raw_data <- reactive({
    if (input$use_demo) return(mtcars)
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    df <- switch(ext, 
                 csv = readr::read_csv(input$file_upload$datapath),
                 xls = readxl::read_xls(input$file_upload$datapath),
                 xlsx = readxl::read_xlsx(input$file_upload$datapath),
                 validate("Invalid file type"))
    return(df)
  })
  
  # --- Dynamic Sidebar ---
  output$sidebar_controls <- renderUI({
    df <- raw_data()
    req(df)
    num_cols <- df %>% select(where(is.numeric)) %>% colnames()
    char_cols <- df %>% select(where(~is.character(.) || is.factor(.))) %>% colnames()
    tagList(
      h4("2. Configure Analysis"),
      selectInput("target_vars", "Variables", choices = num_cols, multiple = TRUE, selected = num_cols[1]),
      selectInput("group_var", "Group By", choices = c("None", char_cols))
    )
  })
  
  # --- Exhaustive Math Engine ---
  calculate_exhaustive_stats <- function(x) {
    x_clean <- na.omit(x)
    n <- length(x_clean)
    if (n == 0) return(NULL)
    
    mean_val <- mean(x_clean)
    sd_val <- sd(x_clean)
    
    # Safe Geometric & Harmonic Means (requires strictly positive numbers)
    pos_x <- x_clean[x_clean > 0]
    geom_m <- if(length(pos_x) > 0) exp(mean(log(pos_x))) else NA
    harm_m <- if(length(pos_x) > 0) 1 / mean(1 / pos_x) else NA
    
    # Simple Mode estimator (most frequent value)
    mode_val <- as.numeric(names(sort(-table(x_clean))))[1]
    
    list(
      n = n, n_na = sum(is.na(x)),
      mean = mean_val, geom_mean = geom_m, harm_mean = harm_m,
      median = median(x_clean), mode = mode_val,
      sd = sd_val, var = var(x_clean), se = sd_val / sqrt(n),
      cv = ifelse(mean_val != 0, (sd_val / mean_val) * 100, NA),
      mad = mad(x_clean), iqr = IQR(x_clean),
      min = min(x_clean), q1 = quantile(x_clean, 0.25, names=F), 
      q3 = quantile(x_clean, 0.75, names=F), max = max(x_clean),
      range = max(x_clean) - min(x_clean),
      skew = moments::skewness(x_clean), kurt = moments::kurtosis(x_clean)
    )
  }
  
  # --- UI Generator for Stat Cards ---
  output$exhaustive_stats_ui <- renderUI({
    df <- raw_data()
    
    # Default stats if user hasn't opened modal yet
    active_stats <- if(is.null(input$selected_stats)) c("n", "mean", "sd", "skew") else input$selected_stats
    
    req(df, input$target_vars, active_stats)
    
    # Helper to build a single card
    build_card <- function(data_subset, var_name, group_label = NULL) {
      stats <- calculate_exhaustive_stats(data_subset[[var_name]])
      if(is.null(stats)) return(NULL)
      
      title_text <- if(is.null(group_label)) var_name else paste(var_name, "-", group_label)
      # Sanitize ID for plots so Shiny doesn't break on spaces/special chars
      safe_group <- if(is.null(group_label)) "global" else make.names(group_label)
      plot_id <- paste0("plot_", make.names(var_name), "_", safe_group)
      
      box(
        title = title_text, width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, class = "stat-card",
        
        # Fixed MathJax: Using \\[ \\] and the JS observer handles rendering
        div(class = "math-box",
            p("Mean: \\[ \\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\]"),
            p("Variance: \\[ s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2 \\]")
        ),
        
        tags$ul(class = "list-group",
                lapply(names(all_stats_options), function(stat_name) {
                  stat_key <- all_stats_options[[stat_name]]
                  if (stat_key %in% active_stats) {
                    val <- stats[[stat_key]]
                    display_val <- if(is.na(val)) "N/A" else round(val, 4)
                    tags$li(class="list-group-item", span(class="stat-label", stat_name), span(class="stat-val", display_val))
                  }
                })
        ),
        
        plotOutput(outputId = plot_id, height = "220px")
      )
    }
    
    # Generate UI Structure
    if (input$group_var == "None") {
      tagList(
        div(class="group-header", h3("Global Analysis")),
        fluidRow(lapply(input$target_vars, function(v) build_card(df, v)))
      )
    } else {
      groups <- unique(df[[input$group_var]])
      groups <- groups[!is.na(groups)]
      lapply(groups, function(g) {
        group_df <- df[df[[input$group_var]] == g, ]
        tagList(
          div(class="group-header", h3(paste("Group:", g))),
          fluidRow(lapply(input$target_vars, function(v) build_card(group_df, v, g)))
        )
      })
    }
  })
  
  # --- Dynamic Plotting Generator ---
  observe({
    df <- raw_data()
    req(df, input$target_vars)
    
    for (v in input$target_vars) {
      if (input$group_var == "None") {
        local({
          v_name <- v
          plot_id <- paste0("plot_", make.names(v_name), "_global")
          output[[plot_id]] <- renderPlot({
            ggplot(df, aes_string(x = v_name)) + 
              geom_histogram(aes(y = ..density..), fill="#3c8dbc", color="white", bins=30, alpha=0.7) + 
              geom_density(color="#d9534f", size=1) +
              theme_minimal() + labs(x = v_name, y = "Density")
          })
        })
      } else {
        groups <- unique(df[[input$group_var]])
        groups <- groups[!is.na(groups)]
        for (g in groups) {
          local({
            v_name <- v
            g_name <- g
            plot_id <- paste0("plot_", make.names(v_name), "_", make.names(g_name))
            
            output[[plot_id]] <- renderPlot({
              plot_df <- df[df[[input$group_var]] == g_name, ]
              ggplot(plot_df, aes_string(x = v_name)) + 
                geom_density(fill="#00a65a", alpha=0.4, color="#008d4c", size=1) + 
                theme_minimal() + labs(x = v_name, y = "Density")
            })
          })
        }
      }
    }
  })
  
  # --- Data Status & Table ---
  output$data_status <- renderValueBox({
    if (is.null(raw_data())) valueBox("Waiting", "Upload data to start", icon = icon("upload"), color = "red")
    else valueBox(nrow(raw_data()), "Rows Active", icon = icon("check-circle"), color = "green")
  })
  
  output$raw_table <- renderDT({ 
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 5, scrollX = TRUE)) 
  })
}

shinyApp(ui, server)