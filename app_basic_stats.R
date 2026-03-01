# Fichier : app.R
# Objectif : Tableau de bord statistique avec Intervalle de Confiance à 95%
# Auteur : R Code Wizard

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(moments)

# Augmenter la limite de taille des fichiers à 30 Mo
options(shiny.maxRequestSize = 30 * 1024^2)

# --- 1. DICTIONNAIRE STATISTIQUE (Explications & Mathématiques) ---
stat_dict <- list(
  "n" = list(desc = "Le nombre total d'observations valides (non manquantes) dans l'échantillon.", math = ""),
  "n_na" = list(desc = "Le nombre de valeurs manquantes (NA) dans l'ensemble de données.", math = ""),
  "mean" = list(desc = "La moyenne arithmétique représente le « centre de gravité » de la distribution.", math = "\\[ \\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\]"),
  "ci_mean" = list(desc = "L'Intervalle de Confiance (IC) de la moyenne à 95% (alpha = 5%). Il indique la plage de valeurs dans laquelle on a 95% de chances de trouver la véritable moyenne de la population globale. Calculé avec la loi de Student.", math = "\\[ IC_{95\\%} = \\left[ \\bar{x} - t_{0.975, n-1} \\frac{s}{\\sqrt{n}} \\, ; \\, \\bar{x} + t_{0.975, n-1} \\frac{s}{\\sqrt{n}} \\right] \\]"),
  "geom_mean" = list(desc = "La moyenne géométrique est la racine n-ième du produit de toutes les valeurs. (Note : nécessite des valeurs > 0).", math = "\\[ \\left(\\prod_{i=1}^n x_i\\right)^{\\frac{1}{n}} \\]"),
  "harm_mean" = list(desc = "La moyenne harmonique est l'inverse de la moyenne arithmétique des inverses des valeurs.", math = "\\[ \\frac{n}{\\sum_{i=1}^n \\frac{1}{x_i}} \\]"),
  "median" = list(desc = "La médiane est la valeur exacte qui coupe l'ensemble des données triées en deux moitiés égales.", math = ""),
  "mode" = list(desc = "Le mode est la valeur (ou les valeurs) qui apparaît le plus fréquemment dans l'ensemble de données.", math = ""),
  "sd" = list(desc = "L'écart-type mesure la dispersion ou l'étalement des données autour de la moyenne.", math = "\\[ s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2} \\]"),
  "var" = list(desc = "La variance est la moyenne des carrés des écarts par rapport à la moyenne.", math = "\\[ s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2 \\]"),
  "se" = list(desc = "L'Erreur Standard de la moyenne (SE) estime à quel point la moyenne de votre échantillon est susceptible de différer de la véritable moyenne.", math = "\\[ SE = \\frac{s}{\\sqrt{n}} \\]"),
  "cv" = list(desc = "Le Coefficient de Variation (en %) est le rapport entre l'écart-type et la moyenne.", math = "\\[ CV = \\frac{s}{\\bar{x}} \\times 100 \\]"),
  "mad" = list(desc = "L'Écart Absolu Médian (MAD) est une mesure très robuste de la variabilité d'un échantillon quantitatif.", math = "\\[ MAD = \\text{médiane}(|x_i - \\tilde{x}|) \\]"),
  "iqr" = list(desc = "L'Écart Interquartile (IQR) est la différence entre le 3ème quartile (Q3) et le 1er quartile (Q1).", math = "\\[ IQR = Q_3 - Q_1 \\]"),
  "min" = list(desc = "La valeur minimale absolue observée dans l'ensemble de données.", math = ""),
  "q1" = list(desc = "Le Premier Quartile (Q1). Exactement 25% des observations sont inférieures ou égales à cette valeur.", math = ""),
  "q3" = list(desc = "Le Troisième Quartile (Q3). Exactement 75% des observations sont inférieures ou égales à cette valeur.", math = ""),
  "max" = list(desc = "La valeur maximale absolue observée dans l'ensemble de données.", math = ""),
  "range" = list(desc = "L'étendue (Range) est la différence brute entre la plus grande valeur et la plus petite valeur.", math = "\\[ R = x_{\\max} - x_{\\min} \\]"),
  "skew" = list(desc = "L'Asymétrie (Skewness) évalue l'équilibre de la distribution autour de sa moyenne.", math = "\\[ \\gamma_1 = \\frac{\\sum_{i=1}^n (x_i - \\bar{x})^3}{(n-1)s^3} \\]"),
  "kurt" = list(desc = "L'Aplatissement (Kurtosis) évalue la propension de la distribution à produire des valeurs extrêmes.", math = "\\[ K = \\frac{\\sum_{i=1}^n (x_i - \\bar{x})^4}{(n-1)s^4} \\]")
)

# --- 2. INTERFACE UTILISATEUR (UI) ---
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "StatsWizard Pro"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gestion des Données", tabName = "data_mgmt", icon = icon("database")),
      menuItem("Statistiques Exhaustives", tabName = "stats", icon = icon("calculator"))
    ),
    hr(),
    div(style = "padding: 10px;",
        h4("1. Charger les Données"),
        
        radioButtons("data_source", "Source des données :", 
                     choices = c("Téléverser un Fichier" = "file", "Jeux de Démo" = "demo"), 
                     selected = "demo"),
        
        conditionalPanel(
          condition = "input.data_source == 'file'",
          fileInput("file_upload", "Choisir un fichier", accept = c(".csv", ".xlsx", ".xls")),
          uiOutput("sheet_selector")
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'demo'",
          selectInput("demo_dataset", "Sélectionner un jeu de données :",
                      choices = c(
                        "Voitures (mtcars)" = "mtcars", "Diamants (diamonds)" = "diamonds",
                        "Fleurs d'Iris (iris)" = "iris", "Consommation Auto (mpg)" = "mpg",
                        "Qualité de l'Air (airquality)" = "airquality", "Séismes aux Fidji (quakes)" = "quakes",
                        "Croissance Dentaire (ToothGrowth)" = "ToothGrowth", "Éruptions Geyser (faithful)" = "faithful",
                        "Poids des Poulets (chickwts)" = "chickwts", "Mensuration Arbres (trees)" = "trees"
                      ), selected = "mtcars")
        ),
        
        hr(),
        uiOutput("sidebar_controls"),
        br(),
        div(align = "center",
            actionButton("show_settings", " Paramètres des Stats", icon = icon("cog"), class = "btn btn-default btn-sm")
        )
    )
  ),
  
  dashboardBody(
    withMathJax(),
    tags$head(
      tags$script(HTML("
        $(document).on('shiny:value', function(event) {
          if (event.name === 'exhaustive_stats_ui') {
            setTimeout(function() { if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]); }, 300);
          }
        });
        $(document).on('shown.bs.collapse', function(e) {
           if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub, e.target]);
        });
      ")),
      tags$style(HTML("
        .stat-card { border-top: 3px solid #3c8dbc; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .group-header { padding: 10px; background: #ecf0f5; border-left: 5px solid #222d32; margin: 20px 0; font-weight: bold;}
        .stat-row { cursor: pointer; transition: background-color 0.2s; }
        .stat-row:hover { background-color: #eef7fa !important; }
        .stat-label { font-weight: 600; color: #444; }
        .stat-val { float: right; color: #005a87; font-family: monospace; font-size: 1.1em; font-weight: bold;}
        .stat-explanation { background-color: #fcfcfc; border-left: 4px solid #f39c12; padding: 15px; margin: 0; font-size: 0.95em; color: #333; border-bottom: 1px solid #ddd; line-height: 1.5;}
        .math-text { overflow-x: auto; margin-top: 10px; padding-top: 10px; border-top: 1px dashed #ccc;}
      "))
    ),
    
    tabItems(
      tabItem(tabName = "data_mgmt",
              valueBoxOutput("data_status", width = 12),
              box(title = "Aperçu du jeu de données", width = 12, status="primary", DTOutput("raw_table"))),
      tabItem(tabName = "stats", uiOutput("exhaustive_stats_ui"))
    )
  )
)

# --- 3. LOGIQUE SERVEUR (SERVER) ---
server <- function(input, output, session) {
  
  # Ajout de l'IC à la liste des options
  all_stats_options <- c(
    "N Valides" = "n", "Manquants (NA)" = "n_na", "Moyenne" = "mean", "IC à 95% (Moyenne)" = "ci_mean",
    "Moyenne Géométrique" = "geom_mean", "Moyenne Harmonique" = "harm_mean", "Médiane" = "median", 
    "Mode (Estimation)" = "mode", "Écart-type" = "sd", "Variance" = "var", "Erreur Standard (SE)" = "se",
    "Coeff. de Variation (CV%)" = "cv", "Écart Absolu Médian (MAD)" = "mad",
    "Écart Interquartile (IQR)" = "iqr", "Minimum" = "min", "1er Quartile (Q1)" = "q1", 
    "3ème Quartile (Q3)" = "q3", "Maximum" = "max", "Étendue (Range)" = "range", 
    "Asymétrie (Skewness)" = "skew", "Aplatissement (Kurtosis)" = "kurt"
  )
  
  observeEvent(input$show_settings, {
    showModal(modalDialog(
      title = "Sélectionner les statistiques à afficher",
      # L'IC 95% est désormais coché par défaut
      checkboxGroupInput("selected_stats", "Choisissez les mesures :", choices = all_stats_options,
                         selected = c("n", "mean", "ci_mean", "median", "sd", "var", "min", "max", "skew", "kurt")),
      footer = modalButton("Enregistrer & Fermer"), size = "m", easyClose = TRUE
    ))
  })
  
  output$sheet_selector <- renderUI({
    req(input$data_source == "file", input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext %in% c("xls", "xlsx")) {
      sheets <- readxl::excel_sheets(input$file_upload$datapath)
      selectInput("sheet_select", "Feuille de calcul :", choices = sheets)
    }
  })
  
  raw_data <- reactive({
    if (input$data_source == "demo") {
      req(input$demo_dataset)
      return(as.data.frame(get(input$demo_dataset)))
    } else {
      req(input$file_upload)
      ext <- tools::file_ext(input$file_upload$name)
      path <- input$file_upload$datapath
      
      if (ext == "csv") {
        return(readr::read_csv(path))
      } else if (ext %in% c("xls", "xlsx")) {
        sheets <- readxl::excel_sheets(path)
        sheet_to_read <- if (!is.null(input$sheet_select) && input$sheet_select %in% sheets) {
          input$sheet_select
        } else {
          sheets[1]
        }
        return(readxl::read_excel(path, sheet = sheet_to_read))
      } else {
        validate("Type de fichier invalide")
      }
    }
  })
  
  output$sidebar_controls <- renderUI({
    df <- raw_data(); req(df)
    num_cols <- df %>% select(where(is.numeric)) %>% colnames()
    char_cols <- df %>% select(where(~is.character(.) || is.factor(.))) %>% colnames()
    tagList(
      h4("2. Configurer l'Analyse"),
      selectInput("target_vars", "Variables Cibles", choices = num_cols, multiple = TRUE, selected = num_cols[1]),
      selectInput("group_var", "Grouper par", choices = c("Aucun", char_cols))
    )
  })
  
  calculate_exhaustive_stats <- function(x) {
    x_clean <- na.omit(x); n <- length(x_clean)
    if (n == 0) return(NULL)
    
    mean_val <- mean(x_clean)
    sd_val <- sd(x_clean)
    se_val <- sd_val / sqrt(n)
    
    # Calcul de l'Intervalle de Confiance à 95% (loi de Student)
    ci_margin <- if(n > 1 && !is.na(sd_val)) qt(0.975, df = n-1) * se_val else NA
    ci_str <- if(!is.na(ci_margin)) paste0("[", round(mean_val - ci_margin, 3), " ; ", round(mean_val + ci_margin, 3), "]") else "N/A"
    
    pos_x <- x_clean[x_clean > 0]
    
    list(
      n = n, n_na = sum(is.na(x)), mean = mean_val, 
      ci_mean = ci_str, # Intégration de l'IC textuel
      geom_mean = if(length(pos_x) > 0) exp(mean(log(pos_x))) else NA, 
      harm_mean = if(length(pos_x) > 0) 1 / mean(1 / pos_x) else NA,
      median = median(x_clean), mode = as.numeric(names(sort(-table(x_clean))))[1],
      sd = sd_val, var = var(x_clean), se = se_val,
      cv = ifelse(mean_val != 0, (sd_val / mean_val) * 100, NA),
      mad = mad(x_clean), iqr = IQR(x_clean), min = min(x_clean), 
      q1 = quantile(x_clean, 0.25, names=F), q3 = quantile(x_clean, 0.75, names=F), 
      max = max(x_clean), range = max(x_clean) - min(x_clean),
      skew = moments::skewness(x_clean), kurt = moments::kurtosis(x_clean)
    )
  }
  
  output$exhaustive_stats_ui <- renderUI({
    df <- raw_data()
    # Par défaut on inclut l'intervalle de confiance
    active_stats <- if(is.null(input$selected_stats)) c("n", "mean", "ci_mean", "sd", "skew") else input$selected_stats
    req(df, input$target_vars, active_stats)
    
    build_card <- function(data_subset, var_name, group_label = NULL) {
      stats <- calculate_exhaustive_stats(data_subset[[var_name]])
      if(is.null(stats)) return(NULL)
      
      title_text <- if(is.null(group_label)) var_name else paste(var_name, "-", group_label)
      safe_group <- if(is.null(group_label)) "global" else make.names(group_label)
      plot_id <- paste0("plot_", make.names(var_name), "_", safe_group)
      
      list_items <- lapply(names(all_stats_options), function(stat_name) {
        stat_key <- all_stats_options[[stat_name]]
        if (stat_key %in% active_stats) {
          val <- stats[[stat_key]]
          
          # NOUVEAU : Logique d'affichage qui gère les nombres ET le texte (comme l'IC)
          display_val <- if (is.null(val) || length(val) == 0 || is.na(val[1])) {
            "N/A"
          } else if (is.numeric(val)) {
            round(val, 4)
          } else {
            val # Si c'est un texte (comme "[12.3 ; 15.6]"), l'afficher directement
          }
          
          collapse_id <- paste0("collapse_", stat_key, "_", make.names(var_name), "_", safe_group)
          
          tagList(
            tags$li(class="list-group-item stat-row", `data-toggle`="collapse", `data-target`=paste0("#", collapse_id),
                    icon("info-circle", class="text-info"), " ", 
                    span(class="stat-label", stat_name), 
                    span(class="stat-val", display_val)),
            tags$div(id = collapse_id, class = "collapse stat-explanation",
                     p(strong("Explication : "), stat_dict[[stat_key]]$desc),
                     if(stat_dict[[stat_key]]$math != "") div(class="math-text", stat_dict[[stat_key]]$math) else NULL
            )
          )
        }
      })
      
      box(title = title_text, width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, class = "stat-card",
          tags$ul(class = "list-group", list_items),
          plotOutput(outputId = plot_id, height = "220px")
      )
    }
    
    if (input$group_var == "Aucun") {
      tagList(div(class="group-header", h3("Analyse Globale")), fluidRow(lapply(input$target_vars, function(v) build_card(df, v))))
    } else {
      groups <- na.omit(unique(df[[input$group_var]]))
      lapply(groups, function(g) {
        tagList(div(class="group-header", h3(paste("Groupe :", g))),
                fluidRow(lapply(input$target_vars, function(v) build_card(df[df[[input$group_var]] == g, ], v, g))))
      })
    }
  })
  
  observe({
    df <- raw_data(); req(df, input$target_vars)
    for (v in input$target_vars) {
      if (input$group_var == "Aucun") {
        local({
          v_name <- v; plot_id <- paste0("plot_", make.names(v_name), "_global")
          output[[plot_id]] <- renderPlot({
            ggplot(df, aes(x = .data[[v_name]])) + 
              geom_histogram(aes(y = after_stat(density)), fill="#3c8dbc", color="white", bins=30, alpha=0.7) + 
              geom_density(color="#d9534f", linewidth=1) + 
              theme_minimal() + 
              labs(x = v_name, y = "Densité")
          })
        })
      } else {
        groups <- na.omit(unique(df[[input$group_var]]))
        for (g in groups) {
          local({
            v_name <- v; g_name <- g; plot_id <- paste0("plot_", make.names(v_name), "_", make.names(g_name))
            output[[plot_id]] <- renderPlot({
              plot_df <- df[df[[input$group_var]] == g_name, ]
              ggplot(plot_df, aes(x = .data[[v_name]])) + 
                geom_density(fill="#00a65a", alpha=0.4, color="#008d4c", linewidth=1) + 
                theme_minimal() + 
                labs(x = v_name, y = "Densité")
            })
          })
        }
      }
    }
  })
  
  output$data_status <- renderValueBox({
    if (is.null(raw_data())) valueBox("En attente", "Téléversez des données pour commencer", icon = icon("upload"), color = "red")
    else valueBox(nrow(raw_data()), "Lignes actives", icon = icon("check-circle"), color = "green")
  })
  output$raw_table <- renderDT({ req(raw_data()); datatable(raw_data(), options = list(pageLength = 5, scrollX = TRUE)) })
}

shinyApp(ui, server)