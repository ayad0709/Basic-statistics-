# Fichier : app.R
# Objectif : Tableau de bord statistique ULTIME (Normalité, Outliers, Corrélations)
# Version : +++ démos (beaucoup), SANS boutons de download
# Auteur : R Code Wizard (corrigé mtcars/datasets)

library(shiny)
library(shinydashboard)
library(datasets)   # <-- important pour data() / datasets lazy
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(moments)
library(tidyr)

options(shiny.maxRequestSize = 30 * 1024^2)

# --- 1) DICTIONNAIRE STATISTIQUE ---
stat_dict <- list(
  "n" = list(desc = "Le nombre total d'observations valides.", math = ""),
  "n_na" = list(desc = "Le nombre de valeurs manquantes (NA) ignorées dans les calculs.", math = ""),
  "mean" = list(desc = "La moyenne arithmétique (centre de gravité).", math = "\\[ \\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\]"),
  "ci_mean" = list(desc = "IC à 95% de la moyenne (Student).", math = "\\[ IC_{95\\%} = \\left[ \\bar{x} - t_{0.975} \\frac{s}{\\sqrt{n}} \\, ; \\, \\bar{x} + t_{0.975} \\frac{s}{\\sqrt{n}} \\right] \\]"),
  "geom_mean" = list(desc = "Moyenne géométrique (taux de croissance).", math = "\\[ \\left(\\prod_{i=1}^n x_i\\right)^{\\frac{1}{n}} \\]"),
  "harm_mean" = list(desc = "Moyenne harmonique (ratios/vitesses).", math = "\\[ \\frac{n}{\\sum_{i=1}^n \\frac{1}{x_i}} \\]"),
  "median" = list(desc = "Médiane (50ème percentile), robuste.", math = ""),
  "mode" = list(desc = "Mode (valeur la plus fréquente).", math = ""),
  "sd" = list(desc = "Écart-type.", math = "\\[ s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2} \\]"),
  "var" = list(desc = "Variance.", math = "\\[ s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2 \\]"),
  "se" = list(desc = "Erreur standard de la moyenne.", math = "\\[ SE = \\frac{s}{\\sqrt{n}} \\]"),
  "cv" = list(desc = "Coefficient de variation (%)", math = "\\[ CV = \\frac{s}{\\bar{x}} \\times 100 \\]"),
  "mad" = list(desc = "MAD (très robuste).", math = "\\[ MAD = \\text{médiane}(|x_i - \\tilde{x}|) \\]"),
  "iqr" = list(desc = "IQR (Q3 - Q1).", math = "\\[ IQR = Q_3 - Q_1 \\]"),
  "min" = list(desc = "Minimum.", math = ""),
  "q1" = list(desc = "Premier quartile (25%).", math = ""),
  "q3" = list(desc = "Troisième quartile (75%).", math = ""),
  "max" = list(desc = "Maximum.", math = ""),
  "range" = list(desc = "Étendue (max-min).", math = "\\[ R = x_{\\max} - x_{\\min} \\]"),
  "skew" = list(desc = "Asymétrie (skewness).", math = "\\[ \\gamma_1 = \\frac{\\sum (x_i - \\bar{x})^3}{(n-1)s^3} \\]"),
  "kurt" = list(desc = "Kurtosis (aplatissement).", math = "\\[ K = \\frac{\\sum (x_i - \\bar{x})^4}{(n-1)s^4} \\]"),
  "shapiro" = list(desc = "Shapiro-Wilk (p-value). p<0.05 => non normal. (n<=5000)", math = ""),
  "outliers" = list(desc = "Nombre d'outliers (règle 1.5×IQR).", math = "\\[ x < Q_1 - 1.5IQR \\;\\;\\text{ou}\\;\\; x > Q_3 + 1.5IQR \\]")
)

# --- 2) JEUX DE DONNÉES DE DÉMO (beaucoup) ---
demo_choices <- c(
  # datasets (data.frame)
  "mtcars (voitures)" = "mtcars",
  "iris (fleurs)" = "iris",
  "airquality (qualité air)" = "airquality",
  "ToothGrowth (dentition)" = "ToothGrowth",
  "PlantGrowth (plantes)" = "PlantGrowth",
  "ChickWeight (poussins)" = "ChickWeight",
  "CO2 (plantes CO2)" = "CO2",
  "USArrests (crime)" = "USArrests",
  "warpbreaks (tissage)" = "warpbreaks",
  "sleep (sommesil)" = "sleep",
  "InsectSprays (insecticides)" = "InsectSprays",
  "Orange (croissance)" = "Orange",
  "npk (engrais)" = "npk",
  "attitude (attitudes)" = "attitude",
  "swiss (démographie)" = "swiss",
  "trees (arbres)" = "trees",
  "women (women)" = "women",
  "longley (macro)" = "longley",
  "stackloss (industrie)" = "stackloss",
  "cars (vitesse/arrêt)" = "cars",
  "pressure (pression)" = "pressure",
  "rock (roches)" = "rock",
  "faithful (geyser)" = "faithful",
  "faithfuld (densité geyser)" = "faithfuld",
  
  # ggplot2 (data.frame) - seulement si ggplot2 est là (il l'est)
  "diamonds (ggplot2)" = "diamonds",
  "mpg (ggplot2)" = "mpg",
  
  # tables -> df
  "HairEyeColor (table -> df)" = "HairEyeColor",
  "Titanic (table -> df)" = "Titanic",
  
  # ts -> df
  "EuStockMarkets (ts -> df)" = "EuStockMarkets",
  "Nile (ts -> df)" = "Nile",
  "AirPassengers (ts -> df)" = "AirPassengers",
  "LakeHuron (ts -> df)" = "LakeHuron",
  "UKGas (ts -> df)" = "UKGas"
)

# --- Chargement robuste d'un dataset demo ---
load_demo_dataset <- function(name) {
  # 1) ggplot2 d'abord (diamonds, mpg, etc.)
  obj <- tryCatch(get(name, envir = asNamespace("ggplot2")), error = function(e) NULL)
  if (!is.null(obj)) return(as.data.frame(obj))
  
  # 2) datasets via data() (robuste pour mtcars/iris/AirPassengers/Nile etc.)
  tmp <- new.env(parent = emptyenv())
  ok <- tryCatch({
    data(list = name, package = "datasets", envir = tmp)
    exists(name, envir = tmp, inherits = FALSE)
  }, error = function(e) FALSE)
  
  if (ok) {
    obj <- get(name, envir = tmp, inherits = FALSE)
  } else {
    # 3) fallback: objets déjà attachés ou global
    obj <- tryCatch(get(name, inherits = TRUE), error = function(e) NULL)
    if (is.null(obj)) stop("Dataset introuvable : ", name)
  }
  
  # 4) conversion stable
  if (is.data.frame(obj)) return(as.data.frame(obj))
  
  if (is.matrix(obj)) return(as.data.frame(obj, stringsAsFactors = FALSE))
  
  if (is.table(obj)) {
    df <- as.data.frame(obj, stringsAsFactors = FALSE)
    if ("Freq" %in% names(df)) df <- df[df$Freq > 0, , drop = FALSE]
    return(df)
  }
  
  if (is.ts(obj)) {
    tt <- time(obj)
    if (is.matrix(obj)) {
      df <- as.data.frame(obj)
      df$time <- as.numeric(tt)
      return(df)
    } else {
      return(data.frame(value = as.numeric(obj), time = as.numeric(tt)))
    }
  }
  
  as.data.frame(obj)
}

# --- 3) UI ---
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "StatsWizard Ultra Pro"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gestion des Données", tabName = "data_mgmt", icon = icon("database")),
      menuItem("Statistiques Exhaustives", tabName = "stats", icon = icon("calculator")),
      menuItem("Corrélations", tabName = "correlations", icon = icon("project-diagram"))
    ),
    hr(),
    div(style = "padding: 10px;",
        h4("1. Charger les Données"),
        
        radioButtons("data_source", "Source :",
                     choices = c("Téléverser un Fichier" = "file", "Jeux de Démo" = "demo"),
                     selected = "demo"),
        
        conditionalPanel(
          condition = "input.data_source == 'file'",
          fileInput("file_upload", "Choisir un fichier", accept = c(".csv", ".xlsx", ".xls")),
          uiOutput("sheet_selector")
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'demo'",
          selectInput("demo_dataset", "Sélectionner un dataset :",
                      choices = demo_choices,
                      selected = unname(demo_choices[1])),
          # helpText("Astuce : certains datasets sont des séries temporelles (ts) ou des tables -> conversion automatique.")
        ),
        
        hr(),
        uiOutput("sidebar_controls"),
        
        radioButtons("plot_type", "Visualisation :",
                     choices = c("Distribution (Densité)" = "dist",
                                 "Dispersion (Boxplot)" = "box"),
                     inline = TRUE),
        
        # checkboxInput("plot_log", "Échelle log10 (si val. > 0)", value = FALSE),
        checkboxInput("plot_drop_outliers", "Exclure outliers du tracé", value = FALSE),
        
        br(),
        div(align = "center",
            actionButton("show_settings", " Paramètres", icon = icon("cog"),
                         class = "btn btn-default btn-sm")
        )
    )
  ),
  
  dashboardBody(
    withMathJax(),
    tags$head(
      tags$script(HTML("
        $(document).on('shiny:value', function(event) {
          setTimeout(function() {
            if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
          }, 300);
        });
        $(document).on('shown.bs.collapse', function(e) {
          if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub, e.target]);
        });
      ")),
      tags$style(HTML("
        .stat-card { border-top: 3px solid #3c8dbc; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .group-header { padding: 10px; background: #ecf0f5; border-left: 5px solid #222d32; margin: 20px 0; font-weight: bold;}
        .stat-row { cursor: pointer; transition: 0.2s; } .stat-row:hover { background-color: #eef7fa !important; }
        .stat-label { font-weight: 600; color: #444; }
        .stat-val { float: right; color: #005a87; font-family: monospace; font-size: 1.1em; font-weight: bold;}
        .stat-explanation { background-color: #fcfcfc; border-left: 4px solid #f39c12; padding: 15px; font-size: 0.95em; border-bottom: 1px solid #ddd; }
        .math-text { overflow-x: auto; margin-top: 10px; padding-top: 10px; border-top: 1px dashed #ccc;}
      "))
    ),
    
    tabItems(
      tabItem(tabName = "data_mgmt",
              valueBoxOutput("data_status", width = 12),
              box(title = "Aperçu du jeu de données",
                  width = 12, status="primary",
                  DTOutput("raw_table"))),
      
      tabItem(tabName = "stats",
              uiOutput("exhaustive_stats_ui")),
      
      tabItem(tabName = "correlations",
              fluidRow(
                box(title = "Paramètres Corrélation", width = 12, status = "warning", solidHeader = TRUE,
                    fluidRow(
                      column(6,
                             selectInput("corr_method", "Méthode :",
                                         choices = c("Pearson" = "pearson",
                                                     "Spearman" = "spearman",
                                                     "Kendall" = "kendall"),
                                         selected = "pearson")
                      ),
                      column(6,
                             selectInput("corr_use", "Gestion des NA :",
                                         choices = c("pairwise.complete.obs (recommandé)" = "pairwise.complete.obs",
                                                     "complete.obs (lignes complètes uniquement)" = "complete.obs"),
                                         selected = "pairwise.complete.obs")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Matrice de Corrélation", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("corr_plot", height = "600px"))
              )
      )
    )
  )
)

# --- 4) SERVER ---
server <- function(input, output, session) {
  
  all_stats_options <- c(
    "N Valides" = "n",
    "Manquants (NA)" = "n_na",
    "Moyenne" = "mean",
    "IC à 95% (Moyenne)" = "ci_mean",
    "Moyenne Géométrique" = "geom_mean",
    "Moyenne Harmonique" = "harm_mean",
    "Médiane" = "median",
    "Mode" = "mode",
    "Écart-type" = "sd",
    "Variance" = "var",
    "Erreur Standard (SE)" = "se",
    "Coeff. de Variation (CV%)" = "cv",
    "Écart Absolu Médian (MAD)" = "mad",
    "Écart Interquartile (IQR)" = "iqr",
    "Minimum" = "min",
    "1er Quartile (Q1)" = "q1",
    "3ème Quartile (Q3)" = "q3",
    "Maximum" = "max",
    "Étendue (Range)" = "range",
    "Asymétrie (Skewness)" = "skew",
    "Aplatissement (Kurtosis)" = "kurt",
    "Normalité (p-value Shapiro)" = "shapiro",
    "Valeurs Aberrantes (N)" = "outliers"
  )
  
  observeEvent(input$show_settings, {
    showModal(modalDialog(
      title = "Sélectionner les statistiques à afficher",
      checkboxGroupInput(
        "selected_stats", "Choisissez les mesures :",
        choices = all_stats_options,
        selected = c("n", "mean", "ci_mean", "median", "sd", "skew", "shapiro", "outliers")
      ),
      footer = modalButton("Fermer"),
      size = "m", easyClose = TRUE
    ))
  })
  
  output$sheet_selector <- renderUI({
    req(input$data_source == "file", input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext %in% c("xls", "xlsx")) {
      selectInput("sheet_select", "Feuille :",
                  choices = readxl::excel_sheets(input$file_upload$datapath))
    }
  })
  
  raw_data <- reactive({
    if (input$data_source == "demo") {
      req(input$demo_dataset)
      load_demo_dataset(input$demo_dataset)
    } else {
      req(input$file_upload)
      ext <- tools::file_ext(input$file_upload$name)
      path <- input$file_upload$datapath
      
      if (ext == "csv") {
        readr::read_csv(path, show_col_types = FALSE)
      } else if (ext %in% c("xls", "xlsx")) {
        sheets <- readxl::excel_sheets(path)
        sheet_to_read <- if (!is.null(input$sheet_select) && input$sheet_select %in% sheets) input$sheet_select else sheets[1]
        readxl::read_excel(path, sheet = sheet_to_read)
      } else {
        validate(need(FALSE, "Fichier invalide (CSV ou Excel uniquement)."))
      }
    }
  })
  
  numeric_cols <- reactive({
    df <- raw_data(); req(df)
    names(df)[vapply(df, is.numeric, logical(1))]
  })
  
  groupable_cols <- reactive({
    df <- raw_data(); req(df)
    names(df)[vapply(df, function(z) is.character(z) || is.factor(z), logical(1))]
  })
  
  output$sidebar_controls <- renderUI({
    df <- raw_data(); req(df)
    num_cols <- numeric_cols()
    grp_cols <- groupable_cols()
    
    validate(need(length(num_cols) > 0, "Aucune colonne numérique détectée (impossible de calculer/plot)."))
    
    tagList(
      h4("2. Configurer"),
      selectInput("target_vars", "Variables Cibles",
                  choices = num_cols, multiple = TRUE,
                  selected = num_cols[1]),
      selectInput("group_var", "Grouper par",
                  choices = c("Aucun", grp_cols),
                  selected = "Aucun")
    )
  })
  
  # garder sélections valides si dataset change
  observeEvent(raw_data(), {
    num_cols <- numeric_cols()
    grp_cols <- groupable_cols()
    
    if (!is.null(input$target_vars)) {
      keep <- intersect(input$target_vars, num_cols)
      if (length(keep) == 0 && length(num_cols) > 0) keep <- num_cols[1]
      updateSelectInput(session, "target_vars", choices = num_cols, selected = keep)
    }
    
    valid_group <- c("Aucun", grp_cols)
    if (!is.null(input$group_var) && !(input$group_var %in% valid_group)) {
      updateSelectInput(session, "group_var", choices = valid_group, selected = "Aucun")
    } else {
      updateSelectInput(session, "group_var", choices = valid_group)
    }
  }, ignoreInit = TRUE)
  
  calculate_exhaustive_stats <- function(x) {
    x_clean <- na.omit(x)
    n <- length(x_clean)
    if (n == 0) return(NULL)
    
    mean_val <- mean(x_clean)
    sd_val <- sd(x_clean)
    se_val <- sd_val / sqrt(n)
    
    ci_margin <- if (n > 1 && is.finite(sd_val)) qt(0.975, df = n - 1) * se_val else NA_real_
    
    q1_val <- as.numeric(quantile(x_clean, 0.25, names = FALSE))
    q3_val <- as.numeric(quantile(x_clean, 0.75, names = FALSE))
    iqr_val <- q3_val - q1_val
    
    shapiro_p <- if (n >= 3 && n <= 5000) shapiro.test(x_clean)$p.value else NA_real_
    outliers_n <- sum(x_clean < (q1_val - 1.5 * iqr_val) | x_clean > (q3_val + 1.5 * iqr_val))
    
    mode_val <- tryCatch({
      as.numeric(names(sort(-table(x_clean)))[1])
    }, error = function(e) NA_real_)
    
    list(
      n = n,
      n_na = sum(is.na(x)),
      mean = mean_val,
      ci_mean = if (!is.na(ci_margin)) paste0("[", round(mean_val - ci_margin, 3), " ; ", round(mean_val + ci_margin, 3), "]") else "N/A",
      geom_mean = if (any(x_clean <= 0)) NA_real_ else exp(mean(log(x_clean))),
      harm_mean = if (any(x_clean <= 0)) NA_real_ else 1 / mean(1 / x_clean),
      median = median(x_clean),
      mode = mode_val,
      sd = sd_val,
      var = var(x_clean),
      se = se_val,
      cv = ifelse(mean_val != 0, (sd_val / mean_val) * 100, NA_real_),
      mad = mad(x_clean),
      iqr = iqr_val,
      min = min(x_clean),
      q1 = q1_val,
      q3 = q3_val,
      max = max(x_clean),
      range = max(x_clean) - min(x_clean),
      skew = moments::skewness(x_clean),
      kurt = moments::kurtosis(x_clean),
      shapiro = shapiro_p,
      outliers = outliers_n
    )
  }
  
  output$exhaustive_stats_ui <- renderUI({
    df <- raw_data()
    active_stats <- if (is.null(input$selected_stats)) c("n", "mean", "ci_mean", "sd", "skew", "shapiro", "outliers") else input$selected_stats
    req(df, input$target_vars, active_stats)
    
    build_card <- function(data_subset, var_name, group_label = NULL) {
      stats <- calculate_exhaustive_stats(data_subset[[var_name]])
      if (is.null(stats)) return(NULL)
      
      title_text <- if (is.null(group_label)) var_name else paste(var_name, "-", group_label)
      safe_group <- if (is.null(group_label)) "global" else make.names(group_label)
      plot_id <- paste0("plot_", make.names(var_name), "_", safe_group)
      
      list_items <- lapply(names(all_stats_options), function(stat_name) {
        stat_key <- all_stats_options[[stat_name]]
        if (stat_key %in% active_stats) {
          val <- stats[[stat_key]]
          display_val <-
            if (is.null(val) || length(val) == 0 || is.na(val[1])) "N/A"
          else if (is.numeric(val)) {
            if (stat_key == "shapiro" && is.finite(val) && val < 0.001) "< 0.001" else round(val, 4)
          } else val
          
          collapse_id <- paste0("collapse_", stat_key, "_", make.names(var_name), "_", safe_group)
          
          tagList(
            tags$li(
              class = "list-group-item stat-row",
              `data-toggle` = "collapse",
              `data-target` = paste0("#", collapse_id),
              icon("info-circle", class = "text-info"), " ",
              span(class = "stat-label", stat_name),
              span(class = "stat-val", display_val)
            ),
            tags$div(
              id = collapse_id, class = "collapse stat-explanation",
              p(strong("Explication : "), stat_dict[[stat_key]]$desc),
              if (stat_dict[[stat_key]]$math != "") div(class = "math-text", stat_dict[[stat_key]]$math) else NULL
            )
          )
        } else NULL
      })
      list_items <- Filter(Negate(is.null), list_items)
      
      box(
        title = title_text, width = 6, status = "primary", solidHeader = TRUE,
        collapsible = TRUE, class = "stat-card",
        tags$ul(class = "list-group", list_items),
        plotOutput(outputId = plot_id, height = "220px")
      )
    }
    
    if (input$group_var == "Aucun") {
      tagList(
        div(class = "group-header", h3("Analyse Globale")),
        fluidRow(lapply(input$target_vars, function(v) build_card(df, v)))
      )
    } else {
      groups <- na.omit(unique(df[[input$group_var]]))
      tagList(lapply(groups, function(g) {
        tagList(
          div(class = "group-header", h3(paste("Groupe :", g))),
          fluidRow(lapply(input$target_vars, function(v) build_card(df[df[[input$group_var]] == g, , drop = FALSE], v, g)))
        )
      }))
    }
  })
  
  drop_outliers_iqr <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 4) return(x)
    q1 <- as.numeric(quantile(x, 0.25))
    q3 <- as.numeric(quantile(x, 0.75))
    iqr <- q3 - q1
    x[x >= (q1 - 1.5 * iqr) & x <= (q3 + 1.5 * iqr)]
  }
  
  # Plots dynamiques
  observe({
    df <- raw_data()
    req(df, input$target_vars, input$plot_type)
    
    for (v in input$target_vars) {
      if (input$group_var == "Aucun") {
        local({
          v_name <- v
          plot_id <- paste0("plot_", make.names(v_name), "_global")
          
          output[[plot_id]] <- renderPlot({
            req(v_name %in% names(df))
            plot_df <- df
            
            if (isTRUE(input$plot_drop_outliers)) {
              keep_vals <- drop_outliers_iqr(plot_df[[v_name]])
              plot_df <- plot_df[plot_df[[v_name]] %in% keep_vals | is.na(plot_df[[v_name]]), , drop = FALSE]
            }
            
            use_log <- isTRUE(input$plot_log) && all(plot_df[[v_name]] > 0, na.rm = TRUE)
            
            if (input$plot_type == "dist") {
              p <- ggplot(plot_df, aes(x = .data[[v_name]])) +
                geom_histogram(aes(y = after_stat(density)),
                               fill = "#3c8dbc", color = "white", bins = 30, alpha = 0.7) +
                geom_density(color = "#d9534f", linewidth = 1, na.rm = TRUE) +
                theme_minimal() +
                labs(x = v_name, y = "Densité")
              if (use_log) p <- p + scale_x_log10()
              p
            } else {
              p <- ggplot(plot_df, aes(x = "", y = .data[[v_name]])) +
                geom_boxplot(fill = "#f39c12", color = "#e67e22", alpha = 0.7,
                             outlier.color = "red", outlier.size = 2.5, na.rm = TRUE) +
                geom_jitter(width = 0.15, alpha = 0.25, size = 1, na.rm = TRUE) +
                theme_minimal() +
                theme(axis.title.x = element_blank(),
                      axis.text.x  = element_blank(),
                      axis.ticks.x = element_blank()) +
                labs(y = v_name)
              if (use_log) p <- p + scale_y_log10()
              p
            }
          })
        })
      } else {
        groups <- na.omit(unique(df[[input$group_var]]))
        for (g in groups) {
          local({
            v_name <- v
            g_name <- g
            plot_id <- paste0("plot_", make.names(v_name), "_", make.names(g_name))
            
            output[[plot_id]] <- renderPlot({
              req(v_name %in% names(df))
              plot_df <- df[df[[input$group_var]] == g_name, , drop = FALSE]
              
              if (isTRUE(input$plot_drop_outliers)) {
                keep_vals <- drop_outliers_iqr(plot_df[[v_name]])
                plot_df <- plot_df[plot_df[[v_name]] %in% keep_vals | is.na(plot_df[[v_name]]), , drop = FALSE]
              }
              
              use_log <- isTRUE(input$plot_log) && all(plot_df[[v_name]] > 0, na.rm = TRUE)
              
              if (input$plot_type == "dist") {
                p <- ggplot(plot_df, aes(x = .data[[v_name]])) +
                  geom_density(fill = "#00a65a", alpha = 0.4, color = "#008d4c", linewidth = 1, na.rm = TRUE) +
                  theme_minimal() +
                  labs(x = v_name, y = "Densité")
                if (use_log) p <- p + scale_x_log10()
                p
              } else {
                p <- ggplot(plot_df, aes(x = "", y = .data[[v_name]])) +
                  geom_boxplot(fill = "#00a65a", alpha = 0.5,
                               outlier.color = "red", outlier.size = 2.5, na.rm = TRUE) +
                  geom_jitter(width = 0.15, alpha = 0.25, size = 1, na.rm = TRUE) +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(),
                        axis.text.x  = element_blank(),
                        axis.ticks.x = element_blank()) +
                  labs(y = paste0(v_name, " (", g_name, ")"))
                if (use_log) p <- p + scale_y_log10()
                p
              }
            })
          })
        }
      }
    }
  })
  
  corr_matrix <- reactive({
    df <- raw_data()
    req(df, input$target_vars, length(input$target_vars) > 1)
    
    num_df <- df %>%
      select(all_of(input$target_vars)) %>%
      select(where(is.numeric))
    
    req(ncol(num_df) > 1)
    round(cor(num_df, use = input$corr_use, method = input$corr_method), 2)
  })
  
  output$corr_plot <- renderPlot({
    cormat <- corr_matrix()
    cormat_melted <- as.data.frame(as.table(cormat))
    names(cormat_melted) <- c("Var1", "Var2", "Correlation")
    
    ggplot(cormat_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#d9534f", high = "#3c8dbc", mid = "white",
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = "Corrélation") +
      geom_text(aes(label = Correlation), color = "black", size = 5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title = element_blank(),
            panel.grid.major = element_blank())
  })
  
  output$data_status <- renderValueBox({
    df <- raw_data()
    if (is.null(df)) {
      valueBox("En attente", "Téléversez des données", icon = icon("upload"), color = "red")
    } else {
      valueBox(paste0(nrow(df), " lignes / ", ncol(df), " colonnes"),
               "Données actives", icon = icon("check-circle"), color = "green")
    }
  })
  
  output$raw_table <- renderDT({
    df <- raw_data(); req(df)
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), filter = "top")
  })
}

shinyApp(ui, server)