library(shiny)
library(jsonlite)
library(mrgsolve)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyjs)

# ============= APP CONFIGURATION =============
models_dir <- "/../models/"
CTS_SUBDIR <- "subdir"
CARDS_PER_ROW <- 3
MIN_YEAR_DEFAULT <- 2000
MAX_YEAR_DEFAULT <- 2025

# ============= HELPER FUNCTIONS =============

# Extract unique models by filename
get_unique_models <- function(data) {
  data[!duplicated(data$filename), ]
}

# Extract year from string
extract_year <- function(year_str) {
  suppressWarnings(as.numeric(substr(year_str, 1, 4)))
}

# Get model file path
get_model_path <- function(filename) {
  file.path(getwd(), models_dir, filename)
}

# Get CTS subdir path
get_cts_path <- function(filename) {
  file.path(getwd(), CTS_SUBDIR, filename)
}

# Expand delimited fields (indication, therapeutic_area with "/" separators)
expand_delimited_fields <- function(data, fields = c("indication", "therapeutic_area")) {
  result <- data
  
  # Store originals
  for (field in fields) {
    if (field %in% colnames(result)) {
      result[[paste0(field, "_original")]] <- result[[field]]
    }
  }
  
  # Expand rows
  for (field in fields) {
    if (field %in% colnames(result)) {
      result[[field]] <- as.character(result[[field]])
      split_rows <- vector("list", nrow(result))
      
      for (i in seq_len(nrow(result))) {
        val <- result[[field]][i]
        val <- trimws(val)
        
        if (is.na(val) || val == "") {
          split_rows[[i]] <- result[i, , drop = FALSE]
        } else {
          val <- gsub("/+", "/", val)
          val <- gsub("^/|/$", "", val)
          
          if (grepl("/", val)) {
            vals <- unlist(strsplit(val, "/"))
            vals <- trimws(vals)
            rows <- result[rep(i, length(vals)), ]
            rows[[field]] <- vals
            split_rows[[i]] <- rows
          } else {
            split_rows[[i]] <- result[i, , drop = FALSE]
          }
        }
      }
      result <- do.call(rbind, split_rows)
      rownames(result) <- NULL
    }
  }
  return(result)
}

get_model_metadata <- function(models_dir) {
  json_files <- list.files(paste0(getwd(),models_dir), pattern = "\\.json$", full.names = TRUE)
  # Define expected fields with default values
  template <- list(
    filename = NA_character_,
    display_name = NA_character_,
    description = NA_character_,
    author = NA_character_,
    date_created = NA_character_,
    date_last_updated = NA_character_,
    year = NA_integer_,
    model_type = NA_character_,
    validation_status = NA_character_,
    compound = NA_character_,
    source = NA_character_,
    therapeutic_area = NA_character_,
    indication = NA_character_,
    applications = NA_character_
  )
  
  # Read and standardize each JSON file
  meta <- lapply(json_files, function(f) {
    tryCatch({
      json_data <- fromJSON(f)
      
      # Extract model_information if it exists (new nested structure)
      if (!is.null(json_data$model_information)) {
        json_data <- json_data$model_information
      }
      
      # Ensure all fields exist with proper types
      for (field in names(template)) {
        if (is.null(json_data[[field]])) {
          json_data[[field]] <- template[[field]]
        }
        # Trim whitespace from character fields
        if (is.character(json_data[[field]])) {
          json_data[[field]] <- trimws(json_data[[field]])
        }
      }
      as.data.frame(json_data, stringsAsFactors = FALSE)
    }, error = function(e) {
      warning("Error reading ", f, ": ", e$message)
      as.data.frame(template, stringsAsFactors = FALSE)
    })
  })
  
  # Combine all data frames
  result <- do.call(rbind, meta)
  rownames(result) <- NULL

  # Use the new expansion function
  result <- expand_delimited_fields(result, fields = c("indication", "therapeutic_area"))
  
  return(result)
}

ui1 <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .card {
        box-shadow: 0 2px 8px rgba(44,62,80,0.08);
        border-radius: 10px;
        margin-bottom: 20px;
        padding: 15px;
        background: #fff;
      }
      .header-image {
        width: 100%;
        margin-bottom: 20px;
      }
      .model-card {
        border: 1px solid #ccc;
        border-radius: 8px;
        margin: 10px;
        padding: 10px;
        background: #f9f9f9;
        cursor: pointer;
        transition: all 0.2s ease;
      }
      .model-card:hover {
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
      }
      .model-card.selected {
        border-color: #337ab7;
        background: #e8f4f8;
      }
      .info-icon {
        display: inline-block;
        width: 18px;
        height: 18px;
        line-height: 18px;
        border-radius: 50%;
        background-color: #5bc0de;
        color: white;
        text-align: center;
        font-size: 12px;
        font-weight: bold;
        margin-left: 5px;
        cursor: help;
        position: relative;
      }
      .info-icon:hover::after {
        content: attr(data-tooltip);
        position: absolute;
        top: 50%;
        left: 100%;
        margin-left: 10px;
        width: 300px;
        background-color: #333;
        color: white;
        padding: 8px;
        border-radius: 4px;
        font-size: 12px;
        font-weight: normal;
        z-index: 1000;
        white-space: normal;
        transform: translateY(-50%);
      }
      .info-icon:hover::before {
        content: '';
        position: absolute;
        top: 50%;
        left: 100%;
        width: 0;
        height: 0;
        border-top: 8px solid transparent;
        border-bottom: 8px solid transparent;
        border-right: 8px solid #333;
        z-index: 1000;
        transform: translateY(-50%);
      }
      .welcome-btn {
        margin-bottom: 15px;
      }
    "))
  ),
  
  # Main library view - shown when no model is selected and SubApp2 is not active
  conditionalPanel(
    condition = "output.show_library == true",
    #tags$img(src = "Modellibraryheader.png", class = "header-image"),
    
    # ========== Compact Welcome Button ==========
    div(class = "welcome-btn", style = "margin: 10px; text-align: right;",
      actionButton("toggle_welcome", "Show Welcome", class = "btn-sm")
    ),
    
    # ========== Collapsible Welcome Panel ==========
    conditionalPanel(
      condition = "input.toggle_welcome % 2 == 1",
      div(class = "alert alert-info", style = "margin: 10px; padding: 12px; border-radius: 5px;",
        tags$h5("Getting Started with EduCTS", style = "margin-top: 0;"),
        tags$ul(style = "margin: 5px 0 0 0;",
          tags$li(tags$strong("Summary View"), " - Overview of models by therapeutic area and type"),
          tags$li(tags$strong("Cards View"), " - Browse and select individual models for simulation"),
          tags$li(tags$strong("Help Tab"), " - Detailed explanations of features and concepts")
        )
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 2,
        tags$h5("Filter Models", style = "margin-top: 0;"),
        
        div(style = "display: flex; align-items: center; margin-bottom: 10px;",
          tags$label("Model type", style = "margin-bottom: 0;"),
          div(class = "info-icon", `data-tooltip` = "PK: absorption and distribution. PKPD: includes drug effects on body",
            tags$span("?", style = "font-size: 14px;")
          )
        ),
        checkboxGroupInput("filter_type", NULL, choices = NULL),
        
        div(style = "display: flex; align-items: center; margin-bottom: 10px;",
          tags$label("Therapeutic Area", style = "margin-bottom: 0;"),
          div(class = "info-icon", `data-tooltip` = "Disease category (e.g., Cardiovascular, Metabolic Disorders)",
            tags$span("?", style = "font-size: 14px;")
          )
        ),
        selectInput("filter_therapeutic_area", NULL, choices = NULL, multiple = TRUE),
        
        div(style = "display: flex; align-items: center; margin-bottom: 10px;",
          tags$label("Indication", style = "margin-bottom: 0;"),
          div(class = "info-icon", `data-tooltip` = "Specific medical condition the model can be used for",
            tags$span("?", style = "font-size: 14px;")
          )
        ),
        selectInput("filter_indication", NULL, choices = NULL, multiple = TRUE),
        uiOutput("year_slider")
      ),
      mainPanel(
        tabsetPanel(
          id = "view_type",
          tabPanel("Summary View",
            fluidRow(
              column(12,
                div(style = "text-align:center; margin-bottom: 18px; margin-top: 30px; font-size: 1.6em; font-weight: bold;", "Models by Therapeutic Area"),
                plotlyOutput("therapeutic_area_pie", height = "800px")
              ),
              column(12,
                div(style = "text-align:center; margin-bottom: 18px; margin-top: 30px; font-size: 1.6em; font-weight: bold;", "Models by Type"),
                plotlyOutput("model_type_pie", height = "400px")
              )
            )
          ),
          tabPanel("Cards View",
            fluidRow(
              column(6,
                textInput("model_search", "Search models", value = "", placeholder = "Type to search...")
              ),
              column(6,
                textInput("compound_search", "Search by Compound", value = "", placeholder = "Type to search compounds...")
              )
            ),
            # ========== NEW: Add selection mode toggle ==========
            fluidRow(
              column(12,
                  radioButtons("selection_mode", "Select for clinical trial simulation:", 
                   choices = c("Single Model" = "single", "Multiple Models" = "multi"),
                   selected = "single",
                   inline = TRUE)
                    )
                  ),
            # ========== NEW: Show selected models panel (only in multi mode) ==========
            conditionalPanel(
              condition = "input.selection_mode == 'multi'",
              uiOutput("selected_models_panel")
          ),
            uiOutput("model_cards_header"),
            uiOutput("model_cards")
          ),
          tabPanel("Help",
            tags$h3("Getting Started with EduCTS"),
            tags$h4("Overview"),
            tags$p("EduCTS is an educational tool for exploring pharmacokinetic-pharmacodynamic (PKPD) and pharmacokinetic (PK) models.
                   You can browse models, review their validation against clinical data, and run virtual clinical trial simulations."),
            
            tags$h4("Key Concepts"),
            tags$strong("Pharmacokinetics (PK):"),
            tags$p("How the body absorbs, distributes, metabolizes, and eliminates a drug."),
            tags$strong("Pharmacodynamics (PD):"),
            tags$p("How the drug affects the body - the relationship between drug concentration and beneficial/harmful effects."),
            tags$strong("PKPD Model:"),
            tags$p("A mathematical model that combines both PK and PD to predict clinical outcomes."),
            
            tags$h4("Workflow"),
            tags$ol(
              tags$li(tags$strong("Browse Models"), " - Use Summary View to see available models or Cards View to search and filter"),
              tags$li(tags$strong("Select a Model"), " - Click on a model card to view details."),
              tags$li(tags$strong("Run Simulation"), " - Click 'Clinical Trial Simulation' to set up and run a virtual trial"),
              tags$li(tags$strong("Explore Results"), " - View simulated outcomes and compare the model predictions against validation data")
            ),
            
            tags$h4("Filter Guide"),
            tags$strong("Model Type:"),
            tags$p("PK - Describes drug pharmacokinetics only. PKPD - Describes both PK and pharmacodynamic effects."),
            tags$strong("Therapeutic Area:"),
            tags$p("The disease category the model is designed for (e.g., Cardiovascular, Metabolic Disorders)."),
            tags$strong("Indication:"),
            tags$p("The specific medical condition the model addresses (e.g., Hypercholesterolemia, Hypertriglyceridemia)."),
            
            tags$h4("Understanding Validation"),
            tags$p("Each model includes validation results comparing model predictions to actual clinical trial data.
                   This demonstrates how well the model captures real drug behavior and supports its use in simulations."),
            
            tags$h4("Tips"),
            tags$ul(
              tags$li("Hover over the ", tags$span("?", style = "display: inline-block; width: 16px; height: 16px; background-color: #5bc0de; color: white; border-radius: 50%; text-align: center; font-weight: bold; font-size: 12px;"), " icons next to filter names for quick tooltips"),
              tags$li("Use the Summary View to get a quick overview of available models"),
              tags$li("Use the Cards View to search for specific models by name or compound"),
              tags$li("Click on a model card to view detailed information before running a simulation.")
            )
          )
        )
      )
    )
  ),
  
  # Model card detail view - shown when a model is selected but CTS app is not launched
  conditionalPanel(
    condition = "output.show_model_card == true",
    uiOutput("model_card_section")
  ),
  
  # CTS App view - shown when SubApp2 is clicked
  conditionalPanel(
    condition = "output.show_cts_app == true",
    uiOutput("cts_app_ui")
  )
)

server <- function(input, output, session) {
  meta <- get_model_metadata(models_dir)
  
  # Populate filter choices
  updateCheckboxGroupInput(session, "filter_type", choices = sort(unique(meta$model_type)))
  updateSelectInput(session, "filter_therapeutic_area", choices = sort(unique(meta$therapeutic_area)))
  updateSelectInput(session, "filter_indication", choices = sort(unique(meta$indication)))
  
  selected_model <- reactiveVal(NULL)
  cts_app_active <- reactiveVal(FALSE)
  
  # Control which panel to show
  output$show_library <- reactive({
    is.null(selected_model()) && !cts_app_active()
  })
  outputOptions(output, "show_library", suspendWhenHidden = FALSE)
  
  output$show_model_card <- reactive({
    !is.null(selected_model()) && !cts_app_active()
  })
  outputOptions(output, "show_model_card", suspendWhenHidden = FALSE)
  
  output$show_cts_app <- reactive({
    cts_app_active()
  })
  outputOptions(output, "show_cts_app", suspendWhenHidden = FALSE)
  
  # Extract years from year and find range, handle empty/NA gracefully
  model_years <- extract_year(meta$year)
  valid_years <- model_years[!is.na(model_years) & is.finite(model_years)]
  if (length(valid_years) > 0) {
    min_year <- min(valid_years)
    max_year <- max(valid_years)
  } else {
    min_year <- MIN_YEAR_DEFAULT
    max_year <- MAX_YEAR_DEFAULT
  }
  
  # Create year range slider
  output$year_slider <- renderUI({
    sliderInput("year_range", 
                "Year Range:",
                min = min_year,
                max = max_year,
                value = c(min_year, max_year),
                step = 1,
                sep = "")
  })
  
  # Reactive filtered meta
  filtered_meta <- reactive({
    m <- meta
    if (!is.null(input$filter_type) && length(input$filter_type) > 0) {
      m <- m[m$model_type %in% input$filter_type, ]
    }
    if (!is.null(input$filter_therapeutic_area) && length(input$filter_therapeutic_area) > 0) {
      m <- m[m$therapeutic_area %in% input$filter_therapeutic_area, ]
    }
    if (!is.null(input$filter_indication) && length(input$filter_indication) > 0) {
      m <- m[m$indication %in% input$filter_indication, ]
    }
    if (!is.null(input$model_search) && input$model_search != "") {
      m <- m[grepl(input$model_search, m$display_name, ignore.case = TRUE), ]
    }
    if (!is.null(input$compound_search) && input$compound_search != "") {
      m <- m[grepl(input$compound_search, m$compound, ignore.case = TRUE), ]
    }
  # Add year range filtering
      years <- extract_year(m$year)
      m <- m[years >= input$year_range[1] & years <= input$year_range[2], ]
  })
  
  # Model cards in main panel
  output$model_cards <- renderUI({
    if (!is.null(selected_model())) return(NULL)
    m <- filtered_meta()
    if (nrow(m) == 0) return(div("No models match the selected filters."))
    
    m_unique <- get_unique_models(m)
    
    # ========== MODIFIED: Different card rendering based on selection mode ==========
    card_list <- lapply(seq_len(nrow(m_unique)), function(i) {
      column(
        width = floor(12 / CARDS_PER_ROW),
        
        # ========== CHOICE: Single-click card OR checkbox card ==========
        if (input$selection_mode == "single") {
          # Original single-selection card (click to view details)
          div(
            class = "model-card",
            id = paste0("card_", i),
            onclick = sprintf("Shiny.setInputValue('%s', Math.random())", paste0("card_click_", i)),
            h4(m_unique$display_name[i]),
            p(m_unique$model_type[i]),
            p(m_unique$therapeutic_area_original[i]),
            p(m_unique$compound[i]),
            style = "margin:0; height: 180px;"
          )
        } else {
          # ========== NEW: Multi-selection card with checkbox ==========
          div(
            class = "model-card",
            style = "height: 200px;",
            checkboxInput(
              inputId = paste0("select_model_", i),
              label = NULL,
              value = FALSE
            ),
            h5(m_unique$display_name[i]),
            p(style = "font-size: 0.9em;", m_unique$model_type[i]),
            p(style = "font-size: 0.85em;", m_unique$therapeutic_area_original[i]),
            p(style = "font-size: 0.85em;", m_unique$compound[i])
          )
        }
      )
    })
    
    rows <- split(card_list, ceiling(seq_along(card_list) / CARDS_PER_ROW))
    tagList(
      lapply(rows, function(row_cards) {
        fluidRow(row_cards)
      })
    )
  })
  
  # Observe card clicks with event delegation
  observe({
    m <- filtered_meta()
    if (!is.null(m) && nrow(m) > 0) {
      m_unique <- get_unique_models(m)
      lapply(seq_len(nrow(m_unique)), function(i) {
        observeEvent(input[[paste0("card_click_", i)]], {
          selected_model(i)
          cts_app_active(FALSE)
        })
      })
    }
  })
  
  # Model card section (same as before)
  output$model_card_section <- renderUI({
    if (is.null(selected_model())) return(NULL)
    i <- selected_model()
    m <- filtered_meta()
    m_unique <- get_unique_models(m)
    if (i > nrow(m_unique)) return(NULL)
    selected <- m_unique[i, ]
    
    fluidRow(
      column(
        width = 8,
        h3("Model Card"),
        h4(selected$display_name),
        p(selected$description),
        tags$b("Author:"), selected$author, br(),
        tags$b("Type:"), selected$model_type, br(),
        tags$b("Compound:"), selected$compound, br(),
        tags$b("Therapeutic Area:"), if (!is.null(selected$therapeutic_area_original)) selected$therapeutic_area_original else selected$therapeutic_area, br(),
        tags$b("Indication:"), if (!is.null(selected$indication_original)) selected$indication_original else selected$indication, br(),
        tags$b("Applications:"), selected$applications, br(),
        tags$b("Source:"), if (!is.na(selected$source) && nzchar(selected$source)) {
          tags$a(href = selected$source, selected$source, target = "_blank")
        } else {
          "N/A"
        }, br(),
        tags$b("Validation Status:"), selected$validation_status, br(),
        tags$b("Last Updated:"), selected$date_last_updated, br(),
        actionButton("back_to_library", "Back to Library", style = "margin-top:15px;")
      ),
      column(
        width = 4,
        div(style = "margin-top:40px;",
          actionButton("view_model_code", "View Model Code", style = "width: 100%; margin-bottom:15px;"),
          downloadButton("download_code", "Download Code", style = "width: 100%;margin-bottom:15px;"),
          actionButton("launch_cts", "Use Model in Clinical Trial Simulator", 
                      style = "width: 100%; background-color: #337ab7; color: #fff;")
        )
      )
    )
  })
  
  # Add this new output in your server function
  output$selected_models_panel <- renderUI({
    m <- filtered_meta()
    m_unique <- get_unique_models(m)
    
    # Find which models are checked
    selected_indices <- which(sapply(seq_len(nrow(m_unique)), function(i) {
      isTRUE(input[[paste0("select_model_", i)]])
    }))
    
    if (length(selected_indices) == 0) {
      return(div(
        style = "padding: 15px; background: #f0f0f0; border-radius: 8px; margin-bottom: 20px;",
        h5("No models selected"),
        p("Check the boxes on model cards to select multiple models for combined simulation.")
      ))
    }
    
    # Show selected models
    div(
      style = "padding: 15px; background: #e8f4f8; border-radius: 8px; margin-bottom: 20px; border: 2px solid #337ab7;",
      h4(paste(length(selected_indices), "model(s) selected")),
      tags$ul(
        lapply(selected_indices, function(idx) {
          tags$li(m_unique$display_name[idx])
        })
      ),
      actionButton("simulate_multi", "Simulate Selected Models", 
                   class = "btn btn-primary", 
                   style = "width: 100%;")
    )
  })

  # Back to library button
  observeEvent(input$back_to_library, {
    selected_model(NULL)
    cts_app_active(FALSE)
  })
  
  # Launch CTS app
  observeEvent(input$launch_cts, {
    i <- selected_model()
    if (is.null(i)) return()
    m <- filtered_meta()
    m_unique <- get_unique_models(m)
    if (i > nrow(m_unique)) return()
    selected <- m_unique[i, ]
    
    # Write the selected model filename to global.R
    global_path <- get_cts_path("global.R")
    writeLines(sprintf('selected_model_filename <- "%s"', selected$filename), global_path)
    
    cts_app_active(TRUE)
  })

  # Add this in your server function
  observeEvent(input$simulate_multi, {
    m <- filtered_meta()
    m_unique <- get_unique_models(m)
    
    selected_indices <- which(sapply(seq_len(nrow(m_unique)), function(i) {
      isTRUE(input[[paste0("select_model_", i)]])
    }))
    
    if (length(selected_indices) == 0) return()
    
    # Write selected model filenames to global.R (as comma-separated list)
    selected_filenames <- m_unique$filename[selected_indices]
    global_path <- get_cts_path("global.R")
    writeLines(sprintf('selected_model_filenames <- c(%s)', 
                       paste(sprintf('"%s"', selected_filenames), collapse = ", ")),
               global_path)
    
    # Launch CTS app with multiple models
    cts_app_active(TRUE)
  })
  
  # CTS app UI
  output$cts_app_ui <- renderUI({
    if (!cts_app_active()) return(NULL)
    
    source(get_cts_path("global.R"))
    appUI <- eval(parse(file = get_cts_path('CTS_ui.R')))
    appUI
  })
  
  # CTS app server logic
  observe({
    if (cts_app_active()) {
      source(get_cts_path("global.R"))
      appServer <- eval(parse(file = get_cts_path('CTS_server.R')))
      appServer(input, output, session)
    }
  })
  
  # Summary visualizations
  output$therapeutic_area_pie <- renderPlotly({
    m <- filtered_meta()
    area_counts <- table(m$therapeutic_area)
    
    total_models <- sum(area_counts)
    plot_ly(
      labels = names(area_counts),
      values = as.numeric(area_counts),
      type = 'pie',
      hole = 0.5,
      textposition = 'outside',
      textinfo = 'label',
      hoverinfo = 'text',
      text = ~paste(
        names(area_counts), 
        "\nCount:", area_counts, 
        "\n(", round(100 * area_counts/sum(area_counts), 1), "%)"
      )
    ) %>% 
    layout(
      showlegend = FALSE,
      annotations = list(
        list(
          text = paste0('<b>', total_models, '</b><br>models'),
          x = 0.5,
          y = 0.5,
          font = list(size = 20),
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          align = 'center',
          valign = 'middle'
        )
      )
    )
  })
  
  output$model_type_pie <- renderPlotly({
    m <- filtered_meta()
    type_counts <- table(m$model_type)
    
    total_models <- sum(type_counts)
    plot_ly(
      labels = names(type_counts),
      values = as.numeric(type_counts),
      type = 'pie',
      hole = 0.5,
      textposition = 'outside',
      textinfo = 'label',
      hoverinfo = 'text',
      text = ~paste(
        names(type_counts), 
        "\nCount:", type_counts,
        "\n(", round(100 * type_counts/sum(type_counts), 1), "%)"
      )
    ) %>% 
    layout(
      showlegend = FALSE,
      annotations = list(
        list(
          text = paste0('<b>', total_models, '</b><br>models'),
          x = 0.5,
          y = 0.5,
          font = list(size = 20),
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          align = 'center',
          valign = 'middle'
        )
      )
    )
  })
  
  # Keep the summary table below the charts
  output$models_summary <- DT::renderDataTable({
    m <- filtered_meta()
    summary_df <- m[, c("display_name", "model_type", "therapeutic_area", 
                       "indication", "compound", "validation_status", "year")]
    colnames(summary_df) <- c("Model Name", "Type", "Therapeutic Area", 
                             "Indication", "Compound", "Validation Status", "Year")
    DT::datatable(summary_df, 
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # View Model Code button
  observeEvent(input$view_model_code, {
    i <- selected_model()
    if (is.null(i)) return()
    m <- filtered_meta()
    m_unique <- get_unique_models(m)
    if (i > nrow(m_unique)) return()
    selected <- m_unique[i, ]
    
    model_file <- get_model_path(selected$filename)
    
    if (file.exists(model_file)) {
      code <- readLines(model_file)
      showModal(modalDialog(
        title = paste("Model Code:", selected$display_name),
        tags$pre(tags$code(paste(code, collapse = "\n"))),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        "Model file not found.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })
  
  # Download Code button
  output$download_code <- downloadHandler(
    filename = function() {
      i <- selected_model()
      if (is.null(i)) return("model.cpp")
      m <- filtered_meta()
      m_unique <- get_unique_models(m)
      if (i > nrow(m_unique)) return("model.cpp")
      selected <- m_unique[i, ]
      selected$filename
    },
    content = function(file) {
      i <- selected_model()
      if (is.null(i)) return()
      m <- filtered_meta()
      m_unique <- get_unique_models(m)
      if (i > nrow(m_unique)) return()
      selected <- m_unique[i, ]
      
      model_file <- get_model_path(selected$filename)
      if (file.exists(model_file)) {
        file.copy(model_file, file)
      }
    }
  )
}

ui <- ui1  # Remove the uiOutput wrapper

shinyApp(ui, server)