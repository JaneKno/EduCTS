library(shiny)
library(jsonlite)
library(mrgsolve)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyjs)

# ============= LOAD QUESTION LIBRARY =============
source("QuestionLibrary.R")
question_lib <- export_question_library()
questions_df              <- question_lib$questions
question_descriptions     <- question_lib$question_descriptions
phase_descriptions        <- question_lib$phase_descriptions
approach_descriptions     <- question_lib$approach_descriptions
filter_models_by_question <- question_lib$filter_models_by_question
get_phases                <- question_lib$get_phases
get_approaches_for_phase  <- question_lib$get_approaches_for_phase
get_questions_for_approach <- question_lib$get_questions_for_approach
get_pd_vars               <- question_lib$get_pd_vars

# ============= APP CONFIGURATION =============
models_dir <- "/models/"
CTS_SUBDIR <- "/scripts/subdir"
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
    clinical_application = NA_character_,
    compound_mode = NA_character_,
    compound_interaction_description = NA_character_,
    author = NA_character_,
    date_created = NA_character_,
    date_last_updated = NA_character_,
    year = NA_integer_,
    model_type = NA_character_,
    model_file_type = NA_character_,
    validation_status = NA_character_,
    compound = NA_character_,
    model_time_unit = NA_character_,
    input = NA_character_,
    input_label = NA_character_,
    dose_unit = NA_character_,
    model_dose_unit = NA_character_,
    output = NA_character_,
    output_label = NA_character_,
    source = NA_character_,
    therapeutic_area = NA_character_,
    indication = NA_character_,
    applications = NA_character_,
    modality_type = NA_character_,
    modality = NA_character_
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
      # Keep only template columns to ensure consistent structure
      json_data <- json_data[names(template)]
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
      body { background-color: #f4f6fb; color: #1e293b; }

      .app-header {
        background: linear-gradient(135deg, #1e3a5f 0%, #2563eb 100%);
        padding: 20px 28px 16px 28px;
      }
      .app-header h2 { color: #fff; margin: 0 0 4px 0; font-size: 1.55em; font-weight: 700; letter-spacing: 0.3px; }
      .app-header p  { color: rgba(255,255,255,0.80); margin: 0; font-size: 0.88em; }

      .nav-tabs { border-bottom: 2px solid #e2e8f0; background: #fff; padding: 0 20px; margin-bottom: 0; }
      .nav-tabs > li > a { color: #64748b; font-weight: 500; border: none !important; padding: 12px 18px; background: transparent !important; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover { color: #2563eb; border-bottom: 3px solid #2563eb !important; background: transparent !important; font-weight: 600; }
      .tab-content { padding-top: 4px; }

      .well { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 10px; box-shadow: 0 1px 4px rgba(0,0,0,0.05); }

      .step-badge {
        display: inline-flex; align-items: center; justify-content: center;
        width: 22px; height: 22px; border-radius: 50%;
        background: #2563eb; color: #fff; font-size: 0.72em; font-weight: 700;
        margin-right: 7px; flex-shrink: 0;
      }
      .step-label {
        display: flex; align-items: center;
        margin-top: 14px; margin-bottom: 4px;
        font-weight: 600; color: #1e3a5f; font-size: 0.91em;
      }

      .model-card {
        border: 1px solid #e2e8f0; border-radius: 10px;
        margin: 8px; padding: 0; background: #fff;
        cursor: pointer; overflow: hidden;
        transition: box-shadow 0.18s ease, transform 0.18s ease;
        box-shadow: 0 1px 4px rgba(0,0,0,0.05);
      }
      .model-card:hover { box-shadow: 0 6px 20px rgba(37,99,235,0.13); transform: translateY(-3px); }
      .model-card.selected { border-color: #2563eb; background: #eff6ff; }
      .model-card-accent { height: 5px; background: linear-gradient(90deg, #1e3a5f, #2563eb, #60a5fa); }
      .model-card-body { padding: 12px 14px 14px 14px; }

      .card { box-shadow: 0 2px 8px rgba(44,62,80,0.08); border-radius: 10px; margin-bottom: 20px; padding: 15px; background: #fff; }
      .header-image { width: 100%; margin-bottom: 20px; }

      .question-card {
        border: 1px solid #bfdbfe; border-left: 4px solid #2563eb;
        background-color: #f8faff; padding: 14px 16px; margin: 10px 0;
        border-radius: 8px; transition: box-shadow 0.15s ease;
      }
      .question-card:hover { box-shadow: 0 3px 10px rgba(37,99,235,0.10); }

      .info-banner {
        border-left: 4px solid #2563eb; background-color: #eff6ff;
        padding: 14px 16px; margin: 12px 0; border-radius: 6px; color: #1e3a5f;
      }
      .description-box {
        background-color: #ffffff; border: 1px solid #e2e8f0;
        border-left: 4px solid #2563eb; padding: 14px 16px;
        margin: 10px 0; border-radius: 6px; color: #374151; font-size: 0.92em;
      }

      .cts-back-bar { background: #fff; border-bottom: 1px solid #e2e8f0; padding: 10px 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }

      .btn-primary { background-color: #2563eb; border-color: #1d4ed8; border-radius: 7px; font-weight: 500; box-shadow: 0 2px 6px rgba(37,99,235,0.25); }
      .btn-primary:hover { background-color: #1d4ed8; border-color: #1e40af; }
      .btn-default { border-radius: 6px; font-weight: 500; }

      .irs--shiny .irs-bar { background: #2563eb; border-color: #2563eb; }
      .irs--shiny .irs-handle { border-color: #2563eb; }

      .info-icon {
        display: inline-flex; align-items: center; justify-content: center;
        width: 18px; height: 18px; border-radius: 50%;
        border: 2px solid #94a3b8; color: #64748b; font-size: 12px; font-weight: bold;
        margin-left: 6px; cursor: help; flex-shrink: 0; position: relative;
      }
      .info-icon:hover::after {
        content: attr(data-tooltip);
        position: absolute; bottom: 24px; left: 50%; transform: translateX(-50%);
        background: #1e3a5f; color: white; padding: 10px 14px; border-radius: 4px;
        font-size: 12px; line-height: 1.4; word-wrap: break-word; white-space: normal; 
        z-index: 1000; font-weight: normal; width: 240px;
        text-align: center; pointer-events: none; box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
      .info-icon:hover::before {
        content: '';
        position: absolute; bottom: 18px; left: 50%; transform: translateX(-50%);
        width: 0; height: 0;
        border-left: 6px solid transparent; border-right: 6px solid transparent;
        border-top: 6px solid #1e3a5f; z-index: 1000; pointer-events: none;
      }

      .quickstart-banner {
        border-left: 4px solid #0275d8; background-color: #e3f2fd;
        padding: 12px 14px; margin-bottom: 0px; border-radius: 6px 6px 0 0; color: #0d47a1;
        cursor: pointer; display: flex; justify-content: space-between; align-items: center;
        transition: background-color 0.2s ease; user-select: none;
      }
      .quickstart-banner:hover { background-color: #bbdefb; }
      .quickstart-banner-icon { display: inline-block; width: 16px; height: 16px;
        background: #0275d8; color: white; border-radius: 50%; text-align: center;
        font-weight: bold; font-size: 11px; margin-right: 8px; flex-shrink: 0; line-height: 16px; }
      .quickstart-banner-toggle { color: #0275d8; font-weight: bold; transition: transform 0.2s ease; }
      .quickstart-banner.collapsed .quickstart-banner-toggle { transform: rotate(-90deg); }
      .quickstart-content { padding: 12px; background: #f5f9ff; margin-bottom: 16px; border-radius: 0 0 6px 6px; display: block !important; }
      .quickstart-banner.collapsed + .quickstart-content { display: none !important; }

      .help-two-column { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; margin: 20px 0; }
      @media (max-width: 768px) { .help-two-column { grid-template-columns: 1fr; } }
      
      .help-content { padding: 20px; }
      .help-content h4 { color: #1e3a5f; font-weight: 700; margin-top: 24px; margin-bottom: 12px; }
      .help-content p { line-height: 1.6; color: #374151; }

      /* Help TOC & Collapsible Sections */
      .help-toc-container { display: grid; grid-template-columns: 220px 1fr; gap: 24px; max-width: 1200px; margin: 0 auto; }
      @media (max-width: 768px) { .help-toc-container { grid-template-columns: 1fr; } }
      
      .help-toc { 
        position: sticky; top: 20px; height: fit-content;
        background: #f8fafc; border: 1px solid #e2e8f0;
        border-radius: 6px; padding: 12px;
      }
      @media (max-width: 768px) { .help-toc { position: static; width: 100%; margin-bottom: 20px; } }
      
      .help-toc-title { font-weight: 700; color: #1e3a5f; margin-bottom: 12px; font-size: 0.95em; }
      
      .help-toc-item {
        padding: 8px 10px; margin: 4px 0; cursor: pointer; border-radius: 4px;
        color: #475569; font-size: 0.9em; transition: all 0.2s ease;
        user-select: none;
      }
      .help-toc-item:hover { background: #e2e8f0; color: #1e3a5f; font-weight: 500; }
      
      .help-section { margin: 0 0 16px 0; }
      .help-section details {
        background: #fafbfc; border: 1px solid #e2e8f0;
        border-radius: 6px; padding: 12px;
      }
      .help-section summary {
        cursor: pointer; font-weight: 600; color: #1e3a5f;
        padding: 8px 0; user-select: none;
      }
      .help-section summary:hover { color: #2563eb; }
      .help-section summary::before {
        content: '▶ '; transform: rotate(0deg); transition: transform 0.2s ease; display: inline-block; width: 1.2em;
      }
      .help-section details[open] summary::before { transform: rotate(90deg); }
      
      .help-section-content { margin-top: 12px; padding-top: 12px; border-top: 1px solid #e2e8f0; }
    "))
  ),
  
  # ============================================================================
  # TOP-LEVEL CONDITIONAL: CTS APP vs QUESTION/MODEL LIBRARY
  # ============================================================================
  
  # Show CTS app when active
  conditionalPanel(
    condition = "output.show_cts_app == true",
    uiOutput("cts_app_ui")
  ),
  
  # Show question/model tabs when CTS not active
  conditionalPanel(
    condition = "!output.show_cts_app",

    # App header
    div(
      class = "app-header",
      h2("MIDD Drug Development Platform"),
      p("Explore model-informed evidence to support preclinical and clinical drug development decisions.")
    ),

    # Master tab structure: Question-First vs Model Library
    tabsetPanel(
      id = "main_view",
      selected = "start_from_question",
      
      # ============================================================================
      # TAB 1: START FROM A QUESTION (Question-first workflow)
      # ============================================================================
      tabPanel(
        "Clinical Question Navigator",
        value = "start_from_question",
        sidebarLayout(
          sidebarPanel(
            width = 3,
          h4("Navigate by Stage", style = "font-weight: 700; color: #1e3a5f; margin: 4px 0 6px 0;"),
          p("Select your area and development stage to find the right models.",
            style = "color: #64748b; font-size: 0.87em; margin-bottom: 18px; line-height: 1.5;"),

          # Step 1: Therapeutic Area pre-filter (data-driven from model JSONs)
          div(style = "display: flex; align-items: center; margin-bottom: 6px;",
            div(class = "step-label", style = "margin-top: 0; margin-bottom: 0;",
              tags$span("1", class = "step-badge"),
              "Therapeutic Area"
            ),
            div(class = "info-icon", `data-tooltip` = "Narrow question suggestions to a specific therapeutic area (optional).", "?")
          ),
          selectInput("ta_filter", label = NULL,
            choices  = c("All Therapeutic Areas" = ""),
            selected = ""
          ),

          # Step 2: Development Phase
          div(style = "display: flex; align-items: center; margin-bottom: 6px;",
            div(class = "step-label", style = "margin-top: 0; margin-bottom: 0;",
              tags$span("2", class = "step-badge"),
              "Development Phase"
            ),
            div(class = "info-icon", `data-tooltip` = "Select where your drug is in development (Preclinical, Phase I, II/III, or Post-Approval).", "?")
          ),
          selectInput("dev_phase", label = NULL,
            choices  = c("Select a phase..." = "", get_phases()),
            selected = ""
          ),

          # Phase description
          uiOutput("phase_description_ui"),

          # Step 3: Clinical Question (cascades from phase)
          uiOutput("question_selector_ui"),

          # Step 4: MIDD Approach (auto-displayed as context for selected question)
          uiOutput("approach_display_ui"),

          hr(),

          # Question details panel
          ),
          
          mainPanel(
            # Quick-start collapsible banner
            div(
              class = "quickstart-banner",
              id = "question_quickstart_banner",
              div(
                span(class = "quickstart-banner-icon", "?"),
                tags$strong("Getting Started: How to use the Clinical Question Navigator")
              ),
              span(class = "quickstart-banner-toggle", "▼")
            ),
            
            div(class = "quickstart-content", id = "question_quickstart_content",
              tags$h5("3-Step Workflow:", style = "margin-top: 0;"),
              tags$ol(
                tags$li(tags$strong("Select development phase"), " — Choose where you are in drug development (Preclinical, Phase I, etc.)"),
                tags$li(tags$strong("Pick a clinical question"), " — System shows questions relevant to that phase"),
                tags$li(tags$strong("Check models"), " — Select the models that answer your question"),
                tags$li(tags$strong("Launch"), " — Click to run simulation with pre-filled parameters")
              ),
              tags$p("Not sure where to start? Go to the ",
                tags$a("Help & Guide", href = "#", onclick = "Shiny.setInputValue('switch_to_help', true); return false;"),
                " tab for more details.",
                style = "font-size: 0.88em; color: #666; margin-top: 12px;")
            ),
            
            uiOutput("question_description_ui"),
            conditionalPanel(
              condition = "input.question_id !== null && input.question_id !== ''",
              h2("Suggested Models for Your Question")
            ),

            # Model suggestion cards with checkboxes
            uiOutput("suggested_models_ui"),

            # Launch button
            uiOutput("launch_cts_button_ui")
          )
        )
      ),
      
      # ============================================================================
      # TAB 2: MODEL LIBRARY (Existing browse-by-model workflow)
      # ============================================================================
      tabPanel(
        "Browse All Models",
        value = "model_library",
        # Main library view - shown when no model is selected and SubApp is not active
        conditionalPanel(
          condition = "output.show_library == true",
          #tags$img(src = "Modellibraryheader.png", class = "header-image"),
          sidebarLayout(
            sidebarPanel(
              width = 2,
              p("FILTERS", style = "font-size:0.72em; font-weight:700; color:#94a3b8; letter-spacing:1px; text-transform:uppercase; margin-bottom:14px;"),
              checkboxGroupInput("filter_type", "Model Type", choices = NULL),
              selectInput("filter_modality", "Modality Type", choices = NULL, multiple = TRUE),
              selectInput("filter_therapeutic_area", "Therapeutic Area", choices = NULL, multiple = TRUE),
              selectInput("filter_indication", "Indication", choices = NULL, multiple = TRUE),
              uiOutput("year_slider")
            ),
            mainPanel(
              tabsetPanel(
                id = "view_type",
                selected = "Cards View",
                tabPanel("Summary View",
                  fluidRow(
                    column(12,
                      div(style = "text-align:center; margin-bottom: 18px; margin-top: 30px; font-size: 1.6em; font-weight: bold;", "Models by Therapeutic Area"),
                      plotlyOutput("therapeutic_area_pie", height = "500px")
                    ),
                    column(6,
                      div(style = "text-align:center; margin-bottom: 18px; margin-top: 30px; font-size: 1.6em; font-weight: bold;", "Models by Type"),
                      plotlyOutput("model_type_pie", height = "400px")
                    ),
                    column(6,
                      div(style = "text-align:center; margin-bottom: 18px; margin-top: 30px; font-size: 1.6em; font-weight: bold;", "Models by Modality"),
                      plotlyOutput("modality_pie", height = "400px")
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
                  # Quick-start collapsible banner
                  div(
                    class = "quickstart-banner",
                    id = "browse_quickstart_banner",
                    div(
                      span(class = "quickstart-banner-icon", "?"),
                      tags$strong("Getting Started: How to use Browse All Models")
                    ),
                    span(class = "quickstart-banner-toggle", "▼")
                  ),
                  
                  div(class = "quickstart-content", id = "browse_quickstart_content",
                    tags$h5("2 Selection Modes:", style = "margin-top: 0;"),
                    tags$ul(
                      tags$li(tags$strong("Single Model:"), " Click a card to view details, download code, investigate validation. Launch simulator with that single model."),
                      tags$li(tags$strong("Multiple Models:"), " Check boxes on multiple cards, optionally compare properties, and run combined simulation.")
                    ),
                    tags$p("Use filters to narrow results by model type, therapeutic area, indication, or publication year.",
                      style = "font-size: 0.88em; color: #666; margin-top: 12px;")
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
                )
              )
            )
          )
        ),
        
        # Model card detail view - shown when a model is selected but CTS app is not launched
        conditionalPanel(
          condition = "output.show_model_card == true",
          uiOutput("model_card_section")
        )
      ),
      
      # ============================================================================
      # TAB 3: HELP & GUIDE
      # ============================================================================
      tabPanel(
        "Help & Guide",
        value = "help_tab",
        div(class = "help-content",
          div(class = "help-toc-container",
            # ========== TABLE OF CONTENTS SIDEBAR ==========
            div(class = "help-toc",
              div(class = "help-toc-title", "📋 Quick Links"),
              div(class = "help-toc-item", onclick = "document.getElementById('help_overview').scrollIntoView({behavior:'smooth'}); document.getElementById('help_overview').open = true;", "1. Welcome"),
              div(class = "help-toc-item", onclick = "document.getElementById('help_question_nav').scrollIntoView({behavior:'smooth'}); document.getElementById('help_question_nav').open = true;", "2. Question Navigator"),
              div(class = "help-toc-item", onclick = "document.getElementById('help_browse').scrollIntoView({behavior:'smooth'}); document.getElementById('help_browse').open = true;", "3. Browse All Models"),
              div(class = "help-toc-item", onclick = "document.getElementById('help_model_info').scrollIntoView({behavior:'smooth'}); document.getElementById('help_model_info').open = true;", "4. Model Information"),
              div(class = "help-toc-item", onclick = "document.getElementById('help_cts').scrollIntoView({behavior:'smooth'}); document.getElementById('help_cts').open = true;", "5. Clinical Trial Sim"),
              div(class = "help-toc-item", onclick = "document.getElementById('help_faqs').scrollIntoView({behavior:'smooth'}); document.getElementById('help_faqs').open = true;", "6. FAQs"),
              div(class = "help-toc-item", onclick = "document.getElementById('help_evidence').scrollIntoView({behavior:'smooth'}); document.getElementById('help_evidence').open = true;", "7. Evidence & Credibility")
            ),
            
            # ========== MAIN CONTENT AREA ==========
            div(class = "help-main",
              # 1. WELCOME & OVERVIEW
              div(class = "help-section",
                tags$details(id = "help_overview", open = TRUE,
                  tags$summary("Welcome to the MIDD Drug Development Platform"),
                  div(class = "help-section-content",
                    div(class = "description-box",
                      tags$p("This platform is built on FDA M15 principles for model-informed drug development (MIDD). Our frameworks reflect evidence from Pfizer's portfolio analysis, which demonstrated average cycle time savings of 10 months and $5M cost reduction per program (2021-2023)."),
                      tags$p("This platform provides two complementary entry points to pharmacokinetic-pharmacodynamic (PKPD), QSP, PBPK and PK models for clinical trial simulation:")
                    ),
                    div(class = "help-two-column",
                      div(class = "card",
                        tags$h5("Clinical Question Navigator", style = "color: #2563eb; margin-top: 0;"),
                        tags$strong("Best for: First-time users, guided workflows"),
                        tags$ul(style = "margin: 8px 0;",
                          tags$li("Answer a specific clinical question"),
                          tags$li("System recommends relevant models"),
                          tags$li("Trial parameters pre-filled for you"),
                          tags$li("Faster path to results")
                        )
                      ),
                      div(class = "card",
                        tags$h5("Browse All Models", style = "color: #2563eb; margin-top: 0;"),
                        tags$strong("Best for: Advanced users, custom analysis"),
                        tags$ul(style = "margin: 8px 0;",
                          tags$li("Search/filter entire model library"),
                          tags$li("Select models manually"),
                          tags$li("Configure trial from scratch"),
                          tags$li("Compare multiple approaches")
                        )
                      )
                    )
                  )
                )
              ),
              
              # 2. QUESTION NAVIGATOR
              div(class = "help-section",
                tags$details(id = "help_question_nav",
                  tags$summary("Clinical Question Navigator - Detailed Guide"),
                  div(class = "help-section-content",
                    div(class = "description-box",
                      tags$h5("Step 1: Therapeutic Area (Optional)"),
                      tags$p("Narrow question suggestions to a specific therapeutic area. Leave blank to see all available questions.")
                    ),
                    div(class = "description-box",
                      tags$h5("Step 2: Development Phase"),
                      tags$p("Select where your drug is in development:"),
                      tags$ul(
                        tags$li(tags$strong("Preclinical:"), " Properties, first-dose selection"),
                        tags$li(tags$strong("Phase I:"), " Safety, tolerability, dose ranges"),
                        tags$li(tags$strong("Phase II/III:"), " Efficacy, dosing strategies, comparisons"),
                        tags$li(tags$strong("Post-Approval:"), " Special populations, combinations")
                      )
                    ),
                    div(class = "description-box",
                      tags$h5("Step 3: MIDD Approach"),
                      tags$p("The scientific strategy the question uses (shown automatically):"),
                      tags$ul(
                        tags$li(tags$strong("Dose-Response Modeling:"), " Find optimal dose and identify safe/effective ranges"),
                        tags$li(tags$strong("Comparative Effectiveness:"), " Compare different treatment strategies"),
                        tags$li(tags$strong("Population PK/PD:"), " Understand individual variability across patient types")
                      )
                    ),
                    div(class = "description-box",
                      tags$h5("Step 4: Clinical Question"),
                      tags$p("Pick the specific decision you need to answer. Each question is pre-configured with trial design, arms, study length, and treatment switching if relevant.")
                    ),
                    div(class = "description-box",
                      tags$h5("Step 5: Select Models"),
                      tags$p("Check boxes next to recommended models. Single questions select 1 model; comparison questions select 2+. Models are grouped by shared endpoints for reliable comparison.")
                    ),
                    div(class = "description-box",
                      tags$h5("Step 6: Launch Simulator"),
                      tags$p("Click the button to open the Clinical Trial Simulator with auto-filled trial parameters from your question. Adjust if needed, then run.")
                    )
                  )
                )
              ),
              
              # 3. BROWSE ALL MODELS
              div(class = "help-section",
                tags$details(id = "help_browse",
                  tags$summary("Browse All Models - Detailed Guide"),
                  div(class = "help-section-content",
                    div(class = "description-box",
                      tags$h5("Selection Modes"),
                      tags$p(tags$strong("Single Model Mode:"), " Click a card to view full details, download code, investigate validation."),
                      tags$p(tags$strong("Multiple Models Mode:"), " Check boxes on multiple model cards, compare properties side-by-side, and run combined simulation.")
                    ),
                    div(class = "description-box",
                      tags$h5("Available Filters"),
                      tags$ul(
                        tags$li("Model type (PK, PKPD)"),
                        tags$li("Modality (small molecule, biologic, antibody)"),
                        tags$li("Therapeutic area"),
                        tags$li("Indication (disease)"),
                        tags$li("Publication year")
                      )
                    )
                  )
                )
              ),
              
              # 4. UNDERSTANDING MODEL INFORMATION
              div(class = "help-section",
                tags$details(id = "help_model_info",
                  tags$summary("Understanding Model Information & Validation Status"),
                  div(class = "help-section-content",
                    div(class = "description-box",
                      tags$h5("Validation Status Badges"),
                      tags$ul(
                        tags$li(tags$span("Fully Validated", class = "status-badge badge-fully-validated"), 
                          " — Tested against independent clinical data"),
                        tags$li(tags$span("Validated", class = "status-badge badge-validated"),
                          " — Fits development data well"),
                        tags$li(tags$span("In Progress", class = "status-badge badge-in-progress"),
                          " — Model under development"),
                        tags$li(tags$span("Under Development", class = "status-badge badge-under-development"),
                          " — Early stage")
                      )
                    ),
                    div(class = "description-box",
                      tags$h5("Model Card Contents"),
                      tags$ul(
                        tags$li(tags$strong("Type:"), " PK or PKPD (pharmacodynamic effects)"),
                        tags$li(tags$strong("Compound:"), " Drug name or identifier"),
                        tags$li(tags$strong("Modality:"), " Small molecule, biologic, antibody, etc."),
                        tags$li(tags$strong("Therapeutic Area:"), " Disease specialty (Cardiovascular, Endocrinology, etc.)"),
                        tags$li(tags$strong("Indication:"), " Specific medical condition"),
                        tags$li(tags$strong("Source:"), " Publication or data reference")
                      )
                    )
                  )
                )
              ),
              
              # 5. CLINICAL TRIAL SIMULATOR
              div(class = "help-section",
                tags$details(id = "help_cts",
                  tags$summary("Once in the Clinical Trial Simulator"),
                  div(class = "help-section-content",
                    div(class = "description-box",
                      tags$h5("Virtual Trial Results Tab"),
                      tags$p("Shows simulation outputs including:"),
                      tags$ul(
                        tags$li("Time-course plots with median predictions and intervals"),
                        tags$li("Summary statistics by treatment arm"),
                        tags$li("Dose-response analysis when applicable")
                      )
                    ),
                    div(class = "description-box",
                      tags$h5("Model Evidence Tab"),
                      tags$p("Validates model predictions against real clinical data:"),
                      tags$ul(
                        tags$li(tags$strong("Internal Validation:"), " Data used to build model (goodness-of-fit)"),
                        tags$li(tags$strong("External Validation:"), " Independent clinical trial data (generalization evidence)"),
                        tags$li("Toggle between different studies and treatment arms")
                      )
                    )
                  )
                )
              ),
              
              # 6. FREQUENTLY ASKED QUESTIONS
              div(class = "help-section",
                tags$details(id = "help_faqs",
                  tags$summary("Frequently Asked Questions"),
                  div(class = "help-section-content",
                    div(class = "description-box",
                      tags$h5("Q: Which entry point should I use?"),
                      tags$p(tags$strong("Start with Clinical Question Navigator"), " for specific drug development questions. Use ", tags$strong("Browse All Models"), " if you know which model(s) you want.")
                    ),
                    div(class = "description-box",
                      tags$h5("Q: Can I compare multiple models?"),
                      tags$p(tags$strong("Yes!"), " Either select from a multi-model question, or use Browse All Models in multi-selection mode. Models must have compatible endpoints.")
                    ),
                    div(class = "description-box",
                      tags$h5("Q: Can I modify trial parameters after launching the simulator?"),
                      tags$p(tags$strong("Absolutely."), " Adjust dose, frequency, study length, number of arms, and more directly in the simulator.")
                    ),
                    div(class = "description-box",
                      tags$h5("Q: What's the difference between internal and external validation?"),
                      tags$p(tags$strong("Internal:"), " Model fits development data (shows goodness-of-fit). ", tags$strong("External:"), " Independent data (shows generalization).")
                    ),
                    div(class = "description-box",
                      tags$h5("Q: What does 'treatment switch' mean?"),
                      tags$p(tags$strong("Treatment switch"), " simulates patients switching treatments mid-trial (e.g., dose escalation or rescue therapy). Phase 1 ends at switch point; Phase 2 uses Phase 1 outcomes as baseline.")
                    )
                  )
                )
              ),
              
              # 7. EVIDENCE & CREDIBILITY
              div(class = "help-section",
                tags$details(id = "help_evidence",
                  tags$summary("Why MIDD? Regulatory Recognition & Proven Value"),
                  div(class = "help-section-content",
                    div(class = "description-box",
                      tags$h5("FDA Recognition", style = "margin-top: 0; color: #2563eb;"),
                      tags$p("The FDA's M15 Guidance (December 2024) establishes model-informed drug development as a recognized regulatory decision tool. The FDA's new Quantitative Medicine Center of Excellence coordinates consistent MIDD application across drug reviews.")
                    ),
                    div(class = "description-box",
                      tags$h5("Efficiency Gains: Portfolio-Level Evidence"),
                      tags$p(tags$strong("Pfizer Analysis (2021-2023):")),
                      tags$table(style = "width: 100%; border-collapse: collapse; margin: 8px 0;",
                        tags$tr(style = "background: #f0f9ff; border-bottom: 1px solid #bfdbfe;",
                          tags$td(style = "padding: 6px 8px; font-weight: 600; color: #0c4a6e;", "Metric"),
                          tags$td(style = "padding: 6px 8px; font-weight: 600; color: #0c4a6e;", "Value")
                        ),
                        tags$tr(style = "border-bottom: 1px solid #e2e8f0;",
                          tags$td(style = "padding: 6px 8px;", "Avg. Cycle Time Savings"),
                          tags$td(style = "padding: 6px 8px; color: #059669;", tags$strong("10 months per program"))
                        ),
                        tags$tr(style = "border-bottom: 1px solid #e2e8f0;",
                          tags$td(style = "padding: 6px 8px;", "Avg. Cost Savings"),
                          tags$td(style = "padding: 6px 8px; color: #059669;", tags$strong("$5M per program"))
                        ),
                        tags$tr(style = "background: #fef3c7;",
                          tags$td(style = "padding: 6px 8px;", "Highest Impact"),
                          tags$td(style = "padding: 6px 8px;", "Pediatric PK studies (up to 4 years)")
                        )
                      )
                    ),
                    div(class = "description-box",
                      tags$h5("Clinical Examples"),
                      div(style = "background: #f0f9ff; border-left: 4px solid #0284c7; border-radius: 4px; padding: 10px; margin: 8px 0;",
                        tags$strong("COVID-19 Dosing Optimization:"),
                        tags$p("Quantitative systems pharmacology modeling predicted 5 days of treatment sufficient vs. planned 10 days. Result: Avoided entire study arm, accelerated development.", style = "margin: 4px 0;")
                      ),
                      div(style = "background: #f0f9ff; border-left: 4px solid #0284c7; border-radius: 4px; padding: 10px; margin: 8px 0;",
                        tags$strong("Pediatric Antibacterial Trials:"),
                        tags$p("Population PK modeling enabled pediatric dosing without additional clinical trial. Savings: $17M + 5 years vs. traditional Phase I/II study.", style = "margin: 4px 0;")
                      ),
                      div(style = "background: #f0f9ff; border-left: 4px solid #0284c7; border-radius: 4px; padding: 10px; margin: 8px 0;",
                        tags$strong("NASH Phase IIb Trial (DGAT2i Inhibitor):"),
                        tags$p("Bayesian dose-response modeling enabled ~50% sample size reduction. Savings: ~$25M + 6 months faster enrollment.", style = "margin: 4px 0;")
                      )
                    ),
                    div(class = "description-box",
                      tags$p(tags$strong("Learn More:"), tags$a(href = "https://www.fda.gov/media/184747/download", target = "_blank", "FDA M15 Guidance PDF", style = "color: #2563eb; text-decoration: underline;"))
                    )
                  )
                )
              )
            )
          ),
          
          style = "line-height: 1.7;"
        )
      )
    )
  ),
  
  # Footer
  tags$hr(),
  div(
    style = "text-align: center; color: #999; font-size: 0.85em; padding: 15px; margin-top: 20px;",
    HTML("Creator: Jane Kn\u00f6chel | Last Updated: 2026-05-03 | v1.1")
  )
)

server <- function(input, output, session) {
  meta <- get_model_metadata(models_dir)
  
  # ============================================================================
  # QUICKSTART BANNER TOGGLE HANDLERS (using shinyjs)
  # ============================================================================
  shinyjs::runjs("
    // Function to set up quickstart banner handler
    function setupQuickstartBanner(bannerId, contentId) {
      var banner = document.getElementById(bannerId);
      if (!banner) return;
      
      banner.addEventListener('click', function(e) {
        if (e.target.closest('.quickstart-banner')) {
          e.preventDefault();
          banner.classList.toggle('collapsed');
        }
      });
    }
    
    // Initialize both banners
    setupQuickstartBanner('question_quickstart_banner', 'question_quickstart_content');
    setupQuickstartBanner('browse_quickstart_banner', 'browse_quickstart_content');
  ")
  
  # Populate filter choices
  updateCheckboxGroupInput(session, "filter_type", choices = sort(unique(meta$model_type)))
  updateSelectInput(session, "filter_modality", choices = sort(unique(meta$modality_type[meta$modality_type != "N/A"])))
  updateSelectInput(session, "filter_therapeutic_area", choices = sort(unique(meta$therapeutic_area)))
  updateSelectInput(session, "filter_indication", choices = sort(unique(meta$indication)))

  # Populate Question tab TA pre-filter from actual model metadata
  updateSelectInput(session, "ta_filter",
    choices  = c("All Therapeutic Areas" = "", sort(unique(meta$therapeutic_area[!is.na(meta$therapeutic_area)]))),
    selected = ""
  )
  
  selected_model <- reactiveVal(NULL)
  selected_model_data <- reactiveVal(NULL)
  cts_app_active <- reactiveVal(FALSE)
  selected_question <- reactiveVal(NULL)
  selected_from_question_models <- reactiveVal(NULL)
  n_suggested_models <- reactiveVal(0)
  question_models_displayed <- reactiveVal(NULL)
  
  # ============================================================================
  # HELP & NAVIGATION OBSERVERS
  # ============================================================================
  observeEvent(input$switch_to_help, {
    updateTabsetPanel(session, "main_view", selected = "help_tab")
  })
  
  # ============================================================================
  # QUESTION-FIRST WORKFLOW SERVER LOGIC
  # MIDD-aligned cascade: TA filter → Phase → Approach → Question → Suggested Models
  # ============================================================================

  # Phase description (shown immediately after phase selection)
  output$phase_description_ui <- renderUI({
    phase <- input$dev_phase
    if (is.null(phase) || phase == "") return(NULL)
    desc  <- phase_descriptions[[phase]]
    if (is.null(desc)) return(NULL)
    div(class = "description-box", style = "margin: 6px 0 10px 0; font-size: 0.88em;", desc)
  })

  # Step 2 → 3: Clinical Question (cascades directly from dev_phase)
  output$question_selector_ui <- renderUI({
    phase <- input$dev_phase
    if (is.null(phase) || phase == "") return(NULL)

    q_df <- questions_df[questions_df$phase == phase, ]
    if (nrow(q_df) == 0) {
      return(div(class = "description-box",
        p("No questions available for this phase.", style = "margin: 0; color: #999;")
      ))
    }

    question_choices <- setNames(q_df$question_id, q_df$question)
    tagList(
      div(class = "step-label",
        tags$span("3", class = "step-badge"),
        "Clinical Question"
      ),
      selectInput("question_id", label = NULL,
        choices  = c("Select a question..." = "", question_choices),
        selected = ""
      )
    )
  })

  # Step 3 → 4: MIDD Approach shown as informational badge after question selection
  output$approach_display_ui <- renderUI({
    if (is.null(input$question_id) || input$question_id == "") return(NULL)

    q_row <- questions_df[questions_df$question_id == input$question_id, ]
    if (nrow(q_row) == 0) return(NULL)

    approach <- q_row$approach[1]
    desc     <- approach_descriptions[[approach]]

    tagList(
      div(class = "step-label",
        tags$span("4", class = "step-badge"),
        "MIDD Approach"
      ),
      div(class = "info-banner", style = "padding: 10px 12px; margin: 4px 0 8px 0; font-size: 0.88em;",
        tags$span(
          style = "display: inline-block; background: #2c6fad; color: #fff; border-radius: 12px;
                   padding: 2px 10px; font-weight: bold; font-size: 0.92em; margin-bottom: 6px;",
          approach
        ),
        if (!is.null(desc)) tagList(br(), desc)
      )
    )
  })

  # Reset model checkboxes when question changes (prevents stale selections carrying over)
  observeEvent(input$question_id, {
    n <- n_suggested_models()
    if (n > 0) {
      for (i in seq_len(n)) {
        updateCheckboxInput(session, paste0("qmodel_", i), value = FALSE)
      }
    }
  }, ignoreInit = TRUE)

  # Question description + trial preset summary
  output$question_description_ui <- renderUI({
    if (is.null(input$question_id) || input$question_id == "") {
      return(p("Select a question to see details.", style = "color: #999; font-style: italic;"))
    }

    q_row <- questions_df[questions_df$question_id == input$question_id, ]
    if (nrow(q_row) == 0) return(NULL)

    selected_question(q_row)

    desc <- question_descriptions[[input$question_id]]
    if (is.null(desc)) return(NULL)

    div(class = "description-box",
      strong(q_row$question[1]),
      br(), br(),
      desc,
      br(), hr(),
      tags$ul(
        tags$li("Trial Design: ", strong(q_row$trial_design[1])),
        tags$li("Number of Arms: ", strong(q_row$n_arms[1])),
        if (q_row$enable_switch[1]) tags$li("Treatment Switching: ", strong("Enabled")),
        tags$li("Suggested Duration: ", strong(q_row$suggested_weeks[1], " weeks"))
      )
    )
  })
  
  # Suggested models UI with checkboxes
  output$suggested_models_ui <- renderUI({
    if (is.null(input$question_id) || input$question_id == "") {
      return(div(class = "info-banner",
        p("Please select a question above to see suggested models.", style = "margin: 0;")
      ))
    }
    
    q_row <- questions_df[questions_df$question_id == input$question_id, ]
    if (nrow(q_row) == 0) return(NULL)

    # Filter models for this question (respects TA pre-filter)
    suggested_models <- filter_models_by_question(meta, q_row, ta_filter = input$ta_filter)

    if (nrow(suggested_models) == 0) {
      ta_msg <- if (!is.null(input$ta_filter) && input$ta_filter != "")
        paste0(" in the '" , input$ta_filter, "' therapeutic area") else ""
      return(div(class = "info-banner",
        p(paste0("No models currently available for this question", ta_msg, "."),
          style = "margin: 0;")
      ))
    }
    
    # Get unique models
    suggested_models_unique <- get_unique_models(suggested_models)

    # For multi-model questions: compute connected components by shared PD outputs
    # so models that can be compared together are visually grouped
    is_multi <- isTRUE(q_row$require_multi_model[1])
    if (is_multi) {
      n        <- nrow(suggested_models_unique)
      pd_list  <- lapply(suggested_models_unique$output, get_pd_vars)
      group_id <- seq_len(n)
      for (i in seq_len(n - 1)) {
        for (j in (i + 1):n) {
          if (any(pd_list[[i]] %in% pd_list[[j]])) {
            old_g <- group_id[j]; new_g <- group_id[i]
            group_id[group_id == old_g] <- new_g
          }
        }
      }
      group_id <- as.integer(factor(group_id, levels = unique(group_id)))
      suggested_models_unique$compat_group <- group_id

      # Build var→human-readable-label maps from the positionally aligned output/output_label fields
      var_label_maps <- lapply(seq_len(n), function(i) {
        vars   <- tolower(trimws(strsplit(suggested_models_unique$output[i],       ",\\s*")[[1]]))
        labels <- trimws(strsplit(suggested_models_unique$output_label[i], ",\\s*")[[1]])
        if (length(labels) != length(vars)) labels <- vars
        setNames(labels, vars)
      })

      # Build a header per group listing the shared PD endpoints using human-readable labels
      unique_groups <- unique(group_id)
      group_labels  <- setNames(vapply(unique_groups, function(g) {
        idx    <- which(group_id == g)
        shared <- names(which(table(unlist(pd_list[idx])) >= 2))
        if (length(shared) == 0) return("")
        display_labels <- vapply(shared, function(v) {
          for (i in idx) { if (v %in% names(var_label_maps[[i]])) return(var_label_maps[[i]][[v]]) }
          toupper(v)
        }, character(1))
        paste("Shared endpoint(s):", paste(display_labels, collapse = ", "))
      }, character(1)), as.character(unique_groups))

      suggested_models_unique <- suggested_models_unique[order(suggested_models_unique$compat_group), ]
    }

    # Create card UI, inserting a group header whenever the compatible group changes
    card_list <- list()
    last_group <- NULL
    for (i in seq_len(nrow(suggested_models_unique))) {
      model    <- suggested_models_unique[i, ]
      model_id <- paste0("qmodel_", i)

      if (is_multi) {
        g <- model$compat_group
        if (is.null(last_group) || g != last_group) {
          lbl <- group_labels[as.character(g)]
          card_list[[length(card_list) + 1]] <- div(
            style = paste0(
              "margin: 18px 0 6px 0; padding: 6px 10px; ",
              "background: #e8f4fd; border-left: 4px solid #0275d8; ",
              "border-radius: 3px; font-size: 0.85em; color: #1a5276;"
            ),
            tags$strong(paste0("Compatible Group ", g)),
            if (nzchar(lbl)) tags$span(paste0(" — ", lbl), style = "font-weight: normal;")
          )
          last_group <- g
        }
      }

      card_list[[length(card_list) + 1]] <- div(class = "question-card",
        fluidRow(
          column(1, checkboxInput(model_id, label = NULL, value = FALSE)),
          column(11,
            h4(model$display_name),
            p(
              if (!is.na(model$clinical_application) && nzchar(model$clinical_application) && model$clinical_application != "NA")
                model$clinical_application else model$description,
              style = "font-size: 0.9em; margin: 5px 0;"
            ),
            tags$small(
              paste0(
                "Modality: ", model$modality_type, " | ",
                "Type: ", model$model_type, " | ",
                "Area: ", model$therapeutic_area
              ),
              style = "color: #666;"
            )
          )
        )
      )
    }

    n_suggested_models(nrow(suggested_models_unique))
    question_models_displayed(suggested_models_unique)
    do.call(tagList, card_list)
  })

  # Launch CTS button
  output$launch_cts_button_ui <- renderUI({
    if (is.null(input$question_id) || input$question_id == "") {
      return(NULL)
    }
    
    q_row <- questions_df[questions_df$question_id == input$question_id, ]
    if (nrow(q_row) == 0) return(NULL)
    
    div(
      actionButton("launch_from_question", 
        label = "Launch CTS with Selected Model(s) →",
        class = "btn-primary btn-lg",
        style = "margin-top: 20px; width: 100%; background-color: #0275d8; border-color: #0275d8;"),
      align = "center"
    )
  })
  
  # Observer for launching CTS from question
  observeEvent(input$launch_from_question, {
    if (is.null(input$question_id) || input$question_id == "") {
      showNotification("Please select a question", type = "error")
      return()
    }
    
    q_row <- questions_df[questions_df$question_id == input$question_id, ]
    if (nrow(q_row) == 0) return()
    
    # Use the already-rendered (sorted) model list to ensure qmodel_i indices match the UI
    suggested_models_unique <- question_models_displayed()
    if (is.null(suggested_models_unique)) {
      showNotification("Models not yet loaded — please wait a moment and try again", type = "error")
      return()
    }
    
    # Collect checked models
    checked_models <- list()
    for (i in seq_len(nrow(suggested_models_unique))) {
      model_id <- paste0("qmodel_", i)
      if (!is.null(input[[model_id]]) && input[[model_id]]) {
        checked_models[[i]] <- suggested_models_unique[i, ]
      }
    }
    
    checked_models <- do.call(rbind, checked_models)
    
    # Validate selection
    required_models <- if (q_row$require_multi_model[1]) 2 else 1
    if (is.null(checked_models) || nrow(checked_models) == 0) {
      showNotification(
        paste("Please select at least 1 model"),
        type = "error"
      )
      return()
    }
    
    if (q_row$require_multi_model[1] && nrow(checked_models) < 2) {
      showNotification(
        paste("This question requires 2 models for comparison. Please select both."),
        type = "error"
      )
      return()
    }
    
    # Store selected models and question presets
    selected_from_question_models(checked_models)
    selected_question(q_row)
    
    # Write to global.R (bridge file)
    trial_presets <- list(
      trial_design = q_row$trial_design[1],
      n_arms = q_row$n_arms[1],
      enable_switch = q_row$enable_switch[1],
      suggested_weeks = q_row$suggested_weeks[1],
      question_title = q_row$question[1]
    )
    
    # Store model filenames in global.R
    model_filenames <- checked_models$filename

    # Derive cts_mode from question approach
    cts_mode_val <- if (q_row$approach[1] %in% c("Comparative Effectiveness")) {
      "comparison"
    } else if (q_row$approach[1] %in% c("Dose-Response Modeling")) {
      "dose_response"
    } else {
      "default"
    }

    global_content <- sprintf(
      'model_filenames <- c(%s)\ntrial_presets <- list(\n  trial_design = "%s",\n  n_arms = %d,\n  enable_switch = %s,\n  suggested_weeks = %d,\n  question_title = "%s",\n  auto_run = TRUE,\n  cts_mode = "%s"\n)',
      paste(sprintf('"%s"', model_filenames), collapse = ", "),
      q_row$trial_design[1],
      q_row$n_arms[1],
      as.character(q_row$enable_switch[1]),
      q_row$suggested_weeks[1],
      gsub('"', "'", q_row$question[1]),
      cts_mode_val
    )
    
    # Write to subdir/global.R
    global_path <- get_cts_path("global.R")
    writeLines(global_content, global_path)
    
    # Activate CTS app
    cts_app_active(TRUE)
    
    showNotification(
      paste("CTS configured for:", q_row$question[1]),
      type = "message"
    )
  })
  
  # ============================================================================
  # END QUESTION-FIRST WORKFLOW SERVER LOGIC
  # ============================================================================
  
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
    if (!is.null(input$filter_modality) && length(input$filter_modality) > 0) {
      m <- m[m$modality_type %in% input$filter_modality, ]
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
  if (!is.null(input$year_range)) {
    years <- extract_year(m$year)
    m <- m[!is.na(years) & years >= input$year_range[1] & years <= input$year_range[2], ]
  }
  return(m)
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
            onclick = sprintf("Shiny.setInputValue('card_clicked', %d, {priority: 'event'})", i),
            div(class = "model-card-accent"),
            div(
              class = "model-card-body",
              tags$h5(m_unique$display_name[i], style = "margin: 0 0 6px 0; font-weight: 700; color: #1e293b; font-size: 0.97em; line-height: 1.3;"),
              tags$p(m_unique$model_type[i], style = "margin: 0 0 4px 0; font-size: 0.78em; font-weight: 600; color: #2563eb; text-transform: uppercase; letter-spacing: 0.5px;"),
              tags$p(paste("Modality:", m_unique$modality_type[i]), style = "margin: 0 0 3px 0; font-size: 0.82em; color: #64748b;"),
              tags$p(m_unique$therapeutic_area_original[i], style = "margin: 0 0 2px 0; font-size: 0.82em; color: #64748b;"),
              tags$p(m_unique$compound[i], style = "margin: 0; font-size: 0.80em; color: #94a3b8;")
            )
          )
        } else {
          # Multi-selection card with checkbox
          # Multi-selection card with checkbox
          div(
            class = "model-card",
            div(class = "model-card-accent"),
            div(
              class = "model-card-body",
              div(style = "display:flex; align-items:flex-start; gap:8px;",
                  div(style = "padding-top:2px; flex-shrink:0; width:20px;",
                      checkboxInput(
                        inputId = paste0("select_model_", i),
                        label = NULL,
                        value = FALSE
                      )
                  ),
                  div(style = "flex:1; min-width:0;",
                      tags$h5(m_unique$display_name[i], style = "margin: 0 0 6px 0; font-weight: 700; color: #1e293b; font-size: 0.97em; line-height: 1.3;"),
                      tags$p(m_unique$model_type[i], style = "margin: 0 0 4px 0; font-size: 0.78em; font-weight: 600; color: #2563eb; text-transform: uppercase; letter-spacing: 0.5px;"),
                      tags$p(paste("Modality:", m_unique$modality_type[i]), style = "margin: 0 0 3px 0; font-size: 0.82em; color: #64748b;"),
                      tags$p(m_unique$therapeutic_area_original[i], style = "margin: 0 0 2px 0; font-size: 0.82em; color: #64748b;"),
                      tags$p(m_unique$compound[i], style = "margin: 0; font-size: 0.80em; color: #94a3b8;")
                  )
              )
            )
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

  # Model count indicator for Cards View
  output$model_cards_header <- renderUI({
    m_unique <- get_unique_models(filtered_meta())
    total    <- nrow(get_unique_models(meta))
    shown    <- nrow(m_unique)
    div(
      style = "color: #64748b; font-size: 0.88em; margin: 8px 0 12px 0; padding: 6px 0; border-bottom: 1px solid #f1f5f9;",
      tags$span(shown, style = "font-weight:700; color:#2563eb;"),
      paste0(" of ", total, " models")
    )
  })

  # Single observer for all card clicks — avoids accumulation on filter changes
  observeEvent(input$card_clicked, {
    i <- input$card_clicked
    m_click <- get_unique_models(filtered_meta())
    if (!is.null(i) && i >= 1 && i <= nrow(m_click)) {
      selected_model_data(m_click[i, ])
      selected_model(i)
      cts_app_active(FALSE)
    }
  }, ignoreInit = TRUE)
  
  # Model card section
  output$model_card_section <- renderUI({
    selected <- selected_model_data()
    if (is.null(selected)) return(NULL)

    tagList(
      div(style = "margin: 16px 0 20px 0;",
        actionButton("back_to_library", "\u2190 Back to Library",
          class = "btn btn-default btn-sm",
          style = "border-radius:6px; font-weight:500;")
      ),
      fluidRow(
        column(
          width = 8,
          div(
            style = "background:#fff; border:1px solid #e2e8f0; border-radius:12px; overflow:hidden; box-shadow:0 2px 8px rgba(0,0,0,0.06);",
            div(style = "height:6px; background:linear-gradient(90deg,#1e3a5f,#2563eb,#60a5fa);"),
            div(
              style = "padding: 24px 28px;",
              h3(selected$display_name, style = "margin: 0 0 6px 0; font-weight: 700; color: #1e293b;"),
              tags$span(
                selected$model_type,
                style = "display:inline-block; background:#eff6ff; color:#2563eb; font-size:0.78em; font-weight:600; text-transform:uppercase; letter-spacing:0.5px; padding:3px 10px; border-radius:12px; margin-bottom:16px;"
              ),
              p(selected$description, style = "color:#374151; margin-bottom:20px; font-size:0.93em; line-height:1.65;"),
              tags$hr(style = "border-color:#f1f5f9; margin: 0 0 16px 0;"),
              tags$table(
                style = "width:100%; border-collapse:collapse; font-size:0.91em;",
                tags$tr(tags$td(tags$b("Author"),            style = "color:#64748b; padding:5px 16px 5px 0; white-space:nowrap; width:160px;"), tags$td(selected$author)),
                tags$tr(tags$td(tags$b("Modality Type"),    style = "color:#64748b; padding:5px 16px 5px 0;"), tags$td(selected$modality_type)),
                tags$tr(tags$td(tags$b("Compound"),         style = "color:#64748b; padding:5px 16px 5px 0;"), tags$td(selected$compound)),
                tags$tr(tags$td(tags$b("Therapeutic Area"), style = "color:#64748b; padding:5px 16px 5px 0;"), tags$td(if (!is.null(selected$therapeutic_area_original)) selected$therapeutic_area_original else selected$therapeutic_area)),
                tags$tr(tags$td(tags$b("Indication"),       style = "color:#64748b; padding:5px 16px 5px 0;"), tags$td(if (!is.null(selected$indication_original)) selected$indication_original else selected$indication)),
                tags$tr(tags$td(tags$b("Applications"),     style = "color:#64748b; padding:5px 16px 5px 0;"), tags$td(selected$applications)),
                tags$tr(tags$td(tags$b("Source"),           style = "color:#64748b; padding:5px 16px 5px 0;"), tags$td(if (!is.na(selected$source) && nzchar(selected$source)) tags$a(href = selected$source, "View source", target = "_blank") else "N/A")),
                tags$tr(tags$td(tags$b("Validation Status"),style = "color:#64748b; padding:5px 16px 5px 0;"), tags$td(selected$validation_status)),
                tags$tr(tags$td(tags$b("Last Updated"),     style = "color:#64748b; padding:5px 16px 5px 0;"), tags$td(selected$date_last_updated))
              )
            )
          )
        ),
        column(
          width = 4,
          div(
            style = "background:#fff; border:1px solid #e2e8f0; border-radius:12px; padding:20px; box-shadow:0 2px 8px rgba(0,0,0,0.06);",
            p("ACTIONS", style = "margin: 0 0 14px 0; font-weight: 700; color: #94a3b8; font-size: 0.72em; text-transform: uppercase; letter-spacing: 1px;"),
            actionButton("view_model_code", "View Model Code",
              style = "width: 100%; margin-bottom:10px; border-radius:7px; font-weight:500;"),
            downloadButton("download_code", "Download Code",
              style = "width: 100%; margin-bottom:10px; border-radius:7px; font-weight:500;"),
            actionButton("launch_cts", "Use in Clinical Trial Simulator",
              class = "btn-primary",
              style = "width: 100%; border-radius:7px; font-weight:600;")
          )
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
        style = "padding: 14px 16px; background: #f8faff; border: 1px dashed #bfdbfe; border-radius: 8px; margin-bottom: 16px; color: #64748b; font-size: 0.9em;",
        tags$b("No models selected"),
        tags$span(" — check boxes on cards to select models for comparison.")
      ))
    }

    # Show selected models
    div(
      style = "padding: 16px 18px; background: #eff6ff; border-radius: 10px; margin-bottom: 16px; border: 2px solid #2563eb;",
      div(style = "display:flex; align-items:center; margin-bottom:10px;",
        tags$span(length(selected_indices),
          style = "display:inline-flex; align-items:center; justify-content:center; width:28px; height:28px; border-radius:50%; background:#2563eb; color:#fff; font-weight:700; font-size:0.9em; margin-right:10px;"),
        h5(paste0("model", if (length(selected_indices) > 1) "s" else "", " selected"),
          style = "margin:0; color:#1e3a5f; font-weight:600;")
      ),
      tags$ul(style = "margin: 0 0 14px 0; padding-left: 20px;",
        lapply(selected_indices, function(idx) {
          tags$li(m_unique$display_name[idx], style = "color:#1e3a5f; font-size:0.92em;")
        })
      ),
      actionButton("simulate_multi", "Simulate Selected Models",
                   class = "btn btn-primary",
                   style = "width: 100%; border-radius:7px; font-weight:600;")
    )
  })

  # Back to library button
  observeEvent(input$back_to_library, {
    selected_model(NULL)
    selected_model_data(NULL)
    cts_app_active(FALSE)
  })

  # Back from CTS (persistent top bar button)
  observeEvent(input$back_from_cts, {
    cts_app_active(FALSE)
  })
  
  # Launch CTS app
  observeEvent(input$launch_cts, {
    selected <- selected_model_data()
    if (is.null(selected)) return()

    withProgress(message = "Loading Clinical Trial Simulator...", value = 0.5, {
      global_path <- get_cts_path("global.R")
      writeLines(sprintf(
        'model_filenames <- c("%s")\ntrial_presets <- list(\n  auto_run = FALSE,\n  cts_mode = "default"\n)',
        selected$filename
      ), global_path)
    })

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
    writeLines(sprintf(
      'model_filenames <- c(%s)\ntrial_presets <- list(\n  auto_run = FALSE,\n  cts_mode = "default"\n)',
      paste(sprintf('"%s"', selected_filenames), collapse = ", ")
    ), global_path)
    
    # Launch CTS app with multiple models
    cts_app_active(TRUE)
  })
  
  # CTS app UI
  output$cts_app_ui <- renderUI({
    if (!cts_app_active()) return(NULL)
    withProgress(message = "Loading Clinical Trial Simulator...", value = 0.5, {
      tryCatch({
        source(get_cts_path("global.R"), local = TRUE)
        eval(parse(file = get_cts_path("CTS_ui.R")))
      }, error = function(e) {
        showNotification(paste("Error loading CTS UI:", e$message), type = "error", duration = NULL)
        cts_app_active(FALSE)
        div(class = "info-banner",
          p(paste("Failed to load Clinical Trial Simulator:", e$message), style = "margin: 0;")
        )
      })
    })
  })

  # CTS app server logic — observeEvent prevents re-firing on inputs touched by appServer
  observeEvent(cts_app_active(), {
    if (cts_app_active()) {
      tryCatch({
        source(get_cts_path("global.R"), local = TRUE)
        appServer <- eval(parse(file = get_cts_path("CTS_server.R")))
        appServer(input, output, session)
      }, error = function(e) {
        showNotification(paste("Error loading CTS server:", e$message), type = "error", duration = NULL)
      })
    }
  }, ignoreInit = TRUE)
  
  # Summary visualizations
  output$therapeutic_area_pie <- renderPlotly({
    shiny::req(input$view_type == "Summary View")
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
      margin = list(l = 120, r = 120, t = 60, b = 60),
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
    shiny::req(input$view_type == "Summary View")
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
      margin = list(l = 120, r = 120, t = 60, b = 60),
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
  
  output$modality_pie <- renderPlotly({
    shiny::req(input$view_type == "Summary View")
    m <- filtered_meta()
    modality_counts <- table(m$modality_type)
    
    # Remove N/A if present
    modality_counts <- modality_counts[names(modality_counts) != "N/A"]
    
    total_models <- sum(modality_counts)
    plot_ly(
      labels = names(modality_counts),
      values = as.numeric(modality_counts),
      type = 'pie',
      hole = 0.5,
      textposition = 'outside',
      textinfo = 'label',
      hoverinfo = 'text',
      text = ~paste(
        names(modality_counts), 
        "\nCount:", modality_counts,
        "\n(", round(100 * modality_counts/sum(modality_counts), 1), "%)"
      )
    ) %>% 
    layout(
      showlegend = FALSE,
      margin = list(l = 120, r = 120, t = 60, b = 60),
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
    summary_df <- m[, c("display_name", "model_type", "modality_type", "therapeutic_area", 
                       "indication", "compound", "validation_status", "year")]
    colnames(summary_df) <- c("Model Name", "Type", "Modality Type", "Therapeutic Area", 
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
    selected <- selected_model_data()
    if (is.null(selected)) return()

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
      selected <- selected_model_data()
      if (is.null(selected)) return("model.cpp")
      selected$filename
    },
    content = function(file) {
      selected <- selected_model_data()
      if (is.null(selected)) return()
      model_file <- get_model_path(selected$filename)
      if (file.exists(model_file)) {
        file.copy(model_file, file)
      }
    }
  )
}

ui <- ui1

shinyApp(ui, server)