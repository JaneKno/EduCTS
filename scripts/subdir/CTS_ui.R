library(shiny)
library(mrgsolve)
library(shinychat)

# Compute initial sidebar state here (before fluidPage) so it can be used
# in class= arguments below. trial_presets is already in scope because this
# file is eval(parse())'d inside renderUI after global.R has been sourced.
.sidebar_hidden_init <- isTRUE(trial_presets$auto_run)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  useShinyjs(),

  # ============================================================
  # SHARED DESIGN TOKENS — mirrors ModelLibrary.R CSS
  # ============================================================
  tags$head(tags$style(HTML("
    body { background-color: #f4f6fb; color: #1e293b; }

    .cts-subheader {
      background: #1e3a5f;
      padding: 14px 28px 12px 28px;
      display: flex; align-items: center; gap: 16px;
    }
    .cts-subheader .cts-icon {
      display: inline-flex; align-items: center; justify-content: center;
      width: 36px; height: 36px; border-radius: 8px;
      background: rgba(255,255,255,0.15); flex-shrink: 0;
      font-size: 1.1em; color: #fff;
    }
    .cts-subheader h3 { color: #fff; margin: 0 0 2px 0; font-size: 1.15em; font-weight: 700; letter-spacing: 0.2px; }
    .cts-subheader p  { color: rgba(255,255,255,0.72); margin: 0; font-size: 0.82em; }

    .nav-tabs { border-bottom: 2px solid #e2e8f0; background: #fff; padding: 0 20px; margin-bottom: 0; }
    .nav-tabs > li > a { color: #64748b; font-weight: 500; border: none !important; padding: 12px 18px; background: transparent !important; }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover { color: #2563eb; border-bottom: 3px solid #2563eb !important; background: transparent !important; font-weight: 600; }
    .tab-content { padding-top: 4px; }

    .well { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 10px; box-shadow: 0 1px 4px rgba(0,0,0,0.05); }

    .btn-primary { background-color: #2563eb; border-color: #1d4ed8; border-radius: 7px; font-weight: 500; box-shadow: 0 2px 6px rgba(37,99,235,0.25); }
    .btn-primary:hover { background-color: #1d4ed8; border-color: #1e40af; }
    .btn-default { border-radius: 6px; font-weight: 500; }

    .cts-section-label {
      font-size: 0.72em; font-weight: 700; color: #94a3b8;
      letter-spacing: 1px; text-transform: uppercase;
      margin: 18px 0 8px 0;
    }

    .irs--shiny .irs-bar { background: #2563eb; border-color: #2563eb; }
    .irs--shiny .irs-handle { border-color: #2563eb; }

    .cts-preset-banner {
      background: #eff6ff; border: 1px solid #bfdbfe;
      border-left: 4px solid #2563eb; border-radius: 6px;
      padding: 10px 14px; margin-bottom: 14px; font-size: 0.88em; color: #1e3a5f;
    }
    .cts-preset-banner strong { display: block; margin-bottom: 4px; color: #1e3a5f; }

    /* Info-icon tooltips (matches ModelLibrary.R) */
    .cts-info-icon {
      display: inline-flex; align-items: center; justify-content: center;
      width: 18px; height: 18px; border-radius: 50%;
      border: 2px solid #94a3b8; color: #64748b; font-size: 12px; font-weight: bold;
      margin-left: 6px; margin-right: 2px; cursor: help; flex-shrink: 0; position: relative;
      vertical-align: middle;
    }
    .cts-info-icon:hover::after {
      content: attr(data-tooltip);
      position: absolute; bottom: 24px; left: 50%; transform: translateX(-50%);
      background: #1e3a5f; color: white; padding: 10px 14px; border-radius: 4px;
      font-size: 12px; line-height: 1.4; word-wrap: break-word; white-space: normal; 
      z-index: 1000; font-weight: normal; width: 240px;
      text-align: center; pointer-events: none; box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    }
    .cts-info-icon:hover::before {
      content: '';
      position: absolute; bottom: 18px; left: 50%; transform: translateX(-50%);
      width: 0; height: 0;
      border-left: 6px solid transparent; border-right: 6px solid transparent;
      border-top: 6px solid #1e3a5f; z-index: 1000; pointer-events: none;
    }

    /* Help section styles */
    .help-content { max-width: 1000px; margin: 0 auto; padding: 20px; }
    .help-toc-container {
      display: grid; grid-template-columns: 220px 1fr; gap: 24px;
      margin-bottom: 20px;
    }
    @media (max-width: 768px) {
      .help-toc-container { grid-template-columns: 1fr; }
      .help-toc { position: static; width: 100%; margin-bottom: 20px; }
    }
    .help-toc {
      position: sticky; top: 20px; height: fit-content;
      background: #f8fafc; border: 1px solid #e2e8f0;
      border-radius: 6px; padding: 12px; font-size: 0.88em;
    }
    .help-toc-title {
      font-weight: 700; color: #1e3a5f; margin-bottom: 10px;
      padding-bottom: 8px; border-bottom: 2px solid #e2e8f0;
    }
    .help-toc-item {
      cursor: pointer; padding: 6px 8px; margin: 4px 0;
      border-radius: 4px; color: #2563eb;
      transition: background-color 0.2s ease;
    }
    .help-toc-item:hover {
      background: #e0e7ff; color: #1d4ed8;
    }
    .help-main {
      flex: 1;
    }
    .help-section { margin: 0 0 16px 0; }
    .help-section details {
      border: none; padding: 0; margin: 0;
    }
    .help-section summary {
      cursor: pointer; font-weight: 600; color: #1e3a5f;
      padding: 10px 0; user-select: none;
      font-size: 1.05em; margin-bottom: 0;
    }
    .help-section summary:hover {
      color: #2563eb;
    }
    .help-section summary::before {
      content: '▶ '; display: inline-block; width: 16px;
      transform: rotate(0deg); transition: transform 0.2s ease;
      color: #2563eb;
    }
    .help-section details[open] summary::before {
      transform: rotate(90deg);
    }
    .help-section-content {
      padding: 12px 0 16px 20px; margin-top: 8px;
    }
    .help-content h4 { color: #1e3a5f; font-weight: 700; margin-top: 0; margin-bottom: 12px; }
    .help-content h5 { color: #2563eb; font-weight: 600; margin-top: 16px; margin-bottom: 10px; }
    .help-content h6 { color: #1e3a5f; font-weight: 600; margin-bottom: 8px; }
    .help-content p { line-height: 1.6; color: #374151; margin: 8px 0; }
    .help-content ul, .help-content ol { margin-left: 20px; margin-bottom: 8px; }
    .help-content li { margin-bottom: 6px; line-height: 1.6; color: #374151; }

    .description-box {
      background: #f8fafc; border: 1px solid #e2e8f0; border-left: 4px solid #2563eb;
      border-radius: 6px; padding: 14px; margin: 12px 0; font-size: 0.95em;
    }
    .description-box p { margin: 8px 0; }
    .description-box ul { margin-left: 18px; }
    .description-box li { margin-bottom: 6px; }

    .help-two-column { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 16px 0; }
    @media (max-width: 768px) { .help-two-column { grid-template-columns: 1fr; } }

    /* Sidebar collapse */
    #cts_sidebar_col.sidebar-hidden { display: none !important; }
    #cts_main_col.col-sm-8 { transition: none; }
    #cts_main_col.expanded { width: 100% !important; max-width: 100% !important; flex: 0 0 100% !important; }
    #cts_main_col.full-width { width: 100% !important; max-width: 100% !important; flex: 0 0 100% !important; }
  "))),

  # ============================================================
  # CTS SUB-HEADER — visually links to the Model Library header
  # ============================================================
  div(
    class = "cts-subheader",
    style = "justify-content: space-between;",
    div(
      style = "display: flex; align-items: center; gap: 16px;",
      div(class = "cts-icon", icon("flask")),
      div(
        h3("EduCTS: Educational Clinical Trial Simulator"),
        p("Configure and run virtual/in-silico trials using the selected model(s).")
      )
    ),
    div(style = "display:flex; gap:8px; align-self:center;",
      actionButton("back_from_cts", "\u2190 Model Library",
        class = "btn btn-sm",
        style = "background: rgba(255,255,255,0.12); color: rgba(255,255,255,0.85); border: 1px solid rgba(255,255,255,0.25); border-radius: 6px; font-weight: 500; font-size: 0.83em; padding: 5px 12px; margin: 0;")
    )
  ),

  fluidRow(
    # ---- Sidebar column ----
    div(
      id    = "cts_sidebar_col",
      class = paste0("col-sm-4", if (.sidebar_hidden_init) " sidebar-hidden" else ""),
      div(
        class = "well",

        div(id = "trial_preset_banner"),

        p("TRIAL DESIGN", class = "cts-section-label"),

        shinyjs::hidden(
          radioButtons(
            inputId  = "trial_design",
            label    = "Type of Trial",
            choices  = c("Parallel" = "parallel", "Cross-over" = "cross-over", "Factorial" = "factorial"),
            selected = if (!is.null(trial_presets$trial_design)) trial_presets$trial_design else "parallel"
          )
        ),

        uiOutput("treatment_switch_toggle_ui"),
        uiOutput("treatment_switch_details_ui"),

        tags$hr(),

        numericInput(
          inputId = "trial_duration",
          label   = "Trial duration (weeks)",
          value   = if (!is.null(trial_presets$suggested_weeks)) trial_presets$suggested_weeks else 24,
          min     = 1
        ),

        tags$hr(),

        numericInput(inputId = "n_trials", label = "Number of Studies", value = 1, min = 1),

        tags$hr(),

        selectInput(
          inputId  = "time_unit",
          label    = "Output Time Unit",
          choices  = c("Hours" = "hours", "Days" = "days", "Weeks" = "weeks", "Months" = "months"),
          selected = "days"
        ),
        tags$hr(),

        selectInput(
          inputId  = "interval_type",
          label    = HTML("Interval Type <span class='cts-info-icon' data-tooltip='CI (Confidence Interval) shows precision of the model&#39;s estimate. PI (Prediction Interval) shows the range of individual subject responses. Use PI to see realistic patient-to-patient variation.'>?</span>"),
          choices  = c("Confidence Interval (CI)" = "ci", "Prediction Interval (PI)" = "pi"),
          selected = "pi"
        ),

        tags$hr(),

        checkboxInput(
          inputId = "include_variability",
          label = HTML("Include Variability <span class='cts-info-icon' data-tooltip='When checked, simulations include inter-individual variability and residual error, showing realistic trial-to-trial variation. Unchecked shows the typical population response without random effects.'>?</span>"),
          value = TRUE
        ),

        tags$hr(),

        uiOutput("dosing_ui"),

        tags$hr(),

        uiOutput("output_mapping_ui"),

        tags$hr(),

        actionButton(
          inputId = "run_sim",
          label   = "Run Simulation",
          class   = "btn btn-primary",
          icon    = icon("play"),
          style   = "width: 100%; border-radius: 7px; font-weight: 600;"
        ),
        tags$hr(),

        p("MODEL", class = "cts-section-label"),
        textOutput("model_info")
      )
    ),

    # ---- Main panel column ----
    div(
      id    = "cts_main_col",
      class = paste0("col-sm-8", if (.sidebar_hidden_init) " expanded" else ""),

      tabsetPanel(
        id = "cts_main_tabset",
        tabPanel(
          "Virtual Trial Results",
          div(
            style = "display: flex; align-items: center; justify-content: space-between; margin: 20px 0 12px 0;",
            tags$h3("Simulation Results", style = "color: #1e293b; font-weight: 700; margin: 0;"),
            actionButton("toggle_sidebar", tagList(icon("sliders"), " Parameters"),
              class = "btn btn-sm btn-default",
              title = "Toggle parameters panel",
              style = "font-size: 0.83em; font-weight: 500;")
          ),
          tags$div(
            style = paste0(
              "background: #fffbeb; border: 1px solid #fcd34d; border-left: 4px solid #f59e0b; ",
              "border-radius: 6px; padding: 10px 14px; margin-bottom: 16px; ",
              "font-size: 0.87em; color: #78350f; line-height: 1.6;"
            ),
            tags$b("Note: "),
            "Simulated variability reflects inter-individual variability, residual error, and the impact of trial design per FDA M15-recognized best practices. ",
            "Parameter uncertainty — the precision with which model parameters were estimated from clinical data — is not accounted for. ",
            tags$a(href = "#help_evidence", onclick = "document.getElementById('help_evidence').scrollIntoView({behavior:'smooth'}); document.getElementById('help_evidence').open = true; return false;", 
              "See Evidence section", style = "color: #1e40af; text-decoration: underline;")
          ),
          shinycssloaders::withSpinner(uiOutput("sim_result"))
        ),
        tabPanel(
          "Model Evidence",
          tags$h3("Model Validation",
            style = "color: #1e293b; font-weight: 700; margin: 20px 0 12px 0;"),
          tags$p(
            "In this section, the model simulations are validated again either published model simulations (internal validation), study data which was used for model building (internal validation) or study data not employed for model building (external validation). Different ways of plotting the model predictions versus the original data are employed. You can select which type of validation you want to assess.",
            style = "color: #64748b; font-size: 0.93em; line-height: 1.65;"
          ),
          uiOutput("validation_ui"),
          tags$hr(),
          uiOutput("validation_selector"),
          shinycssloaders::withSpinner(uiOutput("validation_result"))
        ),
        
        # ============================================================================
        # TAB 3: HELP & GUIDE
        # ============================================================================
        tabPanel(
          "Help & Guide",
          value = "cts_help_tab",
          div(class = "help-content",
            div(class = "help-toc-container",
              # ========== TABLE OF CONTENTS SIDEBAR ==========
              div(class = "help-toc",
                div(class = "help-toc-title", "📋 Quick Links"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_quickstart').scrollIntoView({behavior:'smooth'}); document.getElementById('help_quickstart').open = true;", "1. Quick Start"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_variability').scrollIntoView({behavior:'smooth'}); document.getElementById('help_variability').open = true;", "2. Variability"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_cipi').scrollIntoView({behavior:'smooth'}); document.getElementById('help_cipi').open = true;", "3. CI vs PI"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_switch').scrollIntoView({behavior:'smooth'}); document.getElementById('help_switch').open = true;", "4. Treatment Switch"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_results').scrollIntoView({behavior:'smooth'}); document.getElementById('help_results').open = true;", "5. Understanding Results"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_scenarios').scrollIntoView({behavior:'smooth'}); document.getElementById('help_scenarios').open = true;", "6. Use Cases"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_glossary').scrollIntoView({behavior:'smooth'}); document.getElementById('help_glossary').open = true;", "7. Glossary"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_tips').scrollIntoView({behavior:'smooth'}); document.getElementById('help_tips').open = true;", "8. Tips & Practices"),
                div(class = "help-toc-item", onclick = "document.getElementById('help_evidence').scrollIntoView({behavior:'smooth'}); document.getElementById('help_evidence').open = true;", "9. Evidence & Credibility")
              ),
              
              # ========== MAIN CONTENT AREA ==========
              div(class = "help-main",
                # 1. QUICK START
                div(class = "help-section",
                  tags$details(id = "help_quickstart", open = TRUE,
                    tags$summary("Quick Start: 6-Step Simulation Workflow"),
                    div(class = "help-section-content",
                      div(class = "description-box",
                        tags$ol(
                          tags$li(tags$strong("Set Trial Duration:"), " Specify trial length in weeks and number of study replicates."),
                          tags$li(tags$strong("(Optional) Configure Treatment Switch:"), " If simulating a multi-phase trial, enable and configure the switch point and models."),
                          tags$li(tags$strong("Configure Treatment Groups & Dosing:"), " Add treatment arms, specify sample sizes, dose amounts, and dosing frequency."),
                          tags$li(tags$strong("Choose Interval Type:"), " Select CI (Confidence Interval) for parameter precision or PI (Prediction Interval) for individual subject outcomes."),
                          tags$li(tags$strong("Enable Variability:"), " Check to include inter-individual and residual error; uncheck for typical population response."),
                          tags$li(tags$strong("Run Simulation:"), " Click the blue 'Run Simulation' button and view results in the 'Virtual Trial Results' tab.")
                        ),
                        style = "margin-top: 12px;"
                      )
                    )
                  )
                ),
                
                # 2. VARIABILITY
                div(class = "help-section",
                  tags$details(id = "help_variability",
                    tags$summary("What Does 'Include Variability' Mean?"),
                    div(class = "help-section-content",
                      div(class = "description-box",
                        tags$p("Simulated variability reflects two independent sources of uncertainty in clinical trials:"),
                        tags$ul(
                          tags$li(tags$strong("Inter-Individual Variability (IIV):"), " Natural differences between subjects due to genetics, age, weight, disease severity, and other physiological factors. This is why two patients on the same dose have different responses."),
                          tags$li(tags$strong("Residual Error:"), " Measurement error, model misspecification, and other unexplained variation in the data.")
                        ),
                        tags$p(tags$strong("When 'Include Variability' is CHECKED:"), style = "margin-top: 12px;"),
                        tags$ul(
                          tags$li("Simulations show realistic patient-to-patient variability and trial-to-trial variation"),
                          tags$li("Each simulation run will show different outcomes (use multiple replicates to assess mean and distribution)"),
                          tags$li("Recommended for: Phase II/III efficacy trials, safety assessments, responder rate estimation")
                        ),
                        tags$p(tags$strong("When 'Include Variability' is UNCHECKED:"), style = "margin-top: 12px;"),
                        tags$ul(
                          tags$li("Simulations show only the typical population response (median individual)"),
                          tags$li("No random variation between replicates (deterministic)"),
                          tags$li("Recommended for: Exploratory dose selection, proof-of-concept with small sample sizes, educational purposes")
                        )
                      )
                    )
                  )
                ),
                
                # 3. CI vs PI
                div(class = "help-section",
                  tags$details(id = "help_cipi",
                    tags$summary("Confidence Interval (CI) vs. Prediction Interval (PI)"),
                    div(class = "help-section-content",
                      div(class = "description-box",
                        tags$p(tags$strong("Important:"), " The CI vs. PI choice only matters when ", tags$em("'Include Variability' is CHECKED"), ". When variability is unchecked, the interval shown represents the model's point estimate without variation.")
                      ),
                      div(class = "help-two-column",
                        div(
                          tags$h6("Confidence Interval (CI)", style = "color: #2563eb; margin-top: 0;"),
                          tags$strong("What it shows:"), " The typical population response (median individual) without inter-individual variation",
                          tags$p("• Narrowest representation of population response"),
                          tags$p("• Shows the average/median patient outcome"),
                          tags$p("• Does NOT include patient-to-patient differences"),
                          tags$p(tags$strong("Use CI when:"), " You want to see the average expected outcome for a typical patient in the trial population")
                        ),
                        div(
                          tags$h6("Prediction Interval (PI)", style = "color: #2563eb; margin-top: 0;"),
                          tags$strong("What it shows:"), " Range where individual patient responses will fall in the trial population",
                          tags$p("• Wider than CI = accounts for inter-individual variability"),
                          tags$p("• Shows realistic distribution of responses across patients"),
                          tags$p("• Reflects what you'll actually observe in a real trial"),
                          tags$p(tags$strong("Use PI when:"), " You want to see the diverse patient responses; assess safety margins; predict responder/non-responder rates")
                        )
                      )
                    )
                  )
                ),
                
                # 4. TREATMENT SWITCH
                div(class = "help-section",
                  tags$details(id = "help_switch",
                    tags$summary("Treatment Switch: Multi-Phase Trials"),
                    div(class = "help-section-content",
                      div(class = "description-box",
                        tags$p("A treatment switch allows all subjects to transition from one drug/model to another at a specified timepoint during the trial."),
                        tags$p(tags$strong("Common Scenarios:"), style = "margin-top: 12px;"),
                        tags$ul(
                          tags$li(tags$strong("Dose Escalation:"), " Start at low dose (Drug A), escalate to therapeutic dose (Drug B) based on safety/PK profile."),
                          tags$li(tags$strong("Therapy Switching:"), " Switch due to inadequate response or adverse event."),
                          tags$li(tags$strong("Sequential Therapy:"), " Two-drug combination where each drug has distinct phases.")
                        ),
                        tags$p(tags$strong("Configuration Steps:"), style = "margin-top: 12px;"),
                        tags$ol(
                          tags$li("Check 'Enable Treatment Switch During Trial'"),
                          tags$li("Select Phase 1 and Phase 2 models"),
                          tags$li("Specify switch timepoint in days"),
                          tags$li("Review parameter/output mappings")
                        )
                      )
                    )
                  )
                ),
                
                # 5. UNDERSTANDING RESULTS
                div(class = "help-section",
                  tags$details(id = "help_results",
                    tags$summary("Understanding Your Results"),
                    div(class = "help-section-content",
                      tags$h5("Virtual Trial Results Tab", style = "margin-top: 0;"),
                      div(class = "description-box",
                        tags$p("This tab displays simulation outputs as time-series plots."),
                        tags$p(tags$strong("How to Read:"), style = "margin-top: 12px;"),
                        tags$ul(
                          tags$li(tags$strong("Solid Line:"), " Median response"),
                          tags$li(tags$strong("Shaded Region:"), " CI or PI (depending on selection)")
                        )
                      ),
                      tags$h5("Model Evidence Tab", style = "margin-top: 16px;"),
                      div(class = "description-box",
                        tags$p("Compares model predictions against observed clinical trial data."),
                        tags$p(tags$strong("What to Look For:"), style = "margin-top: 12px;"),
                        tags$ul(
                          tags$li("Points close to line: Good model fit"),
                          tags$li("Systematic bias: Model may overpredict or underpredict"),
                          tags$li("Wide scatter: Large residual error or high IIV")
                        )
                      )
                    )
                  )
                ),
                
                # 6. COMMON USE CASES
                div(class = "help-section",
                  tags$details(id = "help_scenarios",
                    tags$summary("Common Simulation Scenarios"),
                    div(class = "help-section-content",
                      tags$h5("Phase II Dose-Finding", style = "margin-top: 0;"),
                      div(class = "description-box",
                        tags$strong("Goal: "), "Identify optimal dose",
                        tags$ul(
                          tags$li("N = 30-50 per arm"),
                          tags$li("Interval: PI"),
                          tags$li("Variability: YES")
                        )
                      ),
                      tags$h5("Phase III Efficacy", style = "margin-top: 12px;"),
                      div(class = "description-box",
                        tags$strong("Goal: "), "Demonstrate superiority",
                        tags$ul(
                          tags$li("N = 100-200 per arm"),
                          tags$li("Interval: CI"),
                          tags$li("Variability: YES")
                        )
                      ),
                      tags$h5("Dose Escalation", style = "margin-top: 12px;"),
                      div(class = "description-box",
                        tags$strong("Goal: "), "Safely escalate dose",
                        tags$ul(
                          tags$li("N = 20-30 subjects"),
                          tags$li("Treatment Switch: YES"),
                          tags$li("Variability: YES")
                        )
                      ),
                      tags$h5("Safety Assessment", style = "margin-top: 12px;"),
                      div(class = "description-box",
                        tags$strong("Goal: "), "Evaluate safety margins",
                        tags$ul(
                          tags$li("Interval: PI (see tail for outliers)"),
                          tags$li("Variability: YES (critical)"),
                          tags$li("Focus: Upper bound acceptable?")
                        )
                      )
                    )
                  )
                ),
                
                # 7. GLOSSARY
                div(class = "help-section",
                  tags$details(id = "help_glossary",
                    tags$summary("Glossary of Terms"),
                    div(class = "help-section-content",
                      div(class = "description-box",
                        tags$p(tags$strong("Biomarker:"), " A measurable indicator of biological function (e.g., TTR, apoB, LDL cholesterol)."),
                        tags$p(tags$strong("CI (Confidence Interval):"), " Range reflecting precision of parameter estimation."),
                        tags$p(tags$strong("Endpoint:"), " Primary/secondary outcome measured in trial (efficacy, safety, or PK)."),
                        tags$p(tags$strong("IIV:"), " Inter-individual variability; natural differences between subjects in drug response."),
                        tags$p(tags$strong("Parameter:"), " Model input quantifying drug/disease properties (Ka, CL, Emax, etc.)."),
                        tags$p(tags$strong("PI (Prediction Interval):"), " Range where individual subject responses will fall; accounts for variability."),
                        tags$p(tags$strong("Residual Error:"), " Unexplained variation; includes measurement error and model misspecification."),
                        tags$p(tags$strong("Validation:"), " Comparison of model predictions vs. observed clinical data.")
                      )
                    )
                  )
                ),
                
                # 8. TIPS & BEST PRACTICES
                div(class = "help-section",
                  tags$details(id = "help_tips",
                    tags$summary("Tips & Best Practices"),
                    div(class = "help-section-content",
                      div(class = "description-box",
                        tags$h5("Sample Size Guidance:", style = "margin-top: 0;"),
                        tags$ul(
                          tags$li("Phase I: 20-30 subjects"),
                          tags$li("Phase II: 50-100 subjects per arm"),
                          tags$li("Phase III: 300+ subjects per arm")
                        ),
                        tags$h5("Choosing CI vs. PI:", style = "margin-top: 12px;"),
                        tags$ul(
                          tags$li(tags$strong("Note:"), " Only applies when 'Include Variability' is CHECKED."),
                          tags$li(tags$strong("Use CI:"), " See average expected outcome for typical patient"),
                          tags$li(tags$strong("Use PI:"), " See range of individual responses")
                        ),
                        tags$h5("Troubleshooting:", style = "margin-top: 12px;"),
                        tags$ul(
                          tags$li("Fill all required fields (duration, sample size, dose, frequency)"),
                          tags$li("Use biologically realistic dose values"),
                          tags$li("Check error messages for specific guidance")
                        )
                      )
                    )
                  )
                ),
                
                # 9. EVIDENCE & CREDIBILITY
                div(class = "help-section",
                  tags$details(id = "help_evidence",
                    tags$summary("How Does MIDD Improve My Trials? Evidence from Pfizer"),
                    div(class = "help-section-content",
                      div(class = "description-box",
                        tags$h5("Regulatory Precedent", style = "margin-top: 0; color: #2563eb;"),
                        tags$p("FDA M15 Guidance (December 2024) recognizes model-informed drug development (MIDD) as a tool to reduce trial drug-related uncertainty. The FDA's Quantitative Medicine Center of Excellence coordinates consistent MIDD application across drug reviews.")
                      ),
                      div(class = "description-box",
                        tags$h5("Real-World Savings: Pfizer Portfolio Analysis"),
                        tags$p(tags$strong("2021-2023 Portfolio Results:")),
                        tags$table(style = "width: 100%; border-collapse: collapse; margin: 8px 0;",
                          tags$tr(style = "background: #f0f9ff; border-bottom: 1px solid #bfdbfe;",
                            tags$td(style = "padding: 6px 8px; font-weight: 600; color: #0c4a6e;", "Metric"),
                            tags$td(style = "padding: 6px 8px; font-weight: 600; color: #0c4a6e;", "Value")
                          ),
                          tags$tr(style = "border-bottom: 1px solid #e2e8f0;",
                            tags$td(style = "padding: 6px 8px;", "Average Cycle Time Savings"),
                            tags$td(style = "padding: 6px 8px; color: #059669;", tags$strong("10 months per program"))
                          ),
                          tags$tr(style = "border-bottom: 1px solid #e2e8f0;",
                            tags$td(style = "padding: 6px 8px;", "Average Cost Savings"),
                            tags$td(style = "padding: 6px 8px; color: #059669;", tags$strong("$5M per program"))
                          ),
                          tags$tr(style = "background: #fef3c7;",
                            tags$td(style = "padding: 6px 8px;", "Largest Impact"),
                            tags$td(style = "padding: 6px 8px;", "Pediatric PK (up to 4 years saved)")
                          )
                        ),
                        tags$p("These savings translate to medicines reaching patients faster and clinical programs advancing more efficiently.", style = "margin-top: 8px;")
                      ),
                      div(class = "description-box",
                        tags$h5("Your Simulation Features: Examples from Practice"),
                        div(style = "background: #f0f9ff; border-left: 4px solid #0284c7; border-radius: 4px; padding: 10px; margin: 8px 0;",
                          tags$strong("Dose-Finding (CI vs PI):"),
                          tags$p("Bayesian adaptive design with prediction intervals enabled 18-month acceleration in complex hemophilia trial by enrolling highest-probability dose first.", style = "margin: 4px 0;")
                        ),
                        div(style = "background: #f0f9ff; border-left: 4px solid #0284c7; border-radius: 4px; padding: 10px; margin: 8px 0;",
                          tags$strong("Safety Margin Assessment (Variability):"),
                          tags$p("Virtual patient populations with inter-individual variability eliminated need for separate organ-impairment studies during COVID-19 development, saving $250K + 3-6 months enrollment.", style = "margin: 4px 0;")
                        ),
                        div(style = "background: #f0f9ff; border-left: 4px solid #0284c7; border-radius: 4px; padding: 10px; margin: 8px 0;",
                          tags$strong("Pediatric Extrapolation (Population PK):"),
                          tags$p("Population PK modeling extrapolated adult/pediatric data to support dosing in children without Phase I/II pediatric trial. Savings: $17M + 5 years of development time.", style = "margin: 4px 0;")
                        )
                      ),
                      div(class = "description-box",
                        tags$p(tags$strong("Learn More:"), tags$a(href = "https://www.fda.gov/media/184747/download", target = "_blank", "FDA M15 Guidance PDF", style = "color: #2563eb; text-decoration: underline;"))
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # ============================================================================
        # TAB 4: FEEDBACK
        # ============================================================================
        tabPanel(
          "Feedback",
          value = "cts_feedback_tab",
          div(style = "max-width: 900px; margin: 0 auto; padding: 20px;",
            tags$h3("Help Us Improve EduCTS",
              style = "color: #1e293b; font-weight: 700; margin: 20px 0 12px 0;"),
            tags$p(
              "Your feedback is valuable and helps us improve the simulator. This survey takes about 5-10 minutes.",
              style = "color: #64748b; font-size: 0.95em; margin-bottom: 20px; line-height: 1.6;"
            ),
            
            # Feedback form container
            div(id = "feedback_form",
              # Optional metadata
              div(style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 16px; margin-bottom: 20px;",
                selectInput("fb_user_role", "Your role (optional):",
                  choices = c("Select..." = "", 
                              "Clinician/Physician" = "Clinician",
                              "Pharmacokineticist/Modeler" = "Modeler",
                              "Student" = "Student",
                              "Researcher" = "Researcher",
                              "Other" = "Other"),
                  selected = "")
              ),
              
              # SECTION 1: Workflow & Navigation
              div(style = "background: #fff1f2; border-left: 4px solid #ec4899; border-radius: 8px; padding: 16px; margin-bottom: 20px;",
                tags$h4("1. Workflow & Navigation", style = "margin: 0 0 14px 0; color: #1e293b;"),
                tags$p("How intuitive did you find the overall simulator workflow?", style = "margin: 0 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q1_intuitive", "Not intuitive (1) → Very intuitive (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                textAreaInput("fb_q1_comments", "Comments or suggestions:", rows = 2, placeholder = "Optional"),
                
                tags$p("How easy was it to follow the logical flow from trial design to results?", 
                  style = "margin: 12px 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q2_flow", "Difficult (1) → Very easy (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                textAreaInput("fb_q2_comments", "What could be clearer?", rows = 2, placeholder = "Optional")
              ),
              
              # SECTION 2: Question-First Workflow
              div(style = "background: #fef3c7; border-left: 4px solid #f59e0b; border-radius: 8px; padding: 16px; margin-bottom: 20px;",
                tags$h4("2. Question-First Workflow", style = "margin: 0 0 14px 0; color: #1e293b;"),
                tags$p("If you used the question-based entry workflow, how helpful was it?", style = "margin: 0 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q3_helpful", "Not helpful (1) → Very helpful (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                textAreaInput("fb_q3_comments", "What was helpful or missing?", rows = 2, placeholder = "Optional"),
                
                tags$p("Did the suggested trial settings guide your decisions effectively?", style = "margin: 12px 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q4_guide", "No guidance (1) → Excellent guidance (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%")
              ),
              
              # SECTION 3: Trial Design Configuration
              div(style = "background: #dbeafe; border-left: 4px solid #0284c7; border-radius: 8px; padding: 16px; margin-bottom: 20px;",
                tags$h4("3. Trial Design Configuration", style = "margin: 0 0 14px 0; color: #1e293b;"),
                tags$p("How clear was the trial design sidebar (treatment groups, dosing, etc.)?", style = "margin: 0 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q5_design_clear", "Very confusing (1) → Crystal clear (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                textAreaInput("fb_q5_comments", "What was confusing?", rows = 2, placeholder = "Optional"),
                
                tags$p("What features or options were missing from trial configuration?", style = "margin: 12px 0 8px 0; font-weight: 500;"),
                textAreaInput("fb_q6_missing", "Feature requests:", rows = 2, placeholder = "E.g., support for dropout rates, dose adjustments, etc.")
              ),
              
              # SECTION 4: Results & Visualization
              div(style = "background: #dcfce7; border-left: 4px solid #22c55e; border-radius: 8px; padding: 16px; margin-bottom: 20px;",
                tags$h4("4. Results & Visualization", style = "margin: 0 0 14px 0; color: #1e293b;"),
                tags$p("How clear were the plots and visual representations of results?", style = "margin: 0 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q7_plots_clear", "Hard to read (1) → Very clear (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                textAreaInput("fb_q7_comments", "Visualization feedback:", rows = 2, placeholder = "Optional"),
                
                tags$p("How easy was it to interpret what the results mean?", style = "margin: 12px 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q8_interpret", "Unclear (1) → Very clear (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                textAreaInput("fb_q8_comments", "What needed better explanation?", rows = 2, placeholder = "Optional"),
                
                tags$p("How confident are you in the simulation results?", style = "margin: 12px 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q9_confidence", "Not confident (1) → Very confident (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%")
              ),
              
              # SECTION 5: Model Evidence
              div(style = "background: #ede9fe; border-left: 4px solid #a855f7; border-radius: 8px; padding: 16px; margin-bottom: 20px;",
                tags$h4("5. Model Evidence & Validation", style = "margin: 0 0 14px 0; color: #1e293b;"),
                tags$p("How compelling was the model validation evidence (plots vs. observed data)?", style = "margin: 0 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q10_validation", "Not compelling (1) → Very compelling (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                textAreaInput("fb_q10_comments", "What would strengthen the evidence?", rows = 2, placeholder = "Optional")
              ),
              
              # SECTION 6: Help & Documentation
              div(style = "background: #fce7f3; border-left: 4px solid #db2777; border-radius: 8px; padding: 16px; margin-bottom: 20px;",
                tags$h4("6. Help & Documentation", style = "margin: 0 0 14px 0; color: #1e293b;"),
                tags$p("How useful was the Help & Guide section?", style = "margin: 0 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q11_help_useful", "Not useful (1) → Very useful (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                textAreaInput("fb_q11_comments", "Did you find information easily?", rows = 2, placeholder = "Optional"),
                
                tags$p("How well did explanations match your knowledge level?", style = "margin: 12px 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q12_explanations", "Too simple (1) ← → Too technical (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%")
              ),
              
              # SECTION 7: Overall Experience
              div(style = "background: #f0fdf4; border-left: 4px solid #16a34a; border-radius: 8px; padding: 16px; margin-bottom: 20px;",
                tags$h4("7. Overall Experience", style = "margin: 0 0 14px 0; color: #1e293b;"),
                tags$p("Would you use EduCTS for your own work (or recommend to colleagues)?", style = "margin: 0 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q13_again", "No (1) → Definitely yes (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                
                tags$p("How likely are you to recommend EduCTS to colleagues?", style = "margin: 12px 0 8px 0; font-weight: 500;"),
                sliderInput("fb_q14_recommend", "Very unlikely (1) → Very likely (5):",
                  min = 1, max = 5, value = 3, step = 1, width = "100%"),
                
                tags$p("What features would make EduCTS more useful for your needs?", style = "margin: 12px 0 8px 0; font-weight: 500;"),
                textAreaInput("fb_q15_features", "Feature requests and improvement ideas:", rows = 3, placeholder = "E.g., support for more models, advanced analyses, export options, etc.")
              ),
              
              # Submit button
              div(style = "text-align: center; margin-top: 24px;",
                actionButton("submit_feedback", "Submit Feedback",
                  class = "btn btn-primary",
                  icon = icon("paper-plane"),
                  style = "width: 200px; padding: 10px 20px; font-size: 1.05em; font-weight: 600;")
              )
            )
          )
        )
      )
    )
  )
)
