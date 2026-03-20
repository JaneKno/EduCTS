library(shiny)
library(mrgsolve)
library(shinychat)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
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
        width: 250px;
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
      .section-header {
        font-weight: bold;
        font-size: 1.1em;
        margin-top: 15px;
        margin-bottom: 10px;
        color: #2c3e50;
        border-bottom: 2px solid #ecf0f1;
        padding-bottom: 8px;
      }
      .param-label-with-icon {
        display: flex;
        align-items: center;
        margin-bottom: 5px;
      }
    "))
  ),
  
  titlePanel("EduCTS - Educational Clinical Trial Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      # ========== Trial Configuration Section ==========
      div(class = "section-header", "Trial Configuration"),
      
      # Trial design selection
      div(class = "param-label-with-icon",
        tags$label("Type of Trial", style = "margin-bottom: 0;"),
        div(class = "info-icon", `data-tooltip` = "Parallel: Multiple groups receive different treatments. Cross-over: Groups receive treatments in sequence. Factorial: Combination of multiple factors tested.",
          tags$span("?", style = "font-size: 14px;")
        )
      ),
      radioButtons(
        inputId = "trial_design",
        label = NULL,
        choices = c("Parallel", "Cross-over", "Factorial"),
        selected = "Parallel"
      ),
      
      tags$hr(),
      
      # Trial duration input
      div(class = "param-label-with-icon",
        tags$label("Trial Duration (weeks)", style = "margin-bottom: 0;"),
        div(class = "info-icon", `data-tooltip` = "Length of the trial in weeks. Typical range: 12-52 weeks.",
          tags$span("?", style = "font-size: 14px;")
        )
      ),
      numericInput(
        inputId = "trial_duration",
        label = NULL,
        value = 24,
        min = 1
      ),
      
      tags$hr(),
      
      # Number of studies input
      div(class = "param-label-with-icon",
        tags$label("Number of Studies", style = "margin-bottom: 0;"),
        div(class = "info-icon", `data-tooltip` = "Simulate multiple independent trials using the same parameters. Helps assess variability across studies.",
          tags$span("?", style = "font-size: 14px;")
        )
      ),
      numericInput(
        inputId = "n_trials",
        label = NULL,
        value = 1,
        min = 1
      ),
      
      tags$hr(),
      
      # ========== Output Settings Section ==========
      div(class = "section-header", "Output Settings"),
      
      # Time unit selection
      div(class = "param-label-with-icon",
        tags$label("Output Time Unit", style = "margin-bottom: 0;"),
        div(class = "info-icon", `data-tooltip` = "Unit for displaying simulation time on plots and tables.",
          tags$span("?", style = "font-size: 14px;")
        )
      ),
      selectInput(
        inputId = "time_unit",
        label = NULL,
        choices = c("Hours" = "hours", "Days" = "days", "Weeks" = "weeks", "Months" = "months"),
        selected = "days"
      ),

      # CI or PI selection
      div(class = "param-label-with-icon",
        tags$label("Interval Type", style = "margin-bottom: 0;"),
        div(class = "info-icon", `data-tooltip` = "CI: Confidence Interval (uncertainty around mean). PI: Prediction Interval (expected range for individuals).",
          tags$span("?", style = "font-size: 14px;")
        )
      ),
      selectInput(
        inputId = "interval_type",
        label = NULL,
        choices = c("Confidence Interval (CI)" = "ci", "Prediction Interval (PI)" = "pi"),
        selected = "pi"
      ),
  
      # Toggle for including variability
      div(class = "param-label-with-icon",
        tags$label("Include Variability", style = "margin-bottom: 0;"),
        div(class = "info-icon", `data-tooltip` = "Include inter-individual and intra-individual variability in simulations. Uncheck for deterministic predictions.",
          tags$span("?", style = "font-size: 14px;")
        )
      ),
      checkboxInput(
        inputId = "include_variability",
        label = "Yes",
        value = TRUE
      ),
      
      tags$hr(),

      # Conditional UI for dosing information based on trial design
      uiOutput("dosing_ui"),

      tags$hr(),
      
      # ========== NEW: Output mapping UI for multi-model ==========
      uiOutput("output_mapping_ui"),
      
      tags$hr(),

      # ========== Run Simulation Section ==========
      div(class = "section-header", "Execute Simulation"),
      
      # Run Simulation button
      actionButton(
        inputId = "run_sim",
        label = "Run Simulation",
        class = "btn btn-primary", 
        icon = icon("play"),
        style = "width: 100%;"
      ),
      
      tags$hr(),
      
      # Display model information
      tags$h4("Model Information", style = "margin-top: 0;"),
      textOutput("model_info"),
      width = 4
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Simulation",
          tags$h3("Simulation Results"),
          shinycssloaders::withSpinner(uiOutput("sim_result"))
        ),
        tabPanel(
          "Validation",
          tags$h3("Model Validation"),
          tags$p("In this section, the model simulations are validated against either published model simulations (internal validation), study data used for model building (internal validation), or study data not used for model building (external validation). Different visualization methods are provided to compare model predictions versus observed data. Select which type of validation you want to assess."),
          uiOutput("validation_ui"),
          tags$hr(),
          uiOutput("validation_selector"),
          shinycssloaders::withSpinner(uiOutput("validation_result"))
        ),
        tabPanel(
          "Help",
          tags$h3("Educational Clinical Trial Simulator (EduCTS) Guide"),
          
          tags$h4("What is Clinical Trial Simulation?"),
          tags$p("Educational Clinical Trial Simulator (EduCTS) uses mathematical models of drug pharmacokinetics (PK) and pharmacodynamics (PD) to predict how drugs behave in human populations. This helps researchers:"),
          tags$ul(
            tags$li("Optimize dose selection and dosing intervals"),
            tags$li("Predict treatment outcomes in different populations"),
            tags$li("Assess variability and uncertainty"),
            tags$li("Support study design decisions before running expensive clinical trials")
          ),
          
          tags$h4("Trial Configuration"),
          tags$strong("Type of Trial:"),
          tags$ul(
            tags$li(tags$strong("Parallel"), " - Different subjects receive different treatments, run simultaneously"),
            tags$li(tags$strong("Cross-over"), " - Same subjects receive multiple treatments in sequence with washout periods"),
            tags$li(tags$strong("Factorial"), " - Combination of multiple treatment factors tested simultaneously")
          ),
          
          tags$strong("Trial Duration:"),
          tags$p("Length of the simulated trial in weeks. Longer durations may be needed to see drug accumulation or long-term effects."),
          
          tags$strong("Number of Studies:"),
          tags$p("Run multiple independent simulations using the same parameters. More studies provide better assessment of variability and uncertainty."),
          
          tags$h4("Output Settings"),
          tags$strong("Output Time Unit:"),
          tags$p("Controls how time is displayed on results (hours, days, weeks, or months)."),
          
          tags$strong("Interval Type:"),
          tags$ul(
            tags$li(tags$strong("CI (Confidence Interval)"), " - Represents uncertainty around the mean/median prediction"),
            tags$li(tags$strong("PI (Prediction Interval)"), " - Represents expected range for individual subjects (wider than CI)")
          ),
          
          tags$strong("Include Variability:"),
          tags$p("When checked, simulations include both inter-individual variability (differences between subjects) and intra-individual variability (within-subject variation). Uncheck for deterministic mean predictions."),
          
          tags$h4("Interpreting Results"),
          tags$p("The Simulation tab shows:"),
          tags$ul(
            tags$li("Time course of drug concentration and/or biomarker response"),
            tags$li("Median predictions (solid line)"),
            tags$li("Confidence or Prediction Intervals (shaded regions)")
          ),
          
          tags$h4("Model Validation"),
          tags$p("Validation demonstrates how well the model captures real drug behavior by comparing simulations to actual clinical trial data. Good validation provides confidence in using the model for predictions."),
          
          tags$h4("Tips for Use"),
          tags$ul(
            tags$li("Start with default values and observe baseline predictions"),
            tags$li("Vary one parameter at a time to understand its impact"),
            tags$li("Run multiple studies to assess variability"),
            tags$li("Compare simulated and observed data in the Validation tab"),
            tags$li("Consult the Model Information panel for parameter details")
          )
        )
      ),
      width = 8
    )
  ),
  tags$hr(),
  div(style = "text-align: center; color: #999; font-size: 0.85em; padding: 15px; margin-top: 20px;",
    HTML("Creator: Jane Knöchel | Last Updated: 2026-02-17")
  )
)