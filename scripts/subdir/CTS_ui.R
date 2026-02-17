library(shiny)
library(mrgsolve)
library(shinychat)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("EduCTS - Educational Clinical Trial Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h3("Trial Design"),
      
      # Trial design selection
      radioButtons(
        inputId = "trial_design",
        label = "Type of Trial",
        choices = c("Parallel", "Cross-over", "Factorial"),
        selected = "Parallel"
      ),
      
      tags$hr(),
      
      # Trial duration input
      numericInput(
        inputId = "trial_duration",
        label = "Trial duration (weeks)",
        value = 24,
        min = 1
      ),
      
      tags$hr(),
      
      # Number of studies input
      numericInput(
        inputId = "n_trials",
        label = "Number of Studies",
        value = 1,
        min = 1
      ),
      
      tags$hr(),
      
      # Time unit selection
      selectInput(
        inputId = "time_unit",
        label = "Output Time Unit",
        choices = c("Hours" = "hours", "Days" = "days", "Weeks" = "weeks", "Months" = "months"),
        selected = "days"
      ),
      tags$hr(),

      # CI or PI selection
      selectInput(
        inputId = "interval_type",
        label = "Interval Type",
        choices = c("Confidence Interval (CI)" = "ci", "Prediction Interval (PI)" = "pi"),
        selected = "pi"
      ),
  
  tags$hr(),
  
  # Toggle for including variability
  checkboxInput(
    inputId = "include_variability",
    label = "Include Variability",
    value = TRUE
  ),
      tags$hr(),

      # Conditional UI for dosing information based on trial design
      uiOutput("dosing_ui"),

      tags$hr(),
      
      # ========== NEW: Output mapping UI for multi-model ==========
      uiOutput("output_mapping_ui"),
      
      tags$hr(),

      # Run Simulation button
      actionButton(
        inputId = "run_sim",
        label = "Run Simulation",
        class = "btn btn-primary", 
        icon = icon("play")
      ),
      tags$hr(),
      
      # Display model information
      tags$h4("Model Information"),
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
          tags$p("In this section, the model simulations are validated again either published model simulations (internal validation), study data which was used for model building (internal validation) or study data not emplyed for model building (external validation). Different ways of plotting the model predictions versus the original data are employed. You can select which type of validation you want to assess."),
          uiOutput("validation_ui"),
          tags$hr(),
          uiOutput("validation_selector"),
          shinycssloaders::withSpinner(uiOutput("validation_result"))
        )
      ),
      width = 8
    )
  )
)