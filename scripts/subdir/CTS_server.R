library(shiny)
library(mrgsolve)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(plotly)
library(readr)
library(digest)

models_dir <- "../models/" # Adjust path as needed

# Modify the main function
generate_input_dataset <- function(treatment_groups, study_length_weeks, design = "parallel", washout_weeks = 4, model_time_unit, input_compartment = "DEPOT", dose_unit = "mg", model_dose_unit = "mg") {
    # Validate input columns and design parameter
    required_columns <- c("GroupName", "SampleSize", "Treatment", "Dose", "Frequency")
    if (!all(required_columns %in% colnames(treatment_groups))) {
        stop("Dataset must contain: ", paste(required_columns, collapse = ", "))
    }
    if (!design %in% c("parallel", "cross-over", "factorial")) {
        stop("Design must be either 'parallel', 'cross-over', or 'factorial'")
    }
    print(treatment_groups)
    # Calculate study length in hours once
    study_length_model_unit <- convert_time(study_length_weeks * 7 * 24, "hours", model_time_unit, model_time_unit)
    washout_length_model_unit <- convert_time(washout_weeks * 7 * 24, "hours", model_time_unit, model_time_unit)
    print(input_compartment)
    if (design == "parallel") {
        # Original parallel design code
      # For each group, subject, and compartment, create a dosing record with the correct dose
      input_data <- treatment_groups %>%
  group_by(GroupName, Compartment) %>%
  mutate(ID = list(1:SampleSize)) %>%
  unnest(ID) %>%
  ungroup() %>%
  mutate(
    cmt = Compartment,
    time = 0,
    amt = ifelse(is.na(Dose) | Treatment %in% c("placebo", "Placebo"), 0, 
                 convert_dose(Dose, dose_unit, model_dose_unit)),
    rate = 0,
    evid = 1,
    ss = 0,
    Period = 1,
    ii = case_when(
      Frequency == "Twice daily" ~ convert_time(12, "hours", model_time_unit, model_time_unit),
      Frequency == "Daily" ~ convert_time(24, "hours", model_time_unit, model_time_unit),
      Frequency == "Weekly" ~ convert_time(168, "hours", model_time_unit, model_time_unit),
      Frequency == "Biweekly" ~ convert_time(336, "hours", model_time_unit, model_time_unit),
      Frequency == "Every 4 weeks" ~ convert_time(672, "hours", model_time_unit, model_time_unit),
      Frequency == "Monthly" ~ convert_time(720, "hours", model_time_unit, model_time_unit),
      Frequency == "Once every 3 months" ~ convert_time(2160, "hours", model_time_unit, model_time_unit),
      Frequency == "Once every 4 months" ~ convert_time(2880, "hours", model_time_unit, model_time_unit),
      Frequency == "Every 8 weeks" ~ convert_time(1344, "hours", model_time_unit, model_time_unit),
      Frequency == "single_dose" ~ study_length_model_unit,  # Single dose: ii is not used (addl = 0)
      TRUE ~ NA_real_
    ),
    # Conditional addl: use provided value if it exists, otherwise calculate from study length
    addl = if ("Addl" %in% colnames(treatment_groups)) {
      !!rlang::sym("Addl")  # Use provided Addl column
    } else {
      case_when(
        Frequency == "single_dose" ~ 0,  # Single dose has no additional doses
        TRUE ~ pmax(floor(study_length_model_unit / ii) - 1, 0)  # Ensure addl >= 0
      )
    },
    SEQ = 1
  )%>% 
  arrange(GroupName, ID, cmt,time)
        # input_data <- treatment_groups %>%
        #   group_by(GroupName) %>%
        #   mutate(ID = list(1:SampleSize)) %>% 
        #   unnest(ID) %>% 
        #   ungroup() %>% 
        #   mutate(ID=row_number()) %>%
        #   # Create two events per ID - one for each drug
        #   slice(rep(1:n(), each = length(input_compartment))) %>%
        #   group_by(ID) %>%
        #   mutate(
        #     time = 0,
        #     amt = ifelse(is.na(Dose) | Treatment %in% c("placebo", "Placebo"), 0, Dose),
        #     cmt = input_compartment,
        #     rate = 0,
        #     evid = 1,
        #     ss = 0,
        #     Period = 1,
        #     ii = case_when(
        #       Frequency == "Twice daily" ~ convert_time(12, "hours", model_time_unit, model_time_unit),
        #       Frequency == "Daily" ~ convert_time(24, "hours", model_time_unit, model_time_unit),
        #       Frequency == "Weekly" ~ convert_time(168, "hours", model_time_unit, model_time_unit),
        #       Frequency == "Biweekly" ~ convert_time(336, "hours", model_time_unit, model_time_unit),
        #       Frequency == "Every 4 weeks" ~ convert_time(672, "hours", model_time_unit, model_time_unit),
        #       Frequency == "Monthly" ~ convert_time(720, "hours", model_time_unit, model_time_unit),
        #       TRUE ~ NA_real_
        #     ),
        #     addl = floor(study_length_model_unit / ii) - 1,
        #     SEQ = 1
        #   )
    } else if (design == "cross-over") {
        # Crossover design code
        washout_length_model_unit <-  convert_time(washout_weeks * 7 * 24, "hours", model_time_unit, model_time_unit)
        period_length_model_unit <- convert_time(study_length_weeks * 7 * 24, "hours", model_time_unit, model_time_unit)

        # Get unique treatments
        treatments <- unique(treatment_groups$Treatment)
    
        #  Generate all possible sequences
        sequences <- data.frame(GroupName=generate_sequences(treatments)) %>% 
          mutate(SEQ=row_number(),
                 Sequence=GroupName) %>% 
          separate_rows(GroupName, sep="->") %>% 
          group_by(SEQ) %>% 
          mutate(Period = row_number())
        
        groups <- right_join(rename(select(treatment_groups,c(SampleSize,Treatment,Dose,Frequency)),GroupName=Treatment),sequences)

        input_data <- groups %>%
            group_by(SEQ) %>%
            mutate(ID = list(1:SampleSize)) %>%
            unnest(cols=c(ID)) %>%
            group_by(ID,SEQ) %>%
            mutate(
                time = (Period - 1) * (period_length_model_unit + washout_length_model_unit),
                ii = case_when(
                    Frequency == "Twice daily" ~ convert_time(12, "hours", model_time_unit, model_time_unit),
                    Frequency == "Daily" ~ convert_time(24, "hours", model_time_unit, model_time_unit),
                    Frequency == "Weekly" ~ convert_time(168, "hours", model_time_unit, model_time_unit),
                    Frequency == "Biweekly" ~ convert_time(336, "hours", model_time_unit, model_time_unit),
                    Frequency == "Every 4 weeks" ~ convert_time(672, "hours", model_time_unit, model_time_unit),
                    Frequency == "Monthly" ~ convert_time(720, "hours", model_time_unit, model_time_unit),
                    Frequency == "Once every 3 months" ~ convert_time(2160, "hours", model_time_unit, model_time_unit),
                    Frequency == "Once every 4 months" ~ convert_time(2880, "hours", model_time_unit, model_time_unit),
                    Frequency == "Every 8 weeks" ~ convert_time(1344, "hours", model_time_unit, model_time_unit),
                    TRUE ~ NA_real_
                ),
                # Conditional addl: use provided value if it exists, otherwise calculate from study length
                addl = if ("Addl" %in% colnames(treatment_groups)) {
                  !!rlang::sym("Addl")  # Use provided Addl column
                } else {
                  pmax(floor(period_length_model_unit / ii) - 1, 0)  # Ensure addl >= 0
                }) %>% 
          group_by(GroupName) %>% 
          mutate(
              amt = ifelse(Sequence == "Placebo", 0, convert_dose(Dose, dose_unit, model_dose_unit)),
                rate = 0,
                cmt = input_compartment,
                evid = 1,
                ss = 0
            )
    } else if (design == "factorial") {
        # Get unique treatments excluding placebo
        treatments <- unique(treatment_groups$Treatment[!treatment_groups$Treatment %in% c("Placebo", "placebo")])
        
        # Generate factorial combinations
        combinations <- generate_factorial_combinations(treatments)
        
        # Create group names and add group sequence number
        group_names <- apply(combinations, 1, function(row) {
            active_treatments <- treatments[as.logical(row)]
            if(length(active_treatments) == 0) {
                return("Placebo")
            } else {
                return(paste(active_treatments, collapse = "+"))
            }
        })
        
        combinations$GroupName <- group_names
        combinations$GroupSeq <- seq_len(nrow(combinations))

        # Create dosing records for each group
        input_data <- map_dfr(seq_len(nrow(combinations)), function(group_seq) {
            group <- combinations$GroupName[group_seq]
            n_subjects <- treatment_groups$SampleSize[1]
            base_id <- (group_seq - 1) * n_subjects
            
            if (group == "Placebo") {
                tibble(
                    GroupName = group,
                    ID = base_id + 1:n_subjects,
                    SampleSize = n_subjects,
                    cmt = input_compartment,
                    amt = 0
                )
            } else {
                # Get treatments in this combination
                group_treatments <- unlist(strsplit(group, "\\+"))
                
                # Create records for each treatment with fixed compartments
                map_dfr(group_treatments, ~tibble(
                    GroupName = group,
                    ID = base_id + 1:n_subjects,
                    SampleSize = n_subjects,
                    cmt = if(.x == treatments[1]) 1 else 2,
                    amt = convert_dose(treatment_groups$Dose[treatment_groups$Treatment == .x], dose_unit, model_dose_unit)
                ))
            }
        }) %>%
    group_by(GroupName, ID) %>%
    mutate(
        time = 0,
        ii = case_when(
            GroupName %in% treatment_groups$Treatment ~ 
                treatment_groups$Frequency[match(GroupName, treatment_groups$Treatment)] %>%
                recode(
                    "Twice daily" = convert_time(12, "hours", model_time_unit, model_time_unit),
                    "Daily" = convert_time(24, "hours", model_time_unit, model_time_unit),
                    "Weekly" = convert_time(168, "hours", model_time_unit, model_time_unit),
                    "Biweekly" = convert_time(336, "hours", model_time_unit, model_time_unit),
                    "Every 4 weeks" = convert_time(672, "hours", model_time_unit, model_time_unit),
                    "Monthly" = convert_time(720, "hours", model_time_unit, model_time_unit),
                    "Once every 3 months" = convert_time(2160, "hours", model_time_unit, model_time_unit),
                    "Once every 4 months" = convert_time(2880, "hours", model_time_unit, model_time_unit),
                    "Every 8 weeks" = convert_time(1344, "hours", model_time_unit, model_time_unit)
                ),
            TRUE ~ first(treatment_groups$Frequency) %>%
                recode(
                    "Twice daily" = convert_time(12, "hours", model_time_unit, model_time_unit),
                    "Daily" = convert_time(24, "hours", model_time_unit, model_time_unit),
                    "Weekly" = convert_time(168, "hours", model_time_unit, model_time_unit),
                    "Biweekly" = convert_time(336, "hours", model_time_unit, model_time_unit),
                    "Every 4 weeks" = convert_time(672, "hours", model_time_unit, model_time_unit),
                    "Monthly" = convert_time(720, "hours", model_time_unit, model_time_unit),
                    "Once every 3 months" = convert_time(2160, "hours", model_time_unit, model_time_unit),
                    "Once every 4 months" = convert_time(2880, "hours", model_time_unit, model_time_unit),
                    "Every 8 weeks" = convert_time(1344, "hours", model_time_unit, model_time_unit)
                )
        ),
        addl = if ("Addl" %in% colnames(treatment_groups)) {
          !!rlang::sym("Addl")  # Use provided Addl column
        } else {
          pmax(floor(study_length_model_unit / ii) - 1, 0)  # Ensure addl >= 0
        },
        rate = 0,
        evid = 1,
        ss = 0,
        SEQ = 1,
        Period = 1
    ) %>%
    ungroup()
    }
    # if (!design == "factorial"){
    #   input_data <- input_data %>%
    #     ungroup() %>%
    #     # First arrange by SEQ to ensure proper ordering
    #     arrange(SEQ) %>%
    #     # Create sequential IDs across sequences
    #     group_by(SEQ,Period) %>%
    #     mutate(
    #       # Calculate base ID for each sequence
    #       base_ID = (SEQ - 1) * first(SampleSize),
    #       # Add sequential ID within sequence
    #       ID = base_ID + row_number()
    #     ) %>%
    #     # Select final columns
    #     select(ID, time, amt, rate, cmt, evid, ii, addl, ss, SEQ, Period,GroupName)
    # }
    print(input_data)
    return(input_data)
}

convert_time <- function(time, from_unit, to_unit, model_time_unit = "hours") {
  # Define conversion factors (all relative to 1 hour)
  base_conversion <- list(
    "minutes" = 60,               # 60 minutes = 1 hour
    "hours"   = 1,
    "days"    = 1 / 24,
    "weeks"   = 1 / (24 * 7),
    "months"  = 1 / (24 * 30),
    "years"   = 1 / (24 * 365)
  )
  
  # Validate inputs
  for (u in c(from_unit, to_unit, model_time_unit)) {
    if (!u %in% names(base_conversion)) {
      stop("Unsupported time unit: ", u, 
           ". Must be one of: ", paste(names(base_conversion), collapse = ", "))
    }
  }
  
  # Step 1: Convert `time` from `from_unit` → model time unit
  time_in_model_unit <- time * (base_conversion[[model_time_unit]] / base_conversion[[from_unit]])
  
  # Step 2: Convert from model time unit → `to_unit`
  time_in_target_unit <- time_in_model_unit * (base_conversion[[to_unit]] / base_conversion[[model_time_unit]])
  
  return(time_in_target_unit)
}

# Function to convert dose units
convert_dose <- function(dose, from_unit, to_unit) {
  # Handle case where units are the same or missing
  if (is.na(from_unit) || is.na(to_unit) || from_unit == to_unit) {
    cat("DEBUG [convert_dose]: No conversion needed - from_unit:", from_unit, 
        ", to_unit:", to_unit, "\n")
    return(dose)
  }
  
  # Normalize units to lowercase
  from_unit <- tolower(trimws(from_unit))
  to_unit <- tolower(trimws(to_unit))
  
  # Define conversion factors (relative to grams)
  # g → gram, mg → milligram, ug → microgram, ng → nanogram
  base_conversion <- list(
    "g" = 1,
    "gram" = 1,
    "grams" = 1,
    "mg" = 1e-3,
    "milligram" = 1e-3,
    "milligrams" = 1e-3,
    "ug" = 1e-6,
    "mcg" = 1e-6,
    "microgram" = 1e-6,
    "micrograms" = 1e-6,
    "ng" = 1e-9,
    "nanogram" = 1e-9,
    "nanograms" = 1e-9,
    "kg" = 1e3,
    "kilogram" = 1e3,
    "kilograms" = 1e3
  )
  
  # Validate inputs
  if (!from_unit %in% names(base_conversion)) {
    stop("Unsupported dose unit: ", from_unit, 
         ". Must be one of: ", paste(names(base_conversion), collapse = ", "))
  }
  if (!to_unit %in% names(base_conversion)) {
    stop("Unsupported dose unit: ", to_unit, 
         ". Must be one of: ", paste(names(base_conversion), collapse = ", "))
  }
  
  # Convert: dose in 'from_unit' → grams → dose in 'to_unit'
  dose_in_grams <- dose * base_conversion[[from_unit]]
  dose_in_target_unit <- dose_in_grams / base_conversion[[to_unit]]
  
  cat("DEBUG [convert_dose]: Converted dose =", dose, from_unit, "to", 
      round(dose_in_target_unit, 6), to_unit, "\n")
  
  return(dose_in_target_unit)
}

# Function to transform simulation data to match observed data format
transform_simulation_data <- function(summaries, output_var, transformation_type) {
  # transformation_type should be one of:
  # - "absolute": no transformation
  # - "change": absolute change from baseline
  # - "percent_change": percent change from baseline
  # - "change from baseline in percent": percent change from baseline (string format)
  
  if (is.null(transformation_type) || transformation_type == "" || 
      tolower(transformation_type) == "absolute" || 
      tolower(transformation_type) == "concentration") {
    # No transformation needed
    return(summaries)
  }
  
  # Normalize transformation type
  trans_lower <- tolower(transformation_type)
  is_percent_change_target <- grepl("percent|%", trans_lower) && grepl("change|baseline", trans_lower)
  is_change_target <- (grepl("change", trans_lower) && !is_percent_change_target) || trans_lower == "change"
  
  if (!is_percent_change_target && !is_change_target) {
    # Unknown transformation, return as is
    return(summaries)
  }
  
  # Detect variables that are already percent change
  is_pct_variable <- function(x) {
    grepl("percent|pct|%|_pc|_pct", x, ignore.case = TRUE)
  }
  
  pct_vars <- unique(summaries$label[is_pct_variable(summaries$label)])
  
  summaries_pct <- summaries %>% filter(label %in% pct_vars)
  summaries_non_pct <- summaries %>% filter(!label %in% pct_vars)
  
  # If nothing to transform, return original
  if (nrow(summaries_non_pct) == 0) {
    return(summaries)
  }
  
  # ---- Transform only NON percent-change variables ----
  transformed_non_pct <- summaries_non_pct %>%
    group_by(variable, label) %>%
    arrange(time) %>%
    mutate(
      baseline_median = first(median),
      baseline_lower = first(lower),
      baseline_upper = first(upper)
    ) %>%
    ungroup()
  
  if (is_percent_change_target) {
    
    transformed_non_pct <- transformed_non_pct %>%
      mutate(
        median = ((median - baseline_median) / baseline_median) * 100,
        lower  = ((lower - baseline_lower) / baseline_lower) * 100,
        upper  = ((upper - baseline_upper) / baseline_upper) * 100
      )
    
  } else if (is_change_target) {
    
    transformed_non_pct <- transformed_non_pct %>%
      mutate(
        median = median - baseline_median,
        lower  = lower - baseline_lower,
        upper  = upper - baseline_upper
      )
  }
  
  transformed_non_pct <- transformed_non_pct %>%
      select(-baseline_median, -baseline_lower, -baseline_upper)
  
  # ---- Recombine ----
  final <- bind_rows(transformed_non_pct, summaries_pct)
  
  return(final)
}

server <- function(input, output, session) {
  # Load global.R to get selected model(s)
  source("subdir/global.R", local = TRUE)
  
  # Determine if single or multiple models are selected
  is_multi_model <- exists("selected_model_filenames")
  
  if (is_multi_model) {
    # Multiple models selected
    model_filenames <- selected_model_filenames
    n_models <- length(model_filenames)
  } else {
    # Single model selected
    model_filenames <- c(selected_model_filename)
    n_models <- 1
  }
  
  # Load metadata for ALL models
  models_metadata <- lapply(model_filenames, function(fname) {
    json_path <- file.path(models_dir, sub("\\.cpp$", ".json", fname))
    cat("DEBUG: Processing model file:", fname, "\n")
    cat("DEBUG: Looking for JSON at:", json_path, "\n")
    cat("DEBUG: JSON file exists:", file.exists(json_path), "\n")
    full_json <- NULL  # Store full JSON for validation data
    
    meta <- list(
      filename = fname,
      model_time_unit = "hours",
      input_compartment = "CENTRAL",
      input_labels = "Drug",
      dose_unit = "mg",
      model_dose_unit = "mg",  # Unit expected by the model
      output_var = "DV",
      output_label = "Concentration",
      therapeutic_dose = NA,
      therapeutic_frequency = NA,
      internal_validation_data = NULL,
      external_validation_data = NULL,
      validation_data = NULL  # For backward compatibility
    )
    
    if (file.exists(json_path)) {
      cat("DEBUG: JSON file found! Loading...\n")
      full_json <- fromJSON(json_path, simplifyDataFrame = FALSE)
      cat("DEBUG: JSON loaded successfully. Root keys:", paste(names(full_json), collapse = ", "), "\n")
      cat("DEBUG: Has internal_validation_data at root:", "internal_validation_data" %in% names(full_json), "\n")
      cat("DEBUG: Has external_validation_data at root:", "external_validation_data" %in% names(full_json), "\n")
      json_meta <- full_json
      
      # Access model_information if it exists, otherwise use root level
      if (!is.null(json_meta$model_information)) {
        json_meta <- json_meta$model_information
      }
      
      if (!is.null(json_meta$model_time_unit)) {
        meta$model_time_unit <- json_meta$model_time_unit
      }
      
      if (!is.null(json_meta$input)) {
        meta$input_compartment <- strsplit(json_meta$input, ",")[[1]] %>% trimws()
      }
      
      if (length(meta$input_compartment) == 1) {
        meta$input_labels <- json_meta$compound
      } else if (!is.null(json_meta$input_label)) {
        meta$input_labels <- strsplit(json_meta$input_label, ",")[[1]] %>% trimws()
      } else {
        meta$input_labels <- meta$input_compartment
      }
      
      if (!is.null(json_meta$dose_unit)) {
        meta$dose_unit <- strsplit(json_meta$dose_unit, ",")[[1]] %>% trimws()
      }
      
      if (!is.null(json_meta$model_dose_unit)) {
        meta$model_dose_unit <- strsplit(json_meta$model_dose_unit, ",")[[1]] %>% trimws()
      }
      
      if (!is.null(json_meta$output)) {
        meta$output_var <- strsplit(json_meta$output, ",\\s*")[[1]]
      }
      
      if (!is.null(json_meta$output_label)) {
        meta$output_label <- strsplit(json_meta$output_label, ",\\s*")[[1]]
      }
      
      if (!is.null(json_meta$therapeutic_dose)) {
        dose_val <- json_meta$therapeutic_dose
        if (!is.na(dose_val) && dose_val != "NA" && dose_val != "N/A") {
          meta$therapeutic_dose <- as.numeric(dose_val)
        }
      }
      
      if (!is.null(json_meta$therapeutic_frequency)) {
        freq_val <- json_meta$therapeutic_frequency
        if (!is.na(freq_val) && freq_val != "NA" && freq_val != "N/A") {
          meta$therapeutic_frequency <- freq_val
        }
      }
      
      # Load internal and external validation data if available
      if (!is.null(full_json$internal_validation_data)) {
        cat("DEBUG: ✓ FOUND internal_validation_data in JSON! Studies:", length(full_json$internal_validation_data$studies), "\n")
        meta$internal_validation_data <- full_json$internal_validation_data
        # For backward compatibility, use internal as default validation_data
        meta$validation_data <- full_json$internal_validation_data
      } else {
        cat("DEBUG: ✗ NO internal_validation_data found in JSON\n")
      }
      
      if (!is.null(full_json$external_validation_data)) {
        cat("DEBUG: ✓ FOUND external_validation_data in JSON!\n")
        meta$external_validation_data <- full_json$external_validation_data
      } else {
        cat("DEBUG: ✗ NO external_validation_data found in JSON\n")
      }
    } else {
      cat("DEBUG: JSON file NOT FOUND at", json_path, "\n")
    }
    
    meta
  })

# Debug output after loading all models' metadata
cat("DEBUG: ===== MODELS METADATA LOADED =====\n")
for (i in seq_len(n_models)) {
  meta <- models_metadata[[i]]
  model_name <- tools::file_path_sans_ext(basename(meta$filename))
  cat("DEBUG: Model", i, ":", model_name, "\n")
  cat("DEBUG:   - File:", meta$filename, "\n")
  cat("DEBUG:   - Has internal_validation_data:", !is.null(meta$internal_validation_data), "\n")
  cat("DEBUG:   - Has external_validation_data:", !is.null(meta$external_validation_data), "\n")
  if (!is.null(meta$internal_validation_data) && !is.null(meta$internal_validation_data$studies)) {
    cat("DEBUG:   - Internal validation studies:", length(meta$internal_validation_data$studies), "\n")
  }
}
cat("DEBUG: ===== END METADATA =====\n")

# ========== ADD THESE LINES ==========
  # Extract commonly used variables from first model's metadata
  model_time_unit <- models_metadata[[1]]$model_time_unit
  input_compartment <- models_metadata[[1]]$input_compartment
  input_labels <- models_metadata[[1]]$input_labels
  dose_unit <- models_metadata[[1]]$dose_unit

# Define a conversion factor for time units
time_conversion <- switch(
  model_time_unit,
  "mins" = 1 / 60,      # 1 minute = 1/60 hours
  "hours" = 1,          # Default: no conversion
  "days" = 24,          # 1 day = 24 hours
  "weeks" = 24 * 7,     # 1 week = 168 hours
  "months" = 24 * 30,   # 1 month = ~720 hours
  stop("Unsupported model_time_unit: ", model_time_unit)
)
  # Reactive values to store simulation results
  sim_results <- reactiveVal(NULL)
  sim_metadata <- reactiveVal(NULL)
  
  # Reactive value to store the number of treatment groups
  treatment_groups <- reactiveValues()
  
  # ========== INITIALIZE treatment group counts for EACH model ==========
  for (i in seq_len(n_models)) {
    treatment_groups[[paste0("model_", i, "_count")]] <- 1
  }
  # ======================================================================

  model_path <- file.path(models_dir, model_filenames[1])
  
  # Display model information
  output$model_info <- renderText({
    paste("Simulating model:", model_filenames[1])
  })

  # Generate dosing UI based on trial design
  output$dosing_ui <- renderUI({
    if (input$trial_design == "Parallel") {
      # Parallel design: Single group
      tagList(
        lapply(seq_len(n_models), function(model_idx) {
        meta <- models_metadata[[model_idx]]
        model_name <- tools::file_path_sans_ext(basename(meta$filename))
        
        tagList(
          tags$h4(paste("Dosing for Model:", model_name)),
          
          # Treatment groups for this model
          lapply(1:treatment_groups[[paste0("model_", model_idx, "_count")]], function(i) {
            tagList(
              tags$h5(paste("Treatment Group", i)),
              textInput(paste0("m", model_idx, "_group_name_", i), "Group Name", 
                       value = paste("Group", i)),
              numericInput(paste0("m", model_idx, "_sample_size_", i), "Sample Size", 
                          value = 100, min = 1),
              textInput(paste0("m", model_idx, "_treatment_", i), "Treatment", 
                       value = paste("Drug", i)),
              
              # Dose inputs for each compartment
              lapply(seq_along(meta$input_compartment), function(j) {
                # Use therapeutic dose as default, fallback to 10
                default_dose <- if (!is.na(meta$therapeutic_dose)) {
                  meta$therapeutic_dose
                } else {
                  10
                }
                
                # Use therapeutic frequency as default, fallback to "Weekly"
                default_freq <- if (!is.na(meta$therapeutic_frequency)) {
                  meta$therapeutic_frequency
                } else {
                  "Weekly"
                }
                
                fluidRow(
                  column(6,
                    numericInput(
                      inputId = paste0("m", model_idx, "_dose_", i, "_", j),
                      label = paste("Dose for", meta$input_labels[j], "(", meta$dose_unit[j], ")"),
                      value = default_dose,
                      min = 0
                    )
                  ),
                  column(6,
                    selectInput(
                      inputId = paste0("m", model_idx, "_frequency_", i, "_", j),
                      label = paste("Frequency for", meta$input_labels[j]),
                      choices = c("Twice daily", "Daily", "Weekly", "Biweekly","Every 4 weeks", "Monthly", "Once every 3 months", "Once every 4 months","Every 8 weeks"),
                      selected = default_freq
                    )
                  )
                )
              })
            )
          }),
          
          # Add/Remove buttons for this model
          fluidRow(
            column(6,
              actionButton(paste0("add_group_m", model_idx), "Add Group", 
                          class = "btn btn-success btn-sm")
            ),
            column(6,
              actionButton(paste0("remove_group_m", model_idx), "Remove Group", 
                          class = "btn btn-danger btn-sm")
            )
          ),
          tags$hr()
        )
      })
    )
      # # Generate UI for each treatment group
      # lapply(1:treatment_groups$count, function(i) {
      #   tagList(
      #     tags$h5(paste("Treatment Group", i)),
      #     textInput(paste0("group_name_", i), "Group Name", value = paste("Treatment Group", i)),
      #     numericInput(paste0("sample_size_", i), "Sample Size", value = 100, min = 1),
      #     textInput(paste0("treatment_", i), "Treatment", value = paste("Drug", i)),
      #     # For each compartment, add a dose and frequency input
      #     lapply(seq_along(input_compartment), function(j) {
      #       fluidRow(
      #         column(6,
      #           numericInput(
      #             inputId = paste0("dose_", i, "_", j),
      #             label = paste("Dose for", input_labels[j], "(", dose_unit[j], ")"),
      #             value = 10,
      #             min = 0
      #           )
      #         ),
      #         column(6,
      #           selectInput(
      #             inputId = paste0("frequency_", i, "_", j),
      #             label = paste("Frequency for", input_labels[j]),
      #             choices = c("Twice daily", "Daily", "Weekly", "Biweekly", "Monthly"),
      #             selected = "Weekly"
      #           )
      #         )
      #       )
      #     })
      #   )
      # }),
      # # Buttons to add or remove treatment groups
      # actionButton("add_group", "Add Treatment Group", class = "btn btn-success"),
      # actionButton("remove_group", "Remove Treatment Group", class = "btn btn-danger")
      # )
    } else if (input$trial_design == "Cross-over") {
      # Cross-over design: Multiple periods
      tagList(
        tags$h4("Dosing Information"),
        numericInput("num_periods", "Number of Periods", value = 2, min = 1),
        numericInput("sample_size", "Sample Size", value = 100, min = 1),
        textInput("treatment", "Treatment", value = "Drug X"),
        numericInput("dose", "Dose (mg)", value = 10, min = 0),
        numericInput("washout_weeks", "Washout Period (weeks)", value = 4, min = 0),
        selectInput("frequency", "Frequency", choices = c("single_dose", "Twice daily", "Daily", "Weekly", "Biweekly", "Every 4 weeks", "Monthly", "Once every 3 months", "Once every 4 months", "Every 8 weeks"), selected = "Weekly")
      )
    } else if (input$trial_design == "Factorial") {
      # Factorial design: Multiple treatments
      tagList(
        tags$h4("Dosing Information"),
        numericInput("num_treatments", "Number of Treatments", value = 2, min = 1),
        numericInput("sample_size", "Sample Size", value = 100, min = 1),
        textInput("treatment_1", "Treatment 1", value = "Drug A"),
        numericInput("dose_1", "Dose for Treatment 1 (mg)", value = 10, min = 0),
        textInput("treatment_2", "Treatment 2", value = "Drug B"),
        numericInput("dose_2", "Dose for Treatment 2 (mg)", value = 10, min = 0),
        selectInput("frequency", "Frequency", choices = c("single_dose", "Twice daily", "Daily", "Weekly", "Biweekly", "Every 4 weeks", "Monthly", "Once every 3 months", "Once every 4 months", "Every 8 weeks"), selected = "Weekly")
      )
    }
  })
  
  output$model_info <- renderText({
    if (n_models == 1) {
      paste("Simulating model:", model_filenames[1])
    } else {
      paste("Simulating", n_models, "models:",
            paste(model_filenames, collapse = ", "))
    }
  })
# Dynamically create observers for add/remove buttons for each model
observe({
  lapply(seq_len(n_models), function(model_idx) {
    # Add group
    observeEvent(input[[paste0("add_group_m", model_idx)]], {
      current_count <- treatment_groups[[paste0("model_", model_idx, "_count")]]
      treatment_groups[[paste0("model_", model_idx, "_count")]] <- current_count + 1
    })
    
    # Remove group
    observeEvent(input[[paste0("remove_group_m", model_idx)]], {
      current_count <- treatment_groups[[paste0("model_", model_idx, "_count")]]
      if (current_count > 1) {
        treatment_groups[[paste0("model_", model_idx, "_count")]] <- current_count - 1
      }
    })
  })
})

# Reactive values for output mapping
output_mappings <- reactiveValues(count = 0, mappings = list())

# Generate output mapping UI
output$output_mapping_ui <- renderUI({
  if (n_models <= 1) {
    # Single model: just transformation options
    tagList(
      tags$h4("Output Display Options"),
      tags$p("Select how to display the simulation outputs:"),
      selectInput(
        inputId = "single_output_transform",
        label = "Display Type",
        choices = c(
          "Absolute Value" = "absolute",
          "Change from Baseline" = "change",
          "Percent Change from Baseline" = "percent_change"
        ),
        selected = "absolute"
      )
    )
  } else {
    # Multi-model: mapping and transformation options
    
    # Get all outputs from all models
    all_outputs <- lapply(seq_len(n_models), function(i) {
      meta <- models_metadata[[i]]
      model_name <- tools::file_path_sans_ext(basename(meta$filename))
      tibble(
        model_idx = i,
        model_name = model_name,
        output_var = meta$output_var,
        output_label = meta$output_label
      )
    }) %>% bind_rows()
    
    # Group by output_label to find common outputs
    output_groups <- all_outputs %>%
      group_by(output_label) %>%
      summarise(
        models = list(model_name),
        n_models = n(),
        .groups = "drop"
      )
    
    tagList(
      tags$h4("Output Mapping & Display Options"),
      tags$p("Map outputs from different models and choose how to display them."),
      
      # Automatic groupings based on matching labels
      tags$h5("Common Outputs Across Models"),
      lapply(seq_len(nrow(output_groups)), function(i) {
        label <- output_groups$output_label[i]
        models_with_output <- output_groups$models[[i]]
        n_models_with_output <- output_groups$n_models[i]
        
        if (n_models_with_output > 1) {
          # Multiple models have this output - offer mapping
          tagList(
            div(
              style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px; background: #f9f9f9;",
              tags$h6(paste("Output:", label)),
              tags$p(paste("Found in models:", paste(models_with_output, collapse = ", "))),
              fluidRow(
                column(6,
                  checkboxInput(
                    inputId = paste0("combine_output_", i),
                    label = "Combine in single plot",
                    value = TRUE
                  )
                ),
                column(6,
                  selectInput(
                    inputId = paste0("transform_output_", i),
                    label = "Display as:",
                    choices = c(
                      "Absolute Value" = "absolute",
                      "Change from Baseline" = "change",
                      "Percent Change from Baseline" = "percent_change"
                    ),
                    selected = "absolute"
                  )
                )
              ),
              conditionalPanel(
                condition = paste0("input.combine_output_", i, " == true"),
                textInput(
                  inputId = paste0("combined_name_", i),
                  label = "Combined plot title:",
                  value = label
                )
              )
            )
          )
        } else {
          # Only one model has this output
          div(
            style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
            tags$h6(paste("Output:", label, "(", models_with_output[1], ")")),
            selectInput(
              inputId = paste0("transform_output_", i),
              label = "Display as:",
              choices = c(
                "Absolute Value" = "absolute",
                "Change from Baseline" = "change",
                "Percent Change from Baseline" = "percent_change"
              ),
              selected = "absolute"
            )
          )
        }
      }),
      
      tags$hr(),
      
      # Custom output mappings
      tags$h5("Custom Output Mappings"),
      tags$p("Create custom combinations of outputs from different models."),
      actionButton("add_custom_mapping", "Add Custom Mapping", class = "btn btn-info btn-sm"),
      uiOutput("custom_mappings_ui")
    )
  }
})

# Reactive values for output mapping
output_mappings <- reactiveValues(
  count = 0, 
  mappings = list(),
  next_id = 1  # Track next available ID
)

# Generate output mapping UI
output$output_mapping_ui <- renderUI({
  if (n_models <= 1) {
    # Single model: individual output selection with transformation
    meta <- models_metadata[[1]]
    
    tagList(
      tags$h4("Output Display Options"),
      tags$p("Select outputs to display and how to transform them:"),
      
      lapply(seq_along(meta$output_var), function(i) {
        div(
          style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
          fluidRow(
            column(6,
              tags$strong(paste("Output:", meta$output_label[i]))
            ),
            column(6,
              selectInput(
                inputId = paste0("single_transform_", i),
                label = "Display as:",
                choices = c(
                  "Absolute Value" = "absolute",
                  "Change from Baseline" = "change",
                  "% Change from Baseline" = "percent_change"
                ),
                selected = "absolute"
              )
            )
          )
        )
      })
    )
  } else {
    # Multi-model: individual output selection with transformation and mapping
    
    tagList(
      tags$h4("Output Display Options"),
      tags$p("Select outputs from each model and choose how to display them."),
      
      # Toggle to show/hide individual model outputs
      checkboxInput(
        inputId = "show_individual_outputs",
        label = "Show individual model output options",
        value = FALSE
      ),
      
      # Individual model outputs (hidden by default)
      conditionalPanel(
        condition = "input.show_individual_outputs == true",
        tags$h5("Individual Model Outputs"),
        lapply(seq_len(n_models), function(model_idx) {
          meta <- models_metadata[[model_idx]]
          model_name <- tools::file_path_sans_ext(basename(meta$filename))
          
          div(
            style = "border: 1px solid #337ab7; padding: 15px; margin-bottom: 15px; border-radius: 8px; background: #f0f8ff;",
            tags$h6(paste("Model:", model_name)),
            
            lapply(seq_along(meta$output_var), function(output_idx) {
              div(
                style = "margin-bottom: 10px; padding: 8px; background: white; border-radius: 4px;",
                fluidRow(
                  column(6,
                    tags$strong(meta$output_label[output_idx])
                  ),
                  column(6,
                    selectInput(
                      inputId = paste0("m", model_idx, "_transform_", output_idx),
                      label = "Display as:",
                      choices = c(
                        "Absolute Value" = "absolute",
                        "Change from Baseline" = "change",
                        "% Change from Baseline" = "percent_change"
                      ),
                      selected = "absolute"
                    )
                  )
                )
              )
            })
          )
        })
      ),
      
      tags$hr(),
      
      # Custom output mappings
      tags$h5("Custom Output Combinations"),
      tags$p("Combine outputs from different models in a single plot."),
      actionButton("add_custom_mapping", "Add Custom Combination", class = "btn btn-info btn-sm"),
      tags$br(), tags$br(),
      uiOutput("custom_mappings_ui")
    )
  }
})

# UI for custom mappings
output$custom_mappings_ui <- renderUI({
  if (output_mappings$count == 0) return(NULL)
  
  # Get valid mapping IDs (non-NULL entries)
  valid_ids <- which(sapply(output_mappings$mappings, function(x) !is.null(x)))
  
  if (length(valid_ids) == 0) return(NULL)
  
  lapply(valid_ids, function(mapping_id) {
    tagList(
      div(
        style = "border: 2px solid #337ab7; padding: 15px; margin-bottom: 15px; border-radius: 8px; background: #f0f8ff;",
        fluidRow(
          column(10,
            tags$h6(paste("Custom Combination", mapping_id))
          ),
          column(2,
            actionButton(
              paste0("remove_mapping_", mapping_id),
              "Remove",
              class = "btn btn-danger btn-sm",
              style = "float: right;"
            )
          )
        ),
        textInput(
          inputId = paste0("custom_mapping_name_", mapping_id),
          label = "Plot Title:",
          value = paste("Custom Combination", mapping_id)
        ),
        
        # Dropdowns for each model side by side
        tags$strong("Select outputs to combine:"),
        fluidRow(
          lapply(seq_len(n_models), function(model_idx) {
            meta <- models_metadata[[model_idx]]
            model_name <- tools::file_path_sans_ext(basename(meta$filename))
            
            # Create choices for this model's outputs
            output_choices <- c("None" = "none", setNames(
              meta$output_var,
              meta$output_label
            ))
            
            column(
              width = floor(12 / n_models),
              selectInput(
                inputId = paste0("custom_mapping_", mapping_id, "_model_", model_idx),
                label = model_name,
                choices = output_choices,
                selected = "none"
              )
            )
          })
        ),
        
        selectInput(
          inputId = paste0("custom_mapping_transform_", mapping_id),
          label = "Display as:",
          choices = c(
            "Absolute Value" = "absolute",
            "Change from Baseline" = "change",
            "% Change from Baseline" = "percent_change"
          ),
          selected = "absolute"
        )
      )
    )
  })
})

# Add custom mapping button
observeEvent(input$add_custom_mapping, {
  new_id <- output_mappings$next_id
  output_mappings$mappings[[new_id]] <- list(created = TRUE)
  output_mappings$count <- output_mappings$count + 1
  output_mappings$next_id <- output_mappings$next_id + 1
})

# Remove custom mapping buttons - dynamically observe all possible IDs
observe({
  # Check all possible mapping IDs up to next_id
  if (output_mappings$next_id > 1) {
    lapply(1:(output_mappings$next_id - 1), function(mapping_id) {
      observeEvent(input[[paste0("remove_mapping_", mapping_id)]], {
        # Mark this mapping as NULL instead of removing
        output_mappings$mappings[[mapping_id]] <- NULL
        output_mappings$count <- max(0, output_mappings$count - 1)
      }, ignoreInit = TRUE)
    })
  }
})

  observeEvent(input$run_sim, {
    # Load all models
  models <- lapply(model_filenames, function(fname) {
    model_path <- file.path(models_dir, fname)
    if (!file.exists(model_path)) {
      return(NULL)
    }
    tryCatch({
      mread(model_path)
    }, error = function(e) NULL)
  })
  
  # Check if any model failed to load
  if (any(sapply(models, is.null))) {
    output$sim_result <- renderText("Error loading one or more models.")
    return()
  }
    study_length_weeks <- input$trial_duration
    washout_weeks <- if (input$trial_design == "Cross-over") input$washout_weeks else 0
    n_studies <- input$n_trials
    interval_type <- input$interval_type
    include_variability <- input$include_variability

    # Generate treatment groups and input datasets for EACH model
  all_results <- lapply(seq_len(n_models), function(model_idx) {
    meta <- models_metadata[[model_idx]]
    mod <- models[[model_idx]]
    model_name <- tools::file_path_sans_ext(basename(meta$filename))
    
    # Build treatment_groups_df for this model
    n_groups <- treatment_groups[[paste0("model_", model_idx, "_count")]]
    
    treatment_groups_df <- lapply(1:n_groups, function(i) {
      lapply(seq_along(meta$input_compartment), function(j) {
        data.frame(
          GroupName = input[[paste0("m", model_idx, "_group_name_", i)]],
          SampleSize = input[[paste0("m", model_idx, "_sample_size_", i)]],
          Treatment = input[[paste0("m", model_idx, "_treatment_", i)]],
          Dose = input[[paste0("m", model_idx, "_dose_", i, "_", j)]],
          Frequency = input[[paste0("m", model_idx, "_frequency_", i, "_", j)]],
          Compartment = meta$input_compartment[j]
        )
      }) %>% bind_rows()
    }) %>% bind_rows()
    
    # Create GroupID mapping
    group_mapping <- treatment_groups_df %>%
      distinct(GroupName) %>%
      mutate(GroupID = row_number())
    
    # Generate input dataset for this model
    input_dataset <- generate_input_dataset(
      treatment_groups = treatment_groups_df,
      study_length_weeks = study_length_weeks,
      design = tolower(input$trial_design),
      washout_weeks = 0,
      model_time_unit = meta$model_time_unit,
      input_compartment = meta$input_compartment,
      dose_unit = paste(meta$dose_unit, collapse = ","),
      model_dose_unit = paste(meta$model_dose_unit, collapse = ",")
    ) %>%
      left_join(group_mapping, by = "GroupName")
    
    # Simulation function for one study
    sim_one_study <- function(study_id) {
      if (include_variability) {
        mod %>%
          data_set(input_dataset) %>%
          carry_out(GroupID) %>%
          mrgsim(
            end = convert_time(study_length_weeks * 7 * 24, "hours", meta$model_time_unit, meta$model_time_unit),
            delta = convert_time(24, "hours", meta$model_time_unit, meta$model_time_unit)
          ) %>%
          as.data.frame() %>%
          mutate(study = study_id)
      } else {
        mod %>%
          zero_re() %>%
          data_set(input_dataset) %>%
          carry_out(GroupID) %>%
          mrgsim(
            end = convert_time(study_length_weeks * 7 * 24, "hours", meta$model_time_unit, meta$model_time_unit),
            delta = convert_time(24, "hours", meta$model_time_unit, meta$model_time_unit)
          ) %>%
          as.data.frame() %>%
          mutate(study = study_id)
      }
    }
    
    # Run multiple studies for this model
    results <- lapply(1:n_studies, sim_one_study) %>%
      bind_rows() %>%
      left_join(group_mapping, by = "GroupID")
    
    list(
      model_name = model_name,
      results = results,
      metadata = meta,
      group_mapping = group_mapping
    )
  })

    # Store results as LIST (not combined)
  sim_results(all_results)
  
  # Store global metadata
  sim_metadata(list(
    n_models = n_models,
    n_studies = n_studies,
    interval_type = interval_type,
    include_variability = include_variability
  ))
  }) 

  # Reactive expression that converts time and summarizes results
summaries_converted <- reactive({
  # Check if data exists
  if (is.null(sim_results()) || is.null(sim_metadata())) {
    return(NULL)
  }
  
  all_results <- sim_results()
  global_metadata <- sim_metadata()
  
  # Process each model's results separately
  all_summaries <- lapply(all_results, function(model_data) {
    results <- model_data$results
    meta <- model_data$metadata
    model_name <- tools::file_path_sans_ext(basename(meta$filename))
    
    # Check if results exist
    if (is.null(results) || nrow(results) == 0) {
      return(NULL)
    }
    
    # Convert time to selected unit (using THIS model's time unit)
    results_converted <- results %>%
      mutate(time = convert_time(time, meta$model_time_unit, input$time_unit, meta$model_time_unit))
    
    # Create output mapping for this model
    output_mapping <- tibble(
      variable = meta$output_var,
      label = meta$output_label
    )
    
    # Summarize based on interval type
    if (global_metadata$n_studies > 1 && input$interval_type == "ci") {
      summaries <- lapply(meta$output_var, function(yvar) {
        results_converted %>%
          group_by(study, GroupName, time) %>%
          summarise(
            mean_val = mean(.data[[yvar]], na.rm = TRUE),
            lq = quantile(.data[[yvar]], 0.025, na.rm = TRUE),
            uq = quantile(.data[[yvar]], 0.975, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          group_by(GroupName, time) %>%
          summarise(
            median = median(mean_val, na.rm = TRUE),
            lower = quantile(mean_val, 0.05, na.rm = TRUE),
            upper = quantile(mean_val, 0.95, na.rm = TRUE),
            variable = yvar,
            .groups = "drop"
          )
      }) %>% bind_rows()
    } else if (input$interval_type == "pi") {
      summaries <- lapply(meta$output_var, function(yvar) {
        results_converted %>%
          group_by(study, GroupName, time) %>%
          summarise(
            mean_val = mean(.data[[yvar]], na.rm = TRUE),
            lq = quantile(.data[[yvar]], 0.025, na.rm = TRUE),
            uq = quantile(.data[[yvar]], 0.975, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          group_by(GroupName, time) %>%
          summarise(
            median = median(mean_val, na.rm = TRUE),
            lower = median(lq, na.rm = TRUE),
            upper = median(uq, na.rm = TRUE),
            variable = yvar,
            .groups = "drop"
          )
      }) %>% bind_rows()
    } else {
      # Single study case
      summaries <- lapply(meta$output_var, function(yvar) {
        results_converted %>%
          group_by(GroupName, time) %>%
          summarise(
            median = mean(.data[[yvar]], na.rm = TRUE),
            lower = quantile(.data[[yvar]], 0.025, na.rm = TRUE),
            upper = quantile(.data[[yvar]], 0.975, na.rm = TRUE),
            variable = yvar,
            .groups = "drop"
          )
      }) %>% bind_rows()
    }
    
    # Add labels and return as list item
    list(
      model_name = model_name,
      summaries = summaries %>%
        left_join(output_mapping, by = "variable"),
      metadata = meta
    )
  })
  
  # Return as list (one entry per model)
  all_summaries
})
  
# Generate separate plot outputs for each model
observe({
  summaries_list <- summaries_converted()
  global_metadata <- sim_metadata()
  
  if (!is.null(summaries_list) && !is.null(global_metadata)) {
    lapply(seq_len(global_metadata$n_models), function(model_idx) {
      output_name <- paste0("model_plot_", model_idx)
      
      output[[output_name]] <- renderPlotly({
        model_summary <- summaries_list[[model_idx]]
        summaries <- model_summary$summaries
        model_name <- model_summary$model_name
        
        time_label <- switch(input$time_unit,
          "hours" = "Time (hours)",
          "days" = "Time (days)",
          "weeks" = "Time (weeks)",
          "months" = "Time (months)"
        )
        
        y_label <- if (length(unique(summaries$label)) == 1) {
          unique(summaries$label)
        } else {
          "Concentration"
        }
        
        g <- ggplot(summaries, aes(x = time, y = median, color = GroupName)) +
          geom_ribbon(aes(ymin = lower, ymax = upper, fill = GroupName), alpha = 0.3) +
          geom_line() +
          facet_wrap(~ label, scales = "free_y") +
          labs(
            title = model_name,
            x = time_label,
            y = y_label
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
          )
        
        ggplotly(g)
      })
    })
  }
})

# Generate plots for custom mappings
observe({
  summaries_list <- summaries_converted()
  global_metadata <- sim_metadata()
  
  if (!is.null(summaries_list) && !is.null(global_metadata) && global_metadata$n_models > 1) {
    # Get valid mapping IDs
    valid_ids <- which(sapply(output_mappings$mappings, function(x) !is.null(x)))
    
    if (length(valid_ids) > 0) {
      lapply(valid_ids, function(mapping_id) {
        output_name <- paste0("custom_mapping_plot_", mapping_id)
        
        output[[output_name]] <- renderPlotly({
          # Collect selected outputs from each model
          selected_outputs <- lapply(seq_len(global_metadata$n_models), function(model_idx) {
            selected_var <- input[[paste0("custom_mapping_", mapping_id, "_model_", model_idx)]]
            list(
              model_idx = model_idx,
              variable = selected_var
            )
          })
          
          # Filter to only selected outputs (not "none")
          selected_outputs <- Filter(function(x) x$variable != "none", selected_outputs)
          
          if (length(selected_outputs) == 0) {
            # No outputs selected
            return(ggplot() + 
              annotate("text", x = 0.5, y = 0.5, label = "Please select outputs to display",
                       hjust = 0.5, vjust = 0.5, size = 5) +
              theme_void())
          }
          
          # Combine data from selected outputs
          combined_data <- lapply(selected_outputs, function(sel) {
            model_idx <- sel$model_idx
            variable <- sel$variable
            
            model_summary <- summaries_list[[model_idx]]
            summaries <- model_summary$summaries
            model_name <- model_summary$model_name
            
            # Filter to this variable
            summaries %>%
              filter(variable == !!variable) %>%
              mutate(
                model_source = model_name,
                group_model = paste0(GroupName, " (", model_name, ")")
              )
          }) %>% bind_rows()
          
          # Apply transformation if needed
          transform_type <- input[[paste0("custom_mapping_transform_", mapping_id)]]
          
          if (transform_type %in% c("change", "percent_change")) {
            # Calculate baseline (first timepoint) for each group and model
            combined_data <- combined_data %>%
              group_by(GroupName, model_source) %>%
              arrange(time) %>%
              mutate(
                baseline_median = first(median),
                baseline_lower = first(lower),
                baseline_upper = first(upper)
              ) %>%
              ungroup()
            
            if (transform_type == "change") {
              combined_data <- combined_data %>%
                mutate(
                  median = median - baseline_median,
                  lower = lower - baseline_lower,
                  upper = upper - baseline_upper
                )
            } else if (transform_type == "percent_change") {
              combined_data <- combined_data %>%
                mutate(
                  median = ((median - baseline_median) / baseline_median) * 100,
                  lower = ((lower - baseline_lower) / baseline_lower) * 100,
                  upper = ((upper - baseline_upper) / baseline_upper) * 100
                )
            }
          }
          
          # Get custom title
          custom_title <- input[[paste0("custom_mapping_name_", mapping_id)]]
          if (is.null(custom_title) || custom_title == "") {
            custom_title <- paste("Custom Combination", mapping_id)
          }
          
          time_label <- switch(input$time_unit,
            "hours" = "Time (hours)",
            "days" = "Time (days)",
            "weeks" = "Time (weeks)",
            "months" = "Time (months)"
          )
          
          y_label <- switch(transform_type,
            "absolute" = "Concentration",
            "change" = "Change from Baseline",
            "percent_change" = "% Change from Baseline",
            "Concentration"
          )
          
          # Create plot
          g <- ggplot(combined_data, aes(x = time, y = median, color = group_model, linetype = model_source)) +
            geom_ribbon(aes(ymin = lower, ymax = upper, fill = group_model), alpha = 0.2, linetype = 0) +
            geom_line() +
            labs(
              title = custom_title,
              x = time_label,
              y = y_label,
              color = "Group (Model)",
              linetype = "Model Source"
            ) +
            theme_bw() +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
              legend.position = "bottom"
            )
          
          ggplotly(g)
        })
      })
    }
  }
})

# Create dynamic UI with separate plots
output$sim_result <- renderUI({
  if (is.null(sim_results()) || is.null(sim_metadata())) {
    div(
      style = "text-align: center; padding: 50px;",
      h4("No simulation results yet. Click 'Run Simulation' to begin.")
    )
  } else {
    global_metadata <- sim_metadata()
    
    # Get valid custom mapping IDs - safely handle empty mappings
    valid_mapping_ids <- if (length(output_mappings$mappings) > 0) {
      which(sapply(output_mappings$mappings, function(x) !is.null(x)))
    } else {
      integer(0)
    }
    
    tagList(
      h3(paste("Simulation Results -", global_metadata$n_models, "Model(s)"),
         style = "text-align: center;"),
      
      # Individual model plots
      lapply(seq_len(global_metadata$n_models), function(model_idx) {
        div(
          style = "margin-bottom: 30px; border: 1px solid #ddd; padding: 15px; border-radius: 8px;",
          plotlyOutput(paste0("model_plot_", model_idx), height = "400px")
        )
      }),
      
      # Custom mapping plots (if any exist)
      if (length(valid_mapping_ids) > 0) {
        tagList(
          h4("Custom Output Combinations", style = "margin-top: 30px; border-top: 2px solid #337ab7; padding-top: 20px;"),
          lapply(valid_mapping_ids, function(mapping_id) {
            div(
              style = "margin-bottom: 30px; border: 2px solid #337ab7; padding: 15px; border-radius: 8px; background: #f0f8ff;",
              plotlyOutput(paste0("custom_mapping_plot_", mapping_id), height = "400px")
            )
          })
        )
      }
    )
  }
})

  
  # Display summarized results in a table - also reactive to time_unit changes
  output$sim_table <- renderTable({
    # Check if simulation has been run
    if (is.null(sim_results()) || is.null(sim_metadata())) {
      return(data.frame(Message = "No simulation results yet. Click 'Run Simulation' to begin."))
    }
    
    head(summaries_converted(), 20)
  })
  
  # ========== VALIDATION =========================================
  
  # validation helper function, performing simulation and providing simulation summary
  compute_validation_summaries <- function(
    sim_pipeline,
    output_times,
    meta,
    n_subjects,
    time_unit
  ) {
    
    # Single individual → no CI
    if (n_subjects == 1) {
      
      res <- sim_pipeline %>%
        mrgsim(tgrid = output_times) %>%
        as.data.frame() %>%
        mutate(time = convert_time(time, meta$model_time_unit, time_unit, meta$model_time_unit))
      
      summaries <- lapply(meta$output_var, function(yvar) {
        res %>%
          group_by(time) %>%
          summarise(
            median = mean(.data[[yvar]], na.rm = TRUE),
            lower = NA_real_,
            upper = NA_real_,
            variable = yvar,
            .groups = "drop"
          )
      }) %>% bind_rows()
      
      return(summaries)
    }
    
    # Multiple individuals → CI via replicates
    n_replicates <- 100
    
    replicate_means <- lapply(seq_len(n_replicates), function(i) {
      
      sim_result <- sim_pipeline %>%
        mrgsim(tgrid = output_times) %>%
        as.data.frame() %>%
        mutate(time = convert_time(time, meta$model_time_unit, time_unit, meta$model_time_unit))
      
      # Extract means for each variable
      variable_data <- lapply(meta$output_var, function(yvar) {
        sim_result %>%
          group_by(time) %>%
          summarise(
            mean_val = mean(.data[[yvar]], na.rm = TRUE),
            variable = yvar,
            .groups = "drop"
          )
      }) %>% bind_rows()
      
      return(variable_data)
    }) %>% bind_rows(.id = "replicate")
    
    replicate_means %>%
      group_by(time, variable) %>%
      summarise(
        median = median(mean_val),
        lower  = quantile(mean_val, 0.025),
        upper  = quantile(mean_val, 0.975),
        .groups = "drop"
      )
  }
  
  # Reactive values to store both internal and external validation results
  internal_validation_results <- reactiveVal(NULL)
  external_validation_results <- reactiveVal(NULL)
  validation_results <- reactiveVal(NULL)
  validation_metadata <- reactiveVal(NULL)
  
  # Reactive value to track which validation type to show (internal or external)
  validation_type <- reactiveVal("internal")
  
  # Helper function to run validation for a specific validation data set
  run_validation_simulations <- function(val_data_source) {
    cat("DEBUG VALIDATION START: Running", val_data_source, "validation\n")
    
    # First, check how many models have this validation data type
    has_val_data <- sapply(seq_len(n_models), function(idx) {
      meta <- models_metadata[[idx]]
      if (val_data_source == "external") {
        !is.null(meta$external_validation_data)
      } else {
        !is.null(meta$internal_validation_data)
      }
    })
    cat("DEBUG VALIDATION: Models with", val_data_source, "data: ", sum(has_val_data), "out of", n_models, "\n")
    cat("DEBUG VALIDATION: Which models have", val_data_source, "data:", which(has_val_data), "\n")
    
    # Load all models
    models <- lapply(model_filenames, function(fname) {
      model_path <- file.path(models_dir, fname)
      if (!file.exists(model_path)) {
        return(NULL)
      }
      tryCatch({
        mread(model_path)
      }, error = function(e) NULL)
    })
    
    if (any(sapply(models, is.null))) {
      cat("DEBUG VALIDATION: Some models failed to load\n")
      return(NULL)
    }
    
    # Run simulations for each model - ALL study arms from JSON
    all_val_results <- lapply(seq_len(n_models), function(model_idx) {
      meta <- models_metadata[[model_idx]]
      mod <- models[[model_idx]]
      model_name <- tools::file_path_sans_ext(basename(meta$filename))
      
      cat("DEBUG VALIDATION: Processing model", model_idx, "(", model_name, ") for", val_data_source, "\n")
      
      # Select appropriate validation data source
      val_data <- if (val_data_source == "external") {
        meta$external_validation_data
      } else {
        meta$internal_validation_data
      }
      
      # Check if validation data exists
      if (is.null(val_data) || length(val_data) == 0) return(NULL)
      
      # Run validation simulations for each model, each study and each arm that is available
      all_study_arm_results <- lapply(seq_along(val_data$studies), function(study_idx) {
        
        study <- val_data$studies[[study_idx]]
        study_id   <- study$study_id   %||% paste0("Study_", study_idx)
        study_name <- study$study_name %||% study_id
        
        cat("DEBUG VALIDATION: Processing", study_name, "for model", model_name, "\n")
        
        arms <- study$arms
        
        # ===== STUDY-LEVEL CACHING LOGIC =====
        # Determine study cache directory from first arm's data_location
        study_cache_dir <- NULL
        study_config_hash <- NULL
        cache_hit <- FALSE
        
        first_arm <- arms[[1]]
        if (!is.null(first_arm) && is.list(first_arm) && !is.null(first_arm$data_location)) {
          data_location <- first_arm$data_location
          parts <- strsplit(data_location, "/")[[1]]
          model_idx_in_path <- which(parts == model_name)
          if (length(model_idx_in_path) > 0 && model_idx_in_path + 1 <= length(parts)) {
            study_folder_name <- parts[model_idx_in_path + 1]
            study_cache_dir <- file.path("../data/derived/", "validation", model_name, study_folder_name)
            # Create the directory immediately so arm summaries can be saved
            dir.create(study_cache_dir, recursive = TRUE, showWarnings = FALSE)
          }
        }
        
        # Compute hash of the complete study configuration (all arms)
        if (!is.null(study_cache_dir)) {
          study_config_json <- jsonlite::toJSON(study, auto_unbox = TRUE, pretty = FALSE)
          study_config_hash <- digest::digest(study_config_json, algo = "sha256")
          cat("DEBUG VALIDATION: Study", study_name, "- cache directory:", study_cache_dir, "\n")
          cat("DEBUG VALIDATION: Study", study_name, "- config hash:", study_config_hash, "\n")
          
          # Check if cache exists and hash matches
          hash_file <- file.path(study_cache_dir, "config_hash.txt")
          if (file.exists(hash_file)) {
            stored_hash <- trimws(readLines(hash_file, n = 1))
            cat("DEBUG VALIDATION: Study", study_name, "- stored hash:", stored_hash, "\n")
            if (stored_hash == study_config_hash) {
              cache_hit <- TRUE
              cat("DEBUG VALIDATION: Study", study_name, "- CACHE HIT! Loading cached results for all arms.\n")
            }
          }
        }
        
        # If cache hits, load all arm summaries from subdirectories
        if (cache_hit && !is.null(study_cache_dir)) {
          cat("DEBUG VALIDATION: Study", study_name, "- Loading cached results.\n")
          cached_results <- list()
          
          for (arm_idx in seq_along(arms)) {
            arm <- arms[[arm_idx]]
            arm_name <- if (is.list(arm) && !is.null(arm$arm_name)) arm$arm_name else paste("Arm", arm_idx)
            arm_cache_subdir <- file.path(study_cache_dir, paste0("arm_", arm_idx))
            summaries_file <- file.path(arm_cache_subdir, "summaries.rds")
            
            if (file.exists(summaries_file)) {
              tryCatch({
                summaries <- readRDS(summaries_file)
                cat("DEBUG VALIDATION: Study", study_name, "- Arm", arm_idx, "loaded from cache\n")
                
                cached_results[[arm_idx]] <- list(
                  model_name = model_name,
                  study_id = study_id,
                  study_name = study_name,
                  arm_name = arm_name,
                  arm_idx = arm_idx,
                  dose = if (is.list(arm) && !is.null(arm$dose)) arm$dose else 10,
                  frequency = if (is.list(arm) && !is.null(arm$frequency)) arm$frequency else NA_character_,
                  frequency_found = !is.na(if (is.list(arm) && !is.null(arm$frequency)) arm$frequency else NA_character_),
                  results = NULL,
                  sim_pipeline = NULL,
                  output_times = NULL,
                  metadata = meta,
                  data_location = if (is.list(arm) && !is.null(arm$data_location)) arm$data_location else NULL,
                  summaries = summaries,
                  cached = TRUE
                )
              }, error = function(e) {
                cat("DEBUG VALIDATION: Study", study_name, "- Arm", arm_idx, "- Error loading from cache:", e$message, "\n")
              })
            } else {
              cat("DEBUG VALIDATION: Study", study_name, "- Arm", arm_idx, "- Summaries file not found\n")
            }
          }
          
          # Filter out NULL results
          cached_results <- Filter(function(x) !is.null(x), cached_results)
          return(cached_results)
        }
        
        # ===== CACHE MISS: Run all arms =====
        cat("DEBUG VALIDATION: Study", study_name, "- Running simulations for all", length(arms), "arms.\n")
        
        all_study_arm_results <- lapply(seq_along(arms), function(arm_idx) {
        arm <- study$arms[[arm_idx]]
        
        # Ensure arm is a list (defensive check)
        if (!is.list(arm)) {
          cat("DEBUG: Arm", arm_idx, "is not a list (is", class(arm), "), attempting to handle as data\n")
          # If arms themselves are atomic, they might be nested differently
          # Try to get arm info from parent structure
          return(NULL)
        }
        
        # Extract study length and frequency from validation data CSV (if available)
        data_location <- if (is.list(arm) && !is.null(arm$data_location)) arm$data_location else NULL
        study_length_weeks <- 100 / 7  # Default fallback (100 days = ~14.3 weeks)
        frequency_from_data <- NULL  # To store frequency from data file
        n_subjects_from_data <- NULL  # To store n_subjects from data file
        no_doses_from_data <- NULL  # To store number of doses from data file
        
        if (!is.null(data_location) && data_location != "") {
          data_path <- file.path("../", data_location)
          cat("DEBUG: Looking for data at:", data_path, "\n")
          if (file.exists(data_path)) {
            tryCatch({
              obs_raw <- read.csv(data_path, sep = ";", stringsAsFactors = FALSE)
              # Trim whitespace from column names to handle CSV issues
              names(obs_raw) <- trimws(names(obs_raw))
              cat("DEBUG: CSV column names after trimming:", paste(names(obs_raw), collapse = ", "), "\n")
              
              # Find the Time column and get maximum value, accounting for time unit
              if ("Time" %in% names(obs_raw)) {
                # Read time_unit from CSV if available
                time_unit_from_data <- "days"  # default
                if ("Time_unit" %in% names(obs_raw)) {
                  time_unit_from_data <- unique(obs_raw$Time_unit)[1]
                  cat("DEBUG: Arm", arm_idx, "- found time_unit in data:", time_unit_from_data, "\n")
                }
                
                max_time <- max(as.numeric(obs_raw$Time), na.rm = TRUE)
                if (!is.na(max_time) && max_time > 0) {
                  # Convert from data time unit to weeks for generate_input_dataset
                  study_length_weeks <- convert_time(max_time, time_unit_from_data, "weeks", "weeks")
                  cat("DEBUG: Arm", arm_idx, "- extracted max time from data:", max_time, time_unit_from_data, "→", study_length_weeks, "weeks\n")
                }
              }
              
              # Get the arm's dose and frequency from JSON
              arm_dose <- if (is.list(arm) && !is.null(arm$dose)) arm$dose else NULL
              arm_frequency_json <- if (is.list(arm) && !is.null(arm$frequency)) arm$frequency else NULL
              
              # STEP 1: Filter CSV by dose (always do this first)
              obs_filtered_by_dose <- obs_raw
              if (!is.null(arm_dose) && "Dose_mg" %in% names(obs_raw)) {
                cat("DEBUG: Arm", arm_idx, "- filtering CSV by dose", arm_dose, "\n")
                obs_filtered_by_dose <- obs_raw %>%
                  filter(Dose_mg == arm_dose)
                if (nrow(obs_filtered_by_dose) == 0) {
                  cat("DEBUG: Arm", arm_idx, "- WARNING: No rows in CSV match dose", arm_dose, "\n")
                  obs_filtered_by_dose <- obs_raw
                }
              }
              
              # STEP 2: Filter by frequency (JSON first, then extract from data if needed)
              obs_filtered_by_dose_and_freq <- obs_filtered_by_dose
              
              if (!is.null(arm_frequency_json)) {
                # JSON has frequency - filter CSV by BOTH dose and JSON frequency
                cat("DEBUG: Arm", arm_idx, "- JSON has frequency: '", arm_frequency_json, "'. Filtering CSV by dose AND this frequency.\n")
                possible_freq_cols <- c("Frequency", "Dose_Frequency", "DoseFrequency", "FREQ")
                freq_col_found <- FALSE
                for (col in possible_freq_cols) {
                  matching_col <- grep(paste0("^", col, "$"), names(obs_filtered_by_dose), ignore.case = TRUE, value = TRUE)
                  if (length(matching_col) > 0) {
                    obs_filtered_by_dose_and_freq <- obs_filtered_by_dose %>%
                      filter(.data[[matching_col[1]]] == arm_frequency_json)
                    if (nrow(obs_filtered_by_dose_and_freq) > 0) {
                      cat("DEBUG: Arm", arm_idx, "- filtered CSV by dose AND JSON frequency '", arm_frequency_json, "'\n")
                      freq_col_found <- TRUE
                      break
                    } else {
                      cat("DEBUG: Arm", arm_idx, "- WARNING: No CSV rows match dose AND JSON frequency '", arm_frequency_json, "'. Using dose-filtered data.\n")
                      obs_filtered_by_dose_and_freq <- obs_filtered_by_dose
                    }
                  }
                }
              } else {
                # JSON doesn't have frequency - extract it from CSV and filter by it
                cat("DEBUG: Arm", arm_idx, "- JSON has NO frequency. Extracting frequency from CSV.\n")
                possible_freq_cols <- c("Frequency", "Dose_Frequency", "DoseFrequency", "FREQ")
                for (col in possible_freq_cols) {
                  matching_col <- grep(paste0("^", col, "$"), names(obs_filtered_by_dose), ignore.case = TRUE, value = TRUE)
                  if (length(matching_col) > 0) {
                    freq_val <- unique(obs_filtered_by_dose[[matching_col[1]]])[1]
                    if (!is.na(freq_val) && freq_val != "") {
                      frequency_from_data <- as.character(freq_val)
                      obs_filtered_by_dose_and_freq <- obs_filtered_by_dose %>%
                        filter(.data[[matching_col[1]]] == frequency_from_data)
                      cat("DEBUG: Arm", arm_idx, "- extracted and filtered by frequency from data: '", frequency_from_data, "'\n")
                      break
                    }
                  }
                }
              }
              
              # STEP 3: Extract n_subjects and n_doses from the properly filtered data
              obs_filtered_for_metadata <- obs_filtered_by_dose_and_freq
              
              # Extract n_subjects
              possible_n_cols <- c("N", "N_subjects", "Subjects", "SampleSize", "n_subjects", "NSub")
              for (col in possible_n_cols) {
                matching_col <- grep(paste0("^", col, "$"), names(obs_filtered_for_metadata), ignore.case = TRUE, value = TRUE)
                if (length(matching_col) > 0) {
                  n_val <- unique(obs_filtered_for_metadata[[matching_col[1]]])[1]
                  if (!is.na(n_val) && n_val != "") {
                    n_subjects_from_data <- as.numeric(n_val)
                    if (!is.na(n_subjects_from_data) && n_subjects_from_data > 0) {
                      cat("DEBUG: Arm", arm_idx, "- extracted n_subjects from data column '", matching_col[1], "':", n_subjects_from_data, "\n")
                      break
                    }
                  }
                }
              }
              
              # Extract n_doses
              possible_doses_cols <- c("no_doses", "No_Doses", "NumDoses", "Num_Doses", "n_doses", "Doses")
              for (col in possible_doses_cols) {
                if (col %in% names(obs_filtered_for_metadata)) {
                  doses_val <- unique(obs_filtered_for_metadata[[col]])[1]
                  if (!is.na(doses_val) && doses_val != "") {
                    no_doses_from_data <- as.numeric(doses_val)
                    if (!is.na(no_doses_from_data) && no_doses_from_data > 0) {
                      cat("DEBUG: Arm", arm_idx, "- extracted no_doses from data column '", col, "':", no_doses_from_data, "\n")
                      break
                    }
                  }
                }
              }
            }, error = function(e) {
              cat("DEBUG: Could not extract time/frequency/n_subjects/no_doses from data_location for arm", arm_idx, ":", e$message, "\n")
              NULL
            })
          } else {
            cat("DEBUG: Data file not found at:", data_path, "\n")
          }
        } else {
          cat("DEBUG: No data_location for arm", arm_idx, "\n")
        }
        
        # Create treatment group data frame from arm
        arm_name <- if (is.list(arm) && !is.null(arm$arm_name)) arm$arm_name else paste("Arm", arm_idx)
        cat("DEBUG: Arm", arm_idx, "name:", arm_name, "\n")
        
        # Determine frequency: JSON > data file (if JSON not specified) > NO DEFAULT
        # If JSON has frequency, use it. Otherwise, extract from data for this dose.
        # Do NOT use a default if neither JSON nor data have frequency!
        arm_frequency <- if (is.list(arm) && !is.null(arm$frequency)) {
          cat("DEBUG: Arm", arm_idx, "- using frequency from JSON:", arm$frequency, "\n")
          arm$frequency
        } else if (!is.null(frequency_from_data)) {
          cat("DEBUG: Arm", arm_idx, "- NO frequency in JSON, using frequency from CSV:", frequency_from_data, "\n")
          frequency_from_data
        } else {
          cat("DEBUG: Arm", arm_idx, "- WARNING: NO frequency found in JSON or CSV. Will not filter observed data by frequency.\n")
          NA_character_  # Use NA instead of a default
        }
        
        arm_frequency_found <- !is.na(arm_frequency)  # Track whether frequency was actually found
        
        # Determine n_subjects: data file > JSON > default
        arm_n_subjects <- if (!is.null(n_subjects_from_data)) {
          cat("DEBUG: Arm", arm_idx, "- using n_subjects from CSV:", n_subjects_from_data, "\n")
          n_subjects_from_data
        } else if (is.list(arm) && !is.null(arm$n_subjects)) {
          cat("DEBUG: Arm", arm_idx, "- using n_subjects from JSON:", arm$n_subjects, "\n")
          arm$n_subjects
        } else {
          cat("DEBUG: Arm", arm_idx, "- using default n_subjects: 100\n")
          100
        }
        
        # Determine no_doses: data file > JSON > default (addl = no_doses - 1)
        arm_no_doses <- if (!is.null(no_doses_from_data)) {
          no_doses_from_data
        } else if (is.list(arm) && !is.null(arm$no_doses)) {
          arm$no_doses
        } else {
          NULL  # Will be calculated from frequency if not specified
        }
        
        treatment_groups_df <- data.frame(
          GroupName = arm_name,
          SampleSize = arm_n_subjects,
          Treatment = arm_name,
          Dose = if (is.list(arm) && !is.null(arm$dose)) arm$dose else 10,
          Frequency = arm_frequency,
          Compartment = meta$input_compartment[1]
        )
        
        # Add Addl column if we have no_doses information
        if (!is.null(arm_no_doses)) {
          treatment_groups_df$Addl <- as.numeric(arm_no_doses) - 1
          cat("DEBUG: Added Addl column for arm", arm_idx, ", Addl value:", treatment_groups_df$Addl, "\n")
        }
        
        cat("DEBUG: Treatment groups for arm", arm_idx, ":\n")
        print(treatment_groups_df)
        cat("DEBUG: Study length weeks:", study_length_weeks, "\n")
        cat("DEBUG: Model time unit:", meta$model_time_unit, "\n")
        cat("DEBUG: Input compartment:", meta$input_compartment[1], "\n")

        # Generate input dataset
        tryCatch({
          cat("DEBUG: Calling generate_input_dataset for arm", arm_idx, "\n")
          input_dataset <- generate_input_dataset(
            treatment_groups = treatment_groups_df,
            study_length_weeks = study_length_weeks,
            design = "parallel",
            washout_weeks = 0,
            model_time_unit = meta$model_time_unit,
            input_compartment = meta$input_compartment[1],
            dose_unit = paste(meta$dose_unit, collapse = ","),
            model_dose_unit = paste(meta$model_dose_unit, collapse = ",")
          )
          cat("DEBUG: Input dataset generated successfully for arm", arm_idx, "\n")
          cat("DEBUG: Input dataset dimensions:", nrow(input_dataset), "rows,", ncol(input_dataset), "cols\n")
          cat("DEBUG: Input dataset columns:", paste(names(input_dataset), collapse = ", "), "\n")
          
          # Verify addl values are correctly set
          if ("addl" %in% names(input_dataset)) {
            unique_addl <- unique(input_dataset$addl)
            cat("DEBUG: Unique addl values in input_dataset:", paste(unique_addl, collapse = ", "), "\n")
          }
          
          cat("DEBUG: First few rows of input dataset:\n")
          print(head(input_dataset, 3))
        }, error = function(e) {
          cat("DEBUG: Error generating input dataset for arm", arm_idx, ":", e$message, "\n")
          cat("DEBUG: Full error details:\n")
          print(e)
          cat("DEBUG: Error traceback:\n")
          traceback()
          stop(e)
        })
        
        # Run single study - apply zero_re() only for single individual
        tryCatch({
          cat("DEBUG: Starting simulation for arm", arm_idx, "\n")
          cat("DEBUG: Number of subjects:", arm_n_subjects, "\n")
          cat("DEBUG: Simulation parameters - end:", convert_time(study_length_weeks, "weeks", meta$model_time_unit, meta$model_time_unit), 
              ", delta:", convert_time(24, "hours", meta$model_time_unit, meta$model_time_unit), "\n")
          
          # Build simulation pipeline - apply zero_re() only if single individual
          sim_pipeline <- if (arm_n_subjects == 1) {
            cat("DEBUG: Applying zero_re() for single individual\n")
            mod %>%
              zero_re() %>%
              data_set(input_dataset) %>%
              carry_out(GroupName)
          } else {
            cat("DEBUG: NOT applying zero_re() for", arm_n_subjects, "individuals - preserving variability for CI calculation\n")
            mod %>%
              data_set(input_dataset) %>%
              carry_out(GroupName)
          }
          
          # Generate specific output times for smooth plotting (every 12 hours)
          sim_end <- convert_time(study_length_weeks, "weeks", meta$model_time_unit, meta$model_time_unit)
          time_interval <- convert_time(12, "hours", meta$model_time_unit, meta$model_time_unit)
          output_times <- seq(0, sim_end, by = time_interval)
          cat("DEBUG: Generated", length(output_times), "output time points from 0 to", sim_end, "\n")
          
          results <- sim_pipeline %>%
            mrgsim(tgrid = output_times) %>%
            as.data.frame()
          cat("DEBUG: Simulation completed successfully for arm", arm_idx, "with", nrow(results), "rows\n")
          
          # Pre-compute summaries once using the helper function
          # Use model's native time unit during initialization
          summaries <- compute_validation_summaries(
            sim_pipeline = sim_pipeline,
            output_times = output_times,
            meta = meta,
            n_subjects = arm_n_subjects,
            time_unit = meta$model_time_unit
          )
          
          # Create labels for summaries
          output_mapping <- tibble(
            variable = meta$output_var,
            label = meta$output_label
          )
          
          summaries <- summaries %>%
            left_join(output_mapping, by = "variable")
          
          # Save arm summaries to study cache directory
          if (!is.null(study_cache_dir)) {
            tryCatch({
              arm_cache_subdir <- file.path(study_cache_dir, paste0("arm_", arm_idx))
              dir.create(arm_cache_subdir, recursive = TRUE, showWarnings = FALSE)
              
              # Save summaries as RDS to arm subdirectory
              saveRDS(summaries, file.path(arm_cache_subdir, "summaries.rds"))
              
              cat("DEBUG: Arm", arm_idx, "- Saved summaries to:", arm_cache_subdir, "\n")
            }, error = function(e) {
              cat("DEBUG: Warning - Could not save arm", arm_idx, "summaries:", e$message, "\n")
            })
          }
          
          list(
            model_name = model_name,
            study_id = study_id,
            study_name = study_name,
            arm_name = arm_name,
            arm_idx = arm_idx,
            dose = if (is.list(arm) && !is.null(arm$dose)) arm$dose else 10,
            frequency = arm_frequency,  # Use the actual frequency or NA
            frequency_found = arm_frequency_found,  # Track whether frequency was actually found
            results = results,
            sim_pipeline = sim_pipeline,
            output_times = output_times,
            metadata = meta,
            data_location = data_location,
            summaries = summaries,
            cached = FALSE  # Flag to indicate this was freshly computed
          )
        }, error = function(e) {
          cat("DEBUG: Error running simulation for arm", arm_idx, ":", e$message, "\n")
          cat("DEBUG: Full error details:\n")
          print(e)
          cat("DEBUG: Error traceback:\n")
          traceback()
          return(NULL)
        })
      })
      
      # AFTER processing all arms for this study, save the study-level config and hash
      if (!is.null(study_cache_dir)) {
        tryCatch({
          # Directory already created earlier, just save files
          
          # Create complete study configuration with all arms
          complete_study_config <- list(
            model_name = model_name,
            study_id = study_id,
            study_name = study_name,
            study_data_source = study$study_data_source,
            arms = study$arms  # Include ALL arms from the study
          )
          
          study_config_json_output <- jsonlite::toJSON(complete_study_config, auto_unbox = TRUE, pretty = TRUE)
          writeLines(study_config_json_output, file.path(study_cache_dir, "study_config.json"))
          
          # Save the config hash at study level
          if (!is.null(study_config_hash)) {
            writeLines(study_config_hash, file.path(study_cache_dir, "config_hash.txt"))
          }
          
          cat("DEBUG VALIDATION: Saved study config and hash for", study_name, "with", length(study$arms), "arms to:", study_cache_dir, "\n")
        }, error = function(e) {
          cat("DEBUG VALIDATION: Warning - Could not save study config for", study_name, ":", e$message, "\n")
        })
      }
      
      # Filter out NULL results
      all_study_arm_results <- Filter(function(x) !is.null(x), all_study_arm_results)
      cat("DEBUG VALIDATION: Model", model_idx, length(all_study_arm_results), "arm results\n")
      return(all_study_arm_results)
    })  # Close study lapply
    })  # Close model lapply
    
    # Debug the list structure before unlisting
    cat("DEBUG VALIDATION: all_val_results before processing - number of models:", length(all_val_results), "\n")
    for (i in seq_along(all_val_results)) {
      cat("DEBUG VALIDATION: Model", i, "result type:", class(all_val_results[[i]]), 
          "length:", length(all_val_results[[i]]), "\n")
    }
    
    # Debug the list structure before flattening
    cat("DEBUG VALIDATION: all_val_results before flattening\n")
    str(all_val_results, max.level = 3)
    
    # ---- FIX: Explicitly flatten models → studies → arms ----
    flat_results <- list()
    
    for (model_idx in seq_along(all_val_results)) {
      model_res <- all_val_results[[model_idx]]
      if (is.null(model_res)) next
      
      for (study_idx in seq_along(model_res)) {
        study_res <- model_res[[study_idx]]
        if (is.null(study_res)) next
        
        for (arm_idx in seq_along(study_res)) {
          arm_res <- study_res[[arm_idx]]
          if (!is.null(arm_res)) {
            flat_results <- c(flat_results, list(arm_res))
          }
        }
      }
    }
    
    cat("DEBUG VALIDATION: After flattening - total arm results:", length(flat_results), "\n")
    
    # Store in appropriate reactive value
    if (val_data_source == "internal") {
      internal_validation_results(flat_results)
    } else if (val_data_source == "external") {
      external_validation_results(flat_results)
    }
    
    # Always set metadata (even if empty)
    validation_metadata(list(
      n_studies = length(unique(vapply(
        flat_results,
        function(x) x$study_id %||% NA_character_,
        character(1)
      )))
    ))
    
    if (length(flat_results) == 0) {
      cat("DEBUG: No validation results for", val_data_source, "- stored empty list\n")
      return()
    }
    
    cat("DEBUG: Validation results set successfully\n")
  }
  
  # Generate validation plots
  observe({
    val_results <- validation_results()
    val_metadata <- validation_metadata()
    
    flat_results <- val_results #|> flatten()
    
    if (!is.null(val_results) && !is.null(val_metadata)) {
      lapply(seq_len(length(flat_results)), function(res_idx) {
        output_id <- paste0("validation_plot_", res_idx)
        
        output[[output_id]] <- renderPlotly({
          val_res <- flat_results[[res_idx]]
          results <- val_res$results
          sim_pipeline <- val_res$sim_pipeline
          output_times <- val_res$output_times
          meta <- val_res$metadata
          model_name <- val_res$model_name
          arm_name <- val_res$arm_name
          data_location <- val_res$data_location
          is_cached <- isTRUE(val_res$cached)
          
          # Skip result conversion if cached (results will be NULL)
          results_converted <- NULL
          n_individuals <- NA_integer_
          if (!is_cached && !is.null(results)) {
            # Convert time only for freshly computed results
            results_converted <- results %>%
              mutate(time = convert_time(time, meta$model_time_unit, input$time_unit, meta$model_time_unit))
            
            # Check number of individuals
            n_individuals <- length(unique(results_converted$ID))
            cat("DEBUG: Number of individuals in simulation:", n_individuals, "\n")
          }
          
          # USE pre-computed summaries from initialization
          summaries <- val_res$summaries
          
          if (is.null(summaries) || nrow(summaries) == 0) {
            cat("DEBUG WARNING: Pre-computed summaries are NULL or empty.\n")
            # Fallback: try to compute summaries if they're missing (only for non-cached results)
            if (!is_cached && !is.null(val_res$sim_pipeline) && !is.null(val_res$output_times) && !is.null(n_individuals)) {
              cat("DEBUG: Trying to compute summaries on the fly.\n")
              summaries <- compute_validation_summaries(
                sim_pipeline = val_res$sim_pipeline,
                output_times = val_res$output_times,
                meta = meta,
                n_subjects = n_individuals,
                time_unit = meta$model_time_unit
              )
            }
            if (is.null(summaries)) {
              cat("DEBUG ERROR: Could not compute summaries. Returning empty plot.\n")
              return(ggplotly(ggplot() + theme_minimal() + ggtitle("No simulation data available")))
            }
          } else {
            cat("DEBUG: Using pre-computed summaries with", nrow(summaries), "rows", if(is_cached) " (cached)" else "", "\n")
          }
          
          # Convert time from model units to user's selected display unit (ALWAYS do this)
          if (nrow(summaries) > 0 && "time" %in% names(summaries)) {
            summaries <- summaries %>%
              mutate(time = convert_time(time, meta$model_time_unit, input$time_unit, meta$model_time_unit))
          }
          
          # Debug: Check summaries structure and values
          cat("DEBUG: Summaries structure:\n")
          cat("DEBUG: Columns:", paste(names(summaries), collapse = ", "), "\n")
          cat("DEBUG: Unique lower values (first 5):", paste(unique(summaries$lower)[1:5], collapse = ", "), "\n")
          cat("DEBUG: Unique upper values (first 5):", paste(unique(summaries$upper)[1:5], collapse = ", "), "\n")
          cat("DEBUG: Any NA in lower?", any(is.na(summaries$lower)), "\n")
          cat("DEBUG: Any NA in upper?", any(is.na(summaries$upper)), "\n")
          if(!any(is.na(summaries$lower))) {
            cat("DEBUG: Sample lower values:", paste(head(summaries$lower, 3), collapse = ", "), "\n")
            cat("DEBUG: Sample upper values:", paste(head(summaries$upper, 3), collapse = ", "), "\n")
            cat("DEBUG: Sample median values:", paste(head(summaries$median, 3), collapse = ", "), "\n")
          }
          
          # Load observed data if available
          observed_data <- NULL
          observed_transformation <- NULL
          
          label_df <- data.frame(variable =meta$output_var,label=meta$output_label)
          summaries <- left_join(summaries, label_df)
          
          if (!is.null(data_location) && data_location != "") {
            cat("DEBUG PLOT: Looking for data at data_location:", data_location, "\n")
            data_path <- file.path("../", data_location)
            cat("DEBUG PLOT: Full path:", data_path, "\n")
            cat("DEBUG PLOT: File exists:", file.exists(data_path), "\n")
            if (file.exists(data_path)) {
              tryCatch({
                # Read the CSV file with semicolon separator
                cat("DEBUG PLOT: Reading CSV file...\n")
                obs_raw <- read.csv(data_path, sep = ";", stringsAsFactors = FALSE) %>%
                  as_tibble()
                # Trim whitespace from column names
                names(obs_raw) <- trimws(names(obs_raw))
                cat("DEBUG PLOT: CSV loaded, dimensions:", nrow(obs_raw), "rows,", ncol(obs_raw), "cols\n")
                cat("DEBUG PLOT: Column names:", paste(names(obs_raw), collapse = ", "), "\n")
                
                # Extract the DV_unit to determine transformation needed
                if ("DV_unit" %in% names(obs_raw)) {
                  observed_transformation <- unique(obs_raw$DV_unit)[1]
                  cat("DEBUG PLOT: Found DV_unit:", observed_transformation, "\n")
                }
                
                # Find the output variable column (DV_CHR or similar)
                output_col <- NA
                possible_cols <- c("DV_CHR", "Output", "Variable")
                for (col in possible_cols) {
                  if (col %in% names(obs_raw)) {
                    output_col <- col
                    cat("DEBUG PLOT: Found output column:", output_col, "\n")
                    break
                  }
                }
                
                # If not found, use second to last column
                if (is.na(output_col)) {
                  output_col <- names(obs_raw)[ncol(obs_raw) - 1]
                  cat("DEBUG PLOT: Using fallback output column:", output_col, "\n")
                }
                
                # Get the value column (DV or similar)
                value_col <- "DV"
                if (!("DV" %in% names(obs_raw))) {
                  if (ncol(obs_raw) >= 3) {
                    value_col <- names(obs_raw)[3]
                  } else {
                    value_col <- names(obs_raw)[2]
                  }
                  cat("DEBUG PLOT: Using value column:", value_col, "\n")
                }
                
                cat("DEBUG PLOT: Output col =", output_col, ", Value col =", value_col, "\n")
                cat("DEBUG PLOT: Has Time?", "Time" %in% names(obs_raw), "\n")
                
                # Extract relevant columns and rename
                if ("Time" %in% names(obs_raw) && value_col %in% names(obs_raw) && output_col %in% names(obs_raw)) {
                  cat("DEBUG PLOT: Extracting columns...\n")
                  cat("DEBUG PLOT: Unique", output_col, "values in CSV:", paste(unique(obs_raw[[output_col]]), collapse = ", "), "\n")
                  cat("DEBUG PLOT: Summary labels:", paste(unique(summaries$label), collapse = ", "), "\n")
                  
                  # Read time_unit from CSV if available
                  time_unit_from_data <- "days"  # default
                  if ("Time_unit" %in% names(obs_raw)) {
                    time_unit_from_data <- unique(obs_raw$Time_unit)[1]
                    cat("DEBUG PLOT: Found time_unit in CSV:", time_unit_from_data, "\n")
                  }
                  
                  # Create base tibble with required columns
                  observed_data <- obs_raw %>%
                    select(Time = "Time", 
                           observed_value = all_of(value_col),
                           output_var = all_of(output_col),
                           any_of(c("Frequency", "Dose_mg"))) %>%
                    mutate(
                      Time = as.numeric(Time),
                      observed_value = as.numeric(observed_value),
                      # Convert time from source unit to target unit
                      Time = convert_time(Time, time_unit_from_data, input$time_unit, time_unit_from_data)
                    )
                  
                  cat("DEBUG PLOT: Before filter - observed_data has", nrow(observed_data), "rows\n")
                  cat("DEBUG PLOT: Columns in observed_data:", paste(names(observed_data), collapse = ", "), "\n")
                  
                  # Match to our output variables - try exact match first, then partial match
                  observed_data_exact <- observed_data %>%
                    filter(output_var %in% unique(summaries$label))
                  
                  cat("DEBUG PLOT: After exact match - observed_data has", nrow(observed_data_exact), "rows\n")
                  
                  if (nrow(observed_data_exact) > 0) {
                    cat("DEBUG PLOT: Exact match successful\n")
                    observed_data <- observed_data_exact %>%
                      rename(label = output_var)
                  } else {
                    cat("DEBUG PLOT: CSV variable names:", paste(names(obs_raw), collapse = ", "), "\n")
                    
                    observed_data <- obs_raw %>%
                      select(Time = "Time", 
                             observed_value = all_of(value_col),
                             output_var = all_of(output_col),
                             any_of(c("Frequency", "Dose_mg"))) %>%
                      mutate(
                        Time = as.numeric(Time),
                        observed_value = as.numeric(observed_value),
                        Time = convert_time(Time, time_unit_from_data, input$time_unit, time_unit_from_data),
                        # Try to match by finding summary labels that contain this variable name
                        # Strip common prefixes (DV_, DV) to get the core variable name
                        core_var = gsub("^DV_|^DV", "", output_var, ignore.case = TRUE),
                        matched_label = sapply(core_var, function(var) {
                          # Check if this is a percent change variable
                          is_pct_change <- grepl("pct_change|percent_change|_pct|_pct_chg", var, ignore.case = TRUE)
                          
                          if (is_pct_change) {
                            # For percent change variables, prefer labels containing both the base variable and "change"/"percent"/"%"
                            base_var <- gsub("_pct_change|_percent_change|_pct_chg|_pct", "", var, ignore.case = TRUE)
                            matches <- summaries$label[
                              grepl(base_var, summaries$label, ignore.case = TRUE) &
                              grepl("change|percent|%", summaries$label, ignore.case = TRUE)
                            ]
                          } else {
                            # For regular variables, match directly
                            matches <- summaries$label[grepl(var, summaries$label, ignore.case = TRUE)]
                          }
                          
                          if (length(matches) > 0) {
                            cat("DEBUG: Matched '", var, "' (pct_change:", is_pct_change, ") to label '", matches[1], "'\n")
                            matches[1]
                          }
                        })
                      ) %>%
                      filter(!is.na(matched_label)) %>%
                      select(-output_var, -core_var) %>%
                      rename(label = matched_label)
                    
                    cat("DEBUG PLOT: After partial match - observed_data has", nrow(observed_data), "rows\n")
                  }
                  
                  cat("DEBUG PLOT: Final observed data extracted:", nrow(observed_data), "rows\n")
                } else {
                  cat("DEBUG PLOT: Missing required columns. Has Time:", "Time" %in% names(obs_raw), 
                      ", Has value_col:", value_col %in% names(obs_raw), 
                      ", Has output_col:", output_col %in% names(obs_raw), "\n")
                }
              }, error = function(e) {
                cat("DEBUG PLOT: Error loading/parsing data:", e$message, "\n")
                NULL
              })
            } else {
              cat("DEBUG PLOT: Data file not found\n")
            }
          } else {
            cat("DEBUG PLOT: No data_location provided\n")
          }
          
          # Apply transformation to match observed data format if needed
          if (!is.null(observed_transformation) && observed_transformation != "" && 
              !is.null(observed_data) && nrow(observed_data) > 0) {
            summaries <- transform_simulation_data(summaries, meta$output_var, observed_transformation)
          }
          
          # Filter observed data by dose (and frequency if available) to match this arm
          if (!is.null(observed_data) && nrow(observed_data) > 0) {
            # Store original row count
            orig_obs_rows <- nrow(observed_data)
            
            # Filter by dose if Dose_mg column exists
            if ("Dose_mg" %in% names(observed_data)) {
              cat("DEBUG PLOT: Filtering observed data by dose:", val_res$dose, "\n")
              observed_data <- observed_data %>%
                filter(Dose_mg == val_res$dose)
              cat("DEBUG PLOT: Observed data after dose filter:", nrow(observed_data), "rows (was", orig_obs_rows, ")\n")
            }
            
            # Filter by frequency ONLY if frequency was actually found (not a default)
            if (nrow(observed_data) > 0 && 
                val_res$frequency_found && 
                !is.na(val_res$frequency) &&
                ("Frequency" %in% names(observed_data) || "Dose_Frequency" %in% names(observed_data))) {
              freq_col <- if ("Frequency" %in% names(observed_data)) "Frequency" else "Dose_Frequency"
              cat("DEBUG PLOT: Filtering observed data by", freq_col, ":", val_res$frequency, "\n")
              before_freq_filter <- nrow(observed_data)
              observed_data <- observed_data %>%
                filter(get(freq_col) == val_res$frequency)
              cat("DEBUG PLOT: Observed data after frequency filter:", nrow(observed_data), "rows (was", before_freq_filter, ")\n")
            } else if (nrow(observed_data) > 0 && (!val_res$frequency_found || is.na(val_res$frequency))) {
              cat("DEBUG PLOT: Frequency was not found for this arm, NOT filtering by frequency\n")
            }
            
            # If after filtering we have no data, set to NULL so we don't filter summaries
            if (nrow(observed_data) == 0) {
              cat("DEBUG PLOT: No observed data found matching dose and frequency - will show all simulation summaries\n")
              observed_data <- NULL
            }
          }

          
          # Filter summaries to only include variables with observed data (if available)
          # When observed data exists, filter to show only matching variables
          if (!is.null(observed_data) && nrow(observed_data) > 0 && "label" %in% names(observed_data)) {
            cat("DEBUG PLOT: Filtering summaries to only show variables with observed data\n")
            cat("DEBUG PLOT: Observed labels:", paste(unique(observed_data$label), collapse = ", "), "\n")
            cat("DEBUG PLOT: Summary labels before filter:", paste(unique(summaries$label), collapse = ", "), "\n")
            summaries_filtered <- summaries %>%
              filter(label %in% unique(observed_data$label))
            cat("DEBUG PLOT: Summary labels after filter:", paste(unique(summaries_filtered$label), collapse = ", "), "\n")
            
            # Only use filtered summaries if we actually have matching data
            if (nrow(summaries_filtered) > 0) {
              summaries <- summaries_filtered
              cat("DEBUG PLOT: Using filtered summaries with", nrow(summaries), "rows\n")
            } else {
              cat("DEBUG PLOT: WARNING - No summaries matched observed data labels, showing all simulation summaries instead\n")
            }
          } else {
            cat("DEBUG PLOT: No observed data found or observed_data missing label column, showing all simulation summaries\n")
          }
          
          time_label <- switch(input$time_unit,
            "hours" = "Time (hours)",
            "days" = "Time (days)",
            "weeks" = "Time (weeks)",
            "months" = "Time (months)"
          )
          
          # Debug before plotting
          cat("DEBUG PLOT: Final summaries for plotting:\n")
          cat("DEBUG PLOT: Nrows:", nrow(summaries), "\n")
          cat("DEBUG PLOT: Columns:", paste(names(summaries), collapse = ", "), "\n")
          
          # CRITICAL CHECK: Do we have any data to plot?
          if (is.null(summaries) || nrow(summaries) == 0) {
            cat("DEBUG PLOT: ERROR - No summaries data to plot. Returning empty plot.\n")
            cat("DEBUG PLOT: arm_name:", arm_name, "\n")
            return(ggplotly(
              ggplot() + 
                geom_text(aes(x = 0.5, y = 0.5, label = paste("No simulation data for", arm_name)), 
                         hjust = 0.5, vjust = 0.5) +
                theme_minimal() +
                xlim(0, 1) + ylim(0, 1) +
                theme(axis.title = element_blank(), axis.text = element_blank())
            ))
          }
          
          cat("DEBUG PLOT: Check condition - !all(is.na(lower)):", !all(is.na(summaries$lower)), "\n")
          cat("DEBUG PLOT: Check condition - !all(is.na(upper)):", !all(is.na(summaries$upper)), "\n")
          if ("lower" %in% names(summaries)) {
            cat("DEBUG PLOT: Sample lower (first 3):", paste(head(summaries$lower, 3), collapse = ", "), "\n")
            cat("DEBUG PLOT: Sample upper (first 3):", paste(head(summaries$upper, 3), collapse = ", "), "\n")
          } else {
            cat("DEBUG PLOT: WARNING - 'lower' column not found in summaries!\n")
          }
          
          # Build the plot
          g <- ggplot(summaries, aes(x = time, y = median)) +
            geom_line(color = "#337ab7", size = 1, aes(linetype = "Predicted"))
          
          # Add confidence interval ribbon only if CIs are available (n > 1 individual)
          if (!all(is.na(summaries$lower)) && !all(is.na(summaries$upper))) {
            cat("DEBUG PLOT: Adding CI ribbon (calculated from", n_individuals, "individuals)\n")
            g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#337ab7")
          } else {
            cat("DEBUG PLOT: Not adding CI ribbon (n_individuals =", n_individuals, ")\n")
            cat("DEBUG PLOT: Reason - all lower NA?", all(is.na(summaries$lower)), ", all upper NA?", all(is.na(summaries$upper)), "\n")
          }
          
          # Add observed data points if available
          # Only show observed data for labels that exist in summaries
          if (!is.null(observed_data) && nrow(observed_data) > 0) {
            # Filter observed data to only labels present in summaries
            observed_data_to_plot <- observed_data %>%
              filter(label %in% unique(summaries$label))
            
            cat("DEBUG PLOT: Observed data to plot:", nrow(observed_data_to_plot), "rows (filtered from", nrow(observed_data), ")\n")
            
            if (nrow(observed_data_to_plot) > 0) {
              g <- g + geom_point(data = observed_data_to_plot, 
                               aes(x = Time, y = observed_value, color = "Observed"),
                               size = 3, shape = 16, alpha = 0.7)
            } else {
              cat("DEBUG PLOT: No observed data matched summaries labels\n")
            }
          }
          
          g <- g +
            facet_wrap(~ label, scales = "free_y") +
            labs(
              title = arm_name,
              x = time_label,
              y = "Value",
              color = "Data Source",
              linetype = "Data Source"
            ) +
            scale_color_manual(values = c("Observed" = "#e74c3c")) +
            scale_linetype_manual(values = c("Predicted" = "solid")) +
            theme_bw() +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
              legend.position = "bottom"
            )
          
          ggplotly(g)
        })
      })
    }
  })
  
  # Display validation results
  # Render validation type selector - separated from results to prevent reset on re-render
  output$validation_selector <- renderUI({
    # Check which validation types are available across all models
    has_internal <- any(sapply(models_metadata, function(m) !is.null(m$internal_validation_data)))
    has_external <- any(sapply(models_metadata, function(m) !is.null(m$external_validation_data)))
    
    if (!has_internal && !has_external) {
      return(NULL)
    }
    
    # Only show selector if both types are available
    if (has_internal && has_external) {
      div(
        style = "margin-bottom: 20px; padding: 15px; background: #f9f9f9; border-radius: 8px;",
        radioButtons(
          "validation_type_selector",
          "Validation Type:",
          choices = c("Internal" = "internal", "External" = "external"),
          selected = "internal",
          inline = TRUE
        )
      )
    }
  })
  
  # Render validation results and plots - depends on validation_results reactiveVal
  output$validation_result <- renderUI({
    val_results <- validation_results()
    
    # Check which validation types are available across all models
    has_internal <- any(sapply(models_metadata, function(m) !is.null(m$internal_validation_data)))
    has_external <- any(sapply(models_metadata, function(m) !is.null(m$external_validation_data)))
    
    if (!has_internal && !has_external) {
      return(div(
        style = "text-align: center; padding: 50px; color: #999;",
        tags$p("No validation data available for this model yet. ")
      ))
    }
    
    if (is.null(val_results) || length(val_results) == 0) {
      return(div(
        style = "text-align: center; padding: 50px; color: #999;",
        if (has_internal && has_external) {
          tags$p("Select a validation type above to view results.")
        } else {
          tags$p("Validation results will appear here automatically based on validation data in model JSON.")
        }
      ))
    }
    
    # Group results by model and study
    model_study_groups <- list()
    for (res_idx in seq_len(length(val_results))) {
      val_res <- val_results[[res_idx]]
      model_name <- val_res$model_name
      study_name <- val_res$study_name %||% val_res$study_id %||% "Unknown Study"
      
      group_key <- paste(model_name, "||", study_name)
      
      if (is.null(model_study_groups[[group_key]])) {
        model_study_groups[[group_key]] <- list(
          model_name = model_name,
          study_name = study_name,
          result_indices = c()
        )
      }
      
      model_study_groups[[group_key]]$result_indices <- c(
        model_study_groups[[group_key]]$result_indices,
        res_idx
      )
    }
    
    # Render grouped results with headings
    result_elements <- list(
      h3("Validation Results", style = "text-align: center;"),
      tags$hr()
    )
    
    for (group_key in names(model_study_groups)) {
      group <- model_study_groups[[group_key]]
      model_name <- group$model_name
      study_name <- group$study_name
      result_indices <- group$result_indices
      
      # Add model heading
      result_elements[[length(result_elements) + 1]] <- 
        h3(model_name, style = "margin-top: 30px; color: #337ab7;")
      
      # Add study heading
      result_elements[[length(result_elements) + 1]] <- 
        h4(study_name, style = "color: #666; font-weight: normal;")
      
      # Add plots for this study
      for (res_idx in result_indices) {
        result_elements[[length(result_elements) + 1]] <- 
          div(
            style = "margin-bottom: 30px; border: 1px solid #ddd; padding: 15px; border-radius: 8px;",
            plotlyOutput(paste0("validation_plot_", res_idx), height = "400px")
          )
      }
    }
    
    tagList(result_elements)
  })
  
  # Observe validation type selector changes
  observeEvent(input$validation_type_selector, {
    cat("DEBUG: Validation type selector changed to:", input$validation_type_selector, "\n")
    validation_type(input$validation_type_selector)
    
    # Update displayed results based on selection
    if (input$validation_type_selector == "internal") {
      cat("DEBUG: Switching to internal validation results\n")
      cat("DEBUG: internal_validation_results length:", length(internal_validation_results()), "\n")
      validation_results(internal_validation_results())
    } else if (input$validation_type_selector == "external") {
      cat("DEBUG: Switching to external validation results\n")
      cat("DEBUG: external_validation_results length:", length(external_validation_results()), "\n")
      validation_results(external_validation_results())
    }
    
    # Ensure metadata is still available
    if (is.null(validation_metadata())) {
      validation_metadata(list(n_studies = 1))
      cat("DEBUG: Reset validation_metadata\n")
    }
  })
  
  # Pre-compute validation results on app initialization
  observeEvent(TRUE, {
    cat("\n========== VALIDATION INITIALIZATION START ==========\n")
    cat("DEBUG: Starting validation simulation on app initialization\n")
    cat("DEBUG: Total models loaded:", n_models, "\n")
    
    # DETAILED DEBUG: Show what's actually in models_metadata
    cat("DEBUG: ===== DETAILED MODELS_METADATA DEBUG =====\n")
    for (i in seq_len(n_models)) {
      meta <- models_metadata[[i]]
      model_name <- tools::file_path_sans_ext(basename(meta$filename))
      cat("DEBUG: Model", i, "filename:", meta$filename, "\n")
      cat("DEBUG:   - model_name:", model_name, "\n")
      cat("DEBUG:   - meta class:", class(meta), "\n")
      cat("DEBUG:   - meta keys:", paste(names(meta), collapse = ", "), "\n")
      cat("DEBUG:   - internal_validation_data class:", class(meta$internal_validation_data), "\n")
      cat("DEBUG:   - internal_validation_data is.null:", is.null(meta$internal_validation_data), "\n")
      if (!is.null(meta$internal_validation_data)) {
        cat("DEBUG:   - internal_validation_data keys:", paste(names(meta$internal_validation_data), collapse = ", "), "\n")
        cat("DEBUG:   - internal_validation_data$studies length:", length(meta$internal_validation_data$studies), "\n")
      }
    }
    cat("DEBUG: ===== END DETAILED DEBUG =====\n")
    
    # Check which models have what validation data
    for (i in seq_len(n_models)) {
      meta <- models_metadata[[i]]
      model_name <- tools::file_path_sans_ext(basename(meta$filename))
      has_int <- !is.null(meta$internal_validation_data)
      has_ext <- !is.null(meta$external_validation_data)
      cat("DEBUG: Model", i, "(", model_name, ") - internal:", has_int, ", external:", has_ext, "\n")
    }
    
    # Check if models have validation data
    has_internal <- any(sapply(models_metadata, function(m) !is.null(m$internal_validation_data)))
    has_external <- any(sapply(models_metadata, function(m) !is.null(m$external_validation_data)))
    
    cat("DEBUG: SUMMARY - has_internal:", has_internal, ", has_external:", has_external, "\n")
    
    if (has_internal) {
      cat("DEBUG: Computing internal validation\n")
      tryCatch({
        run_validation_simulations("internal")
        cat("DEBUG: Internal validation completed. Length:", length(internal_validation_results()), "\n")
      }, error = function(e) {
        cat("DEBUG: Error in internal validation:", e$message, "\n")
        cat("DEBUG: Error traceback:\n")
        print(e)
      })
    }
    
    if (has_external) {
      cat("DEBUG: Computing external validation\n")
      tryCatch({
        run_validation_simulations("external")
        cat("DEBUG: External validation completed. Length:", length(external_validation_results()), "\n")
      }, error = function(e) {
        cat("DEBUG: Error in external validation:", e$message, "\n")
        cat("DEBUG: Error traceback:\n")
        print(e)
      })
    }
    
    if (!has_internal && !has_external) {
      cat("DEBUG: No validation data found in any models\n")
    }
    
    # Ensure internal validation is displayed by default
    if (has_internal) {
      cat("DEBUG: Setting initial display to internal validation\n")
      validation_results(internal_validation_results())
      cat("DEBUG: validation_results set, length:", length(validation_results()), "\n")
    } else if (has_external) {
      cat("DEBUG: Setting initial display to external validation (internal not available)\n")
      validation_results(external_validation_results())
      cat("DEBUG: validation_results set, length:", length(validation_results()), "\n")
    }
    
    cat("DEBUG: Initialization complete. validation_results length:", length(validation_results()), "\n")
    cat("========== VALIDATION INITIALIZATION END ==========\n\n")
  }, once = TRUE)
}