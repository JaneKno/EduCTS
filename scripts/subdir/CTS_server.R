library(shiny)
library(mrgsolve)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(jsonlite)
library(plotly)
library(readr)
library(digest)
library(googlesheets4)
library(googledrive)

gs4_auth(
  path = "metal-direction-483919-t3-336a646db148.json",
  email = "test-511@metal-direction-483919-t3.iam.gserviceaccount.com",
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = FALSE  # always generate fresh JWT from JSON key, never read stale cached token
)

models_dir <- "./models/" # Adjust path as needed

# ========== GOOGLE SHEETS VALIDATION CACHE ==========
# Single shared spreadsheet — created manually and shared with the service account.
# Service account only adds/replaces tabs; never creates files (no Drive quota used).
SHEETS_CACHE_SS_ID <- "1vxl7vp2RHBqOtE9nAixuRmXVUeXqPJcCDZB4DKWA-rU"

# Tab name: safe_name__study_folder__study_id__arm_N (max 100 chars, Sheets limit)
.sheets_tab_name <- function(safe_name, study_folder_name, study_id, arm_i) {
  key <- paste(safe_name, study_folder_name, study_id, paste0("arm_", arm_i), sep = "__")
  if (nchar(key) > 99) key <- paste0(substr(key, 1, 85), "__", substr(digest::digest(key), 1, 10))
  key
}

sheets_cache_upload <- function(safe_name, summaries_df, tab_name) {
  tryCatch({
    existing <- googlesheets4::sheet_names(SHEETS_CACHE_SS_ID)
    if (tab_name %in% existing) googlesheets4::sheet_delete(SHEETS_CACHE_SS_ID, tab_name)
    googlesheets4::write_sheet(summaries_df, ss = SHEETS_CACHE_SS_ID, sheet = tab_name)
  }, error = function(e) { NULL })
}

sheets_cache_download <- function(safe_name, tab_name) {
  tryCatch({
    existing <- googlesheets4::sheet_names(SHEETS_CACHE_SS_ID)
    if (!tab_name %in% existing) return(NULL)
    df <- googlesheets4::read_sheet(SHEETS_CACHE_SS_ID, sheet = tab_name)
    df
  }, error = function(e) { NULL })
}
# =======================================================

# ========== FEEDBACK COLLECTION SHEET ==========
FEEDBACK_SHEET_ID <- "1VUFjkXOSfjfcaCHAbahs0oNXywzNy7okuj9_jf6AQng"

feedback_sheet_upload <- function(feedback_df) {
  tryCatch({
    googlesheets4::sheet_append(feedback_df, ss = FEEDBACK_SHEET_ID, sheet = 1)
    cat("Feedback saved successfully\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error saving feedback:", as.character(e), "\n")
    return(FALSE)
  })
}
# ====================================================

# Modify the main function
generate_input_dataset <- function(treatment_groups, study_length_weeks, design = "parallel", washout_weeks = 4, model_time_unit, input_compartment = "DEPOT", dose_unit = "mg", model_dose_unit = "mg", molecular_weight_Da = NULL) {
    # Validate input columns and design parameter
    # Check if either Frequency or DosingDays is present
    has_frequency <- "Frequency" %in% colnames(treatment_groups)
    has_dosing_days <- "DosingDays" %in% colnames(treatment_groups)
    
    required_columns <- c("GroupName", "SampleSize", "Treatment", "Dose")
    if (!all(required_columns %in% colnames(treatment_groups))) {
        stop("Dataset must contain: ", paste(required_columns, collapse = ", "))
    }
    
    # Ensure at least one dosing specification method is provided
    if (!has_frequency && !has_dosing_days) {
        stop("Dataset must contain either 'Frequency' or 'DosingDays' column")
    }
    
    if (!design %in% c("parallel", "cross-over", "factorial")) {
        stop("Design must be either 'parallel', 'cross-over', or 'factorial'")
    }
    # Calculate study length in hours once
    study_length_model_unit <- convert_time(study_length_weeks * 7 * 24, "hours", model_time_unit, model_time_unit)
    washout_length_model_unit <- convert_time(washout_weeks * 7 * 24, "hours", model_time_unit, model_time_unit)
    if (design == "parallel") {
        # Original parallel design code
      # Check which method to use for dosing: DosingDays or Frequency+Addl
      # If DosingDays is present, use it; otherwise fall back to Frequency+Addl
      
      if (has_dosing_days && any(!is.na(treatment_groups$DosingDays))) {
        # ===== DOSING_DAYS METHOD =====
        # Generate dosing records for specific days
        input_data <- treatment_groups %>%
          group_by(GroupName, Compartment) %>%
          mutate(ID = list(1:SampleSize)) %>%
          unnest(ID) %>%
          ungroup() %>%
          # Expand rows for each dosing day
          purrr::pmap_df(function(...) {
            row <- tibble(...)
            dosing_days_str <- row$DosingDays
            
            # Parse dosing_days: handle both comma-separated strings and vectors
            dosing_days <- tryCatch({
              if (is.character(dosing_days_str) && !is.na(dosing_days_str)) {
                # Parse comma-separated string like "1,90,270,450"
                dosing_vector <- as.numeric(strsplit(as.character(dosing_days_str), ",")[[1]])
                dosing_vector <- dosing_vector[!is.na(dosing_vector)]
              } else {
                as.numeric(dosing_days_str)
              }
            }, error = function(e) {
              c()
            })
            
            # Create a row for each dosing day
            if (length(dosing_days) > 0) {
              tibble(
                GroupName = rep(row$GroupName, length(dosing_days)),
                Compartment = rep(row$Compartment, length(dosing_days)),
                ID = rep(row$ID, length(dosing_days)),
                cmt = rep(row$Compartment, length(dosing_days)),
                time = convert_time(dosing_days, "days", model_time_unit, model_time_unit),
                amt = ifelse(is.na(row$Dose) | row$Treatment %in% c("placebo", "Placebo"), 0,
                            convert_dose(row$Dose, dose_unit, model_dose_unit, molecular_weight_Da)),
                rate = 0,
                evid = 1,
                ss = 0,
                ii = NA_real_,
                addl = 0,
                Period = 1,
                SEQ = 1,
                SampleSize = row$SampleSize,
                Treatment = row$Treatment,
                Dose = row$Dose
              )
            } else {
              # Empty dosing_days - return single row at time 0
              tibble(
                GroupName = row$GroupName,
                Compartment = row$Compartment,
                ID = row$ID,
                cmt = row$Compartment,
                time = 0,
                amt = ifelse(is.na(row$Dose) | row$Treatment %in% c("placebo", "Placebo"), 0,
                            convert_dose(row$Dose, dose_unit, model_dose_unit, molecular_weight_Da)),
                rate = 0,
                evid = 1,
                ss = 0,
                ii = NA_real_,
                addl = 0,
                Period = 1,
                SEQ = 1,
                SampleSize = row$SampleSize,
                Treatment = row$Treatment,
                Dose = row$Dose
              )
            }
          }) %>%
          select(-SampleSize, -Treatment, -Dose) %>%
          arrange(GroupName, ID, cmt, time)
      } else {
        # ===== FREQUENCY+ADDL METHOD (Original) =====
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
                         convert_dose(Dose, dose_unit, model_dose_unit, molecular_weight_Da)),
            rate = 0,
            evid = 1,
            ss = 0,
            Period = 1,
            ii = case_when(
              Frequency == "Twice daily" ~ convert_time(12, "hours", model_time_unit, model_time_unit),
              Frequency == "Daily" ~ convert_time(24, "hours", model_time_unit, model_time_unit),
              Frequency == "Weekly" ~ convert_time(168, "hours", model_time_unit, model_time_unit),
              Frequency == "Biweekly" ~ convert_time(336, "hours", model_time_unit, model_time_unit),
              Frequency == "Every 2 weeks" ~ convert_time(336, "hours", model_time_unit, model_time_unit),
              Frequency == "Every 4 weeks" ~ convert_time(672, "hours", model_time_unit, model_time_unit),
              Frequency == "Monthly" ~ convert_time(720, "hours", model_time_unit, model_time_unit),
              Frequency == "Once every 3 months" ~ convert_time(2160, "hours", model_time_unit, model_time_unit),
              Frequency == "Once every 4 months" ~ convert_time(2880, "hours", model_time_unit, model_time_unit),
              Frequency == "Once every 6 months" ~ convert_time(4320, "hours", model_time_unit, model_time_unit),
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
          ) %>% 
          arrange(GroupName, ID, cmt, time)
      }
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
                    Frequency == "Every 2 weeks" ~ convert_time(336, "hours", model_time_unit, model_time_unit),
                    Frequency == "Every 4 weeks" ~ convert_time(672, "hours", model_time_unit, model_time_unit),
                    Frequency == "Monthly" ~ convert_time(720, "hours", model_time_unit, model_time_unit),
                    Frequency == "Once every 3 months" ~ convert_time(2160, "hours", model_time_unit, model_time_unit),
                    Frequency == "Once every 4 months" ~ convert_time(2880, "hours", model_time_unit, model_time_unit),
                    Frequency == "Once every 6 months" ~ convert_time(4320, "hours", model_time_unit, model_time_unit),
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
              amt = ifelse(Sequence == "Placebo", 0, convert_dose(Dose, dose_unit, model_dose_unit, molecular_weight_Da)),
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
                    amt = convert_dose(treatment_groups$Dose[treatment_groups$Treatment == .x], dose_unit, model_dose_unit, molecular_weight_Da)
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
                    "Every 2 weeks" = convert_time(336, "hours", model_time_unit, model_time_unit),
                    "Every 4 weeks" = convert_time(672, "hours", model_time_unit, model_time_unit),
                    "Monthly" = convert_time(720, "hours", model_time_unit, model_time_unit),
                    "Once every 3 months" = convert_time(2160, "hours", model_time_unit, model_time_unit),
                    "Once every 4 months" = convert_time(2880, "hours", model_time_unit, model_time_unit),
                    "Once every 6 months" = convert_time(4320, "hours", model_time_unit, model_time_unit),
                    "Every 8 weeks" = convert_time(1344, "hours", model_time_unit, model_time_unit)
                ),
            TRUE ~ first(treatment_groups$Frequency) %>%
                recode(
                    "Twice daily" = convert_time(12, "hours", model_time_unit, model_time_unit),
                    "Daily" = convert_time(24, "hours", model_time_unit, model_time_unit),
                    "Weekly" = convert_time(168, "hours", model_time_unit, model_time_unit),
                    "Biweekly" = convert_time(336, "hours", model_time_unit, model_time_unit),
                    "Every 2 weeks" = convert_time(336, "hours", model_time_unit, model_time_unit),
                    "Every 4 weeks" = convert_time(672, "hours", model_time_unit, model_time_unit),
                    "Monthly" = convert_time(720, "hours", model_time_unit, model_time_unit),
                    "Once every 3 months" = convert_time(2160, "hours", model_time_unit, model_time_unit),
                    "Once every 4 months" = convert_time(2880, "hours", model_time_unit, model_time_unit),
                    "Once every 6 months" = convert_time(4320, "hours", model_time_unit, model_time_unit),
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
# Supports mass-only (mg → ug etc.) and mass ↔ molar (mg → nmol) conversions.
# For mass ↔ molar, molecular_weight_Da must be supplied.
convert_dose <- function(dose, from_unit, to_unit, molecular_weight_Da = NULL) {
  # Handle case where units are the same or missing
  if (is.na(from_unit) || is.na(to_unit) || from_unit == to_unit) {
    return(dose)
  }

  # Normalize units to lowercase
  from_unit <- tolower(trimws(from_unit))
  to_unit   <- tolower(trimws(to_unit))

  if (from_unit == to_unit) return(dose)

  # Unit lookup tables
  mass_scale <- list(
    "g" = 1, "gram" = 1, "grams" = 1,
    "mg" = 1e-3, "milligram" = 1e-3, "milligrams" = 1e-3,
    "ug" = 1e-6, "mcg" = 1e-6, "microgram" = 1e-6, "micrograms" = 1e-6,
    "ng" = 1e-9, "nanogram" = 1e-9, "nanograms" = 1e-9,
    "kg" = 1e3,  "kilogram" = 1e3,  "kilograms" = 1e3
  )
  mol_scale <- list(
    "mol" = 1, "mmol" = 1e-3, "umol" = 1e-6, "nmol" = 1e-9, "pmol" = 1e-12
  )

  from_is_mass  <- from_unit %in% names(mass_scale)
  from_is_molar <- from_unit %in% names(mol_scale)
  to_is_mass    <- to_unit   %in% names(mass_scale)
  to_is_molar   <- to_unit   %in% names(mol_scale)

  if (!from_is_mass && !from_is_molar)
    stop("Unsupported dose unit: '", from_unit, "'")
  if (!to_is_mass && !to_is_molar)
    stop("Unsupported dose unit: '", to_unit, "'")

  # Mass ↔ molar: requires molecular weight
  if (from_is_mass && to_is_molar) {
    if (is.null(molecular_weight_Da) || is.na(molecular_weight_Da))
      stop("molecular_weight_Da is required for mass → molar dose conversion (",
           from_unit, " → ", to_unit, ")")
    dose_g   <- dose * mass_scale[[from_unit]]          # → grams
    dose_mol <- dose_g / molecular_weight_Da            # → mol  (Da = g/mol)
    return(dose_mol / mol_scale[[to_unit]])             # → target molar unit
  }

  if (from_is_molar && to_is_mass) {
    if (is.null(molecular_weight_Da) || is.na(molecular_weight_Da))
      stop("molecular_weight_Da is required for molar → mass dose conversion (",
           from_unit, " → ", to_unit, ")")
    dose_mol <- dose * mol_scale[[from_unit]]           # → mol
    dose_g   <- dose_mol * molecular_weight_Da          # → grams
    return(dose_g / mass_scale[[to_unit]])              # → target mass unit
  }

  # Pure mass ↔ mass conversion
  if (from_is_mass && to_is_mass) {
    return(dose * mass_scale[[from_unit]] / mass_scale[[to_unit]])
  }

  # Pure molar ↔ molar conversion
  return(dose * mol_scale[[from_unit]] / mol_scale[[to_unit]])
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

# ========== DOSE-RESPONSE SUMMARY HELPER ==========
# Collapses per-subject time-series into per-arm endpoint summaries keyed by dose.
compute_dose_response_summaries <- function(results_df, treatment_groups_df, output_vars, output_labels) {
  # One dose value per group (first compartment row)
  dose_per_group <- treatment_groups_df %>%
    group_by(GroupName) %>%
    summarise(dose = first(Dose), .groups = "drop")

  # Exclude plasma/serum concentration variables — not meaningful on a dose-response axis
  conc_pattern <- "conc|plasma|serum|Cp|Cc|C_plasma|C_central|AUC"
  pd_vars   <- output_vars[!grepl(conc_pattern, output_vars,   ignore.case = TRUE)]
  pd_labels <- output_labels[!grepl(conc_pattern, output_vars, ignore.case = TRUE)]

  # Fall back to all vars if everything got filtered
  if (length(pd_vars) == 0) { pd_vars <- output_vars; pd_labels <- output_labels }

  # Use last simulated timepoint as the endpoint
  last_time <- max(results_df$time, na.rm = TRUE)
  endpoint_df <- results_df %>% filter(time == last_time)

  purrr::map_dfr(seq_along(pd_vars), function(j) {
    var <- pd_vars[j]
    lbl <- if (j <= length(pd_labels)) pd_labels[j] else var
    if (!var %in% colnames(endpoint_df)) return(NULL)
    endpoint_df %>%
      group_by(GroupName) %>%
      summarise(
        median = median(.data[[var]], na.rm = TRUE),
        lower  = quantile(.data[[var]], 0.05, na.rm = TRUE),
        upper  = quantile(.data[[var]], 0.95, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(label = lbl, variable = var)
  }) %>%
    left_join(dose_per_group, by = "GroupName")
}
# ====================================================

server <- function(input, output, session) {
  # Load global.R to get model_filenames (set by question-first workflow)
  source("scripts/subdir/global.R", local = TRUE)
  if (!exists("trial_presets")) trial_presets <- list()

  # Determine if single or multiple models are selected
  if (!exists("model_filenames")) {
    stop("No model filenames specified in global.R")
  }
  
  # Ensure model_filenames is a character vector
  model_filenames <- as.character(model_filenames)
  n_models <- length(model_filenames)
  
  # Load metadata for ALL models
  models_metadata <- lapply(model_filenames, function(fname) {
    json_path <- file.path(models_dir, sub("\\.cpp$", ".json", fname))
    full_json <- NULL  # Store full JSON for validation data
    
    meta <- list(
      filename = fname,
      display_name = NULL,
      description = NULL,
      source = NULL,
      clinical_application = NULL,
      author = NULL,
      year = NULL,
      model_time_unit = "hours",
      input_compartment = "CENTRAL",
      input_labels = "Drug",
      dose_unit = "mg",
      model_dose_unit = "mg",  # Unit expected by the model
      output_var = "DV",
      output_label = "Concentration",
      therapeutic_dose = NA,
      therapeutic_frequency = NA,
      compound = NULL,
      molecular_weight_Da = NULL,
      internal_validation_data = NULL,
      external_validation_data = NULL,
      validation_data = NULL  # For backward compatibility
    )
    
    if (file.exists(json_path)) {
      full_json <- fromJSON(json_path, simplifyDataFrame = FALSE)
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
      
      # If therapeutic_dose/frequency not found in model_information, try compounds array
      if (is.na(meta$therapeutic_dose) && !is.null(full_json$compounds) && length(full_json$compounds) > 0) {
        first_compound <- full_json$compounds[[1]]
        if (!is.null(first_compound$therapeutic_dose)) {
          dose_val <- first_compound$therapeutic_dose
          if (!is.na(dose_val) && dose_val != "NA" && dose_val != "N/A") {
            meta$therapeutic_dose <- as.numeric(dose_val)
          }
        }
      }
      
      if (is.na(meta$therapeutic_frequency) && !is.null(full_json$compounds) && length(full_json$compounds) > 0) {
        first_compound <- full_json$compounds[[1]]
        if (!is.null(first_compound$therapeutic_frequency)) {
          freq_val <- first_compound$therapeutic_frequency
          if (!is.na(freq_val) && freq_val != "NA" && freq_val != "N/A") {
            meta$therapeutic_frequency <- freq_val
          }
        }
      }

      # Extract molecular weight from compounds array (used for mass ↔ molar dose conversion)
      if (is.null(meta$molecular_weight_Da) && !is.null(full_json$compounds) && length(full_json$compounds) > 0) {
        first_compound <- full_json$compounds[[1]]
        if (!is.null(first_compound$molecular_weight_Da)) {
          meta$molecular_weight_Da <- as.numeric(first_compound$molecular_weight_Da)
        }
      }

      # Extract display/context fields for UI rendering
      if (!is.null(json_meta$display_name))          meta$display_name          <- json_meta$display_name
      if (!is.null(json_meta$description))           meta$description           <- json_meta$description
      if (!is.null(json_meta$source))                meta$source                <- json_meta$source
      if (!is.null(json_meta$clinical_application))  meta$clinical_application  <- json_meta$clinical_application
      if (!is.null(json_meta$author))                meta$author                <- json_meta$author
      if (!is.null(json_meta$year))                  meta$year                  <- json_meta$year
      if (!is.null(json_meta$compound))              meta$compound              <- json_meta$compound

      # Load internal and external validation data if available
      if (!is.null(full_json$internal_validation_data)) {
        meta$internal_validation_data <- full_json$internal_validation_data
        # For backward compatibility, use internal as default validation_data
        meta$validation_data <- full_json$internal_validation_data
      }
      
      if (!is.null(full_json$external_validation_data)) {
        meta$external_validation_data <- full_json$external_validation_data
      }
    }
    
    meta
  })

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
  auto_run_trigger <- reactiveVal(FALSE)
  comparison_mode  <- reactiveVal(FALSE)
  shared_vars_list <- reactiveVal(NULL)
  # Clinical question type from question-first workflow — read cts_mode written by ModelLibrary.R
  question_type <- reactiveVal(if (!is.null(trial_presets$cts_mode)) trial_presets$cts_mode else "")
  # Sidebar visibility state — initially hidden if auto_run (dose-response/comparison modes)
  sidebar_visible <- reactiveVal(!isTRUE(trial_presets$auto_run))

  # ========== Ensure sidebar starts in the correct initial state ==========
  session$onFlushed(function() {
    if (isTRUE(trial_presets$auto_run)) {
      # Hide sidebar and expand main content
      shinyjs::addClass("cts_sidebar_col", "sidebar-hidden")
      shinyjs::addClass("cts_main_col", "expanded")
    } else {
      # Show sidebar and keep main content normal
      shinyjs::removeClass("cts_sidebar_col", "sidebar-hidden")
      shinyjs::removeClass("cts_main_col", "expanded")
    }
  }, once = TRUE)
  # ========================================================================

  # Reactive value to store combined treatment switch results for custom mapping
  treatment_switch_combined <- reactiveVal(NULL)
  
  # Reactive value for feedback submission status
  feedback_submitted <- reactiveVal(FALSE)
  
  # Reactive value to store the number of treatment groups
  treatment_groups <- reactiveValues()
  
  # ========== INITIALIZE treatment group counts for EACH model ==========
  # For dose-response questions, set count immediately from preset n_arms so dosing_ui
  # renders the correct number of groups on first paint (before onFlushed fires).
  initial_arm_count <- if (isTRUE(trial_presets$cts_mode == "dose_response") && !is.null(trial_presets$n_arms))
    as.integer(trial_presets$n_arms) else 1L
  for (i in seq_len(n_models)) {
    treatment_groups[[paste0("model_", i, "_count")]] <- initial_arm_count
  }
  # ======================================================================
  
  # ============================================================================
  # HANDLE TRIAL PRESETS FROM QUESTION-FIRST WORKFLOW
  # ============================================================================
  if (!is.null(trial_presets) && is.list(trial_presets)) {
    # Pre-fill trial design based on question preset (once, after first flush)
    session$onFlushed(function() {
      # Show info banner with question title
      if (!is.null(trial_presets$question_title)) {
        info_html <- paste0(
          '<div class="alert alert-info alert-dismissible" style="border-left: 4px solid #0275d8; background-color: #d1ecf1; color: #0c5460;">',
          '<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>',
          '<strong>ℹ️ Configured for:</strong> ',
          trial_presets$question_title,
          '</div>'
        )
        shinyjs::html("trial_preset_banner", info_html)
      }

      # For dose-response: set multi-arm count so all groups exist before auto_run fires
      if (isTRUE(trial_presets$cts_mode == "dose_response") && !is.null(trial_presets$n_arms)) {
        for (mi in seq_len(n_models)) {
          treatment_groups[[paste0("model_", mi, "_count")]] <- as.integer(trial_presets$n_arms)
        }
      }

      # Auto-run: schedule a JS click on run_sim after the browser has rendered the CTS UI.
      # shinyjs::delay() is NOT used here because it eval()s the deparsed R expression as
      # raw JavaScript, which breaks when the call uses the shinyjs:: namespace prefix.
      # shinyjs::runjs() with an explicit setTimeout is reliable in all cases.
      # Use 2500ms when dose_response so dosing_ui has time to re-render all arm inputs.
      if (isTRUE(trial_presets$auto_run)) {
        delay_ms <- if (isTRUE(trial_presets$cts_mode == "dose_response")) 2500L else 1500L
        shinyjs::runjs(paste0("setTimeout(function(){ var btn = document.getElementById('run_sim'); if(btn) btn.click(); }, ", delay_ms, ");" ))
      }
    }, once = TRUE)
  }
  # ============================================================================

  model_path <- file.path(models_dir, model_filenames[1])

  # Generate dosing UI based on trial design
  output$dosing_ui <- renderUI({
    cat("\n===== DOSING UI RENDER =====\n")
    cat("trial_design input:", as.character(input$trial_design), "\n")
    cat("n_models:", n_models, "\n")

    trial_design_val <- if (!is.null(input$trial_design)) input$trial_design else
      if (!is.null(trial_presets$trial_design)) trial_presets$trial_design else "parallel"
    cat("trial_design_val:", trial_design_val, "\n")

    if (trial_design_val == "parallel") {
      # Parallel design: Single group
      tagList(
        lapply(seq_len(n_models), function(model_idx) {
        meta <- models_metadata[[model_idx]]
        model_name <- tools::file_path_sans_ext(basename(meta$filename))

        count_raw <- tryCatch(
          treatment_groups[[paste0("model_", model_idx, "_count")]],
          error = function(e) { cat("ERROR reading count:", e$message, "\n"); NULL }
        )
        cat("model_idx:", model_idx, "| count_raw:", as.character(count_raw), "\n")

        {
          n_grps_now <- if (is.null(count_raw) || count_raw < 1L) initial_arm_count else as.integer(count_raw)
          cat("n_grps_now:", n_grps_now, "\n")
          tagList(
          tags$h4(paste("Dosing for Model:", model_name)),
          
          # Treatment groups for this model
          lapply(seq_len(n_grps_now), function(i) {
            tagList(
              tags$h5(paste("Treatment Group", i)),
              textInput(paste0("m", model_idx, "_group_name_", i), "Group Name", 
                       value = if (isTRUE(trial_presets$cts_mode == "comparison") && !is.null(meta$compound))
                                 meta$compound
                               else paste("Group", i)),
              numericInput(paste0("m", model_idx, "_sample_size_", i), "Sample Size", 
                          value = 100, min = 1),
              textInput(paste0("m", model_idx, "_treatment_", i), "Treatment", 
                       value = paste("Drug", i)),
              
              # Dose inputs for each compartment
              lapply(seq_along(meta$input_compartment), function(j) {
                # Base therapeutic dose
                base_dose <- if (!is.na(meta$therapeutic_dose) && as.numeric(meta$therapeutic_dose) > 0)
                  as.numeric(meta$therapeutic_dose) else 10

                # For dose-response mode: assign log-spaced dose per group (same formula as run_sim)
                n_grps <- n_grps_now
                default_dose <- if (isTRUE(trial_presets$cts_mode == "dose_response") && n_grps > 1) {
                  dose_levels <- round(exp(seq(log(base_dose * 0.25), log(base_dose * 4), length.out = n_grps)), 4)
                  dose_levels[i]
                } else {
                  base_dose
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
                      choices = c("Twice daily", "Daily", "Weekly", "Biweekly", "Every 2 weeks", "Every 4 weeks", "Monthly", "Once every 3 months", "Once every 4 months", "Once every 6 months", "Every 8 weeks"),
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
      }})
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
    } else if (trial_design_val == "cross-over") {
      # Cross-over design: Multiple periods
      tagList(
        tags$h4("Dosing Information"),
        numericInput("num_periods", "Number of Periods", value = 2, min = 1),
        numericInput("sample_size", "Sample Size", value = 100, min = 1),
        textInput("treatment", "Treatment", value = "Drug X"),
        numericInput("dose", "Dose (mg)", value = 10, min = 0),
        numericInput("washout_weeks", "Washout Period (weeks)", value = 4, min = 0),
        selectInput("frequency", "Frequency", choices = c("single_dose", "Twice daily", "Daily", "Weekly", "Biweekly", "Every 2 weeks", "Every 4 weeks", "Monthly", "Once every 3 months", "Once every 4 months", "Once every 6 months", "Every 8 weeks"), selected = "Weekly")
      )
    } else if (trial_design_val == "factorial") {
      # Factorial design: Multiple treatments
      tagList(
        tags$h4("Dosing Information"),
        numericInput("num_treatments", "Number of Treatments", value = 2, min = 1),
        numericInput("sample_size", "Sample Size", value = 100, min = 1),
        textInput("treatment_1", "Treatment 1", value = "Drug A"),
        numericInput("dose_1", "Dose for Treatment 1 (mg)", value = 10, min = 0),
        textInput("treatment_2", "Treatment 2", value = "Drug B"),
        numericInput("dose_2", "Dose for Treatment 2 (mg)", value = 10, min = 0),
        selectInput("frequency", "Frequency", choices = c("single_dose", "Twice daily", "Daily", "Weekly", "Biweekly", "Every 2 weeks", "Every 4 weeks", "Monthly", "Once every 3 months", "Once every 4 months", "Once every 6 months", "Every 8 weeks"), selected = "Weekly")
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

  # Prevent Shiny from suspending these outputs when the sidebar is hidden
  # (sidebar starts hidden in auto_run/comparison mode, so without this they never render)
  outputOptions(output, "dosing_ui",  suspendWhenHidden = FALSE)
  outputOptions(output, "model_info", suspendWhenHidden = FALSE)

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

# ========== Reactive values for treatment switch ==========
treatment_switch_settings <- reactiveValues(
  enabled = isTRUE(trial_presets$enable_switch),
  initial_model = 1,
  switch_model = 2,
  switch_time = 12,
  switch_time_unit = "weeks",
  compartment_mappings = NULL,  # Data frame with model compartment mappings (if needed)
  parameter_mappings = NULL,    # Data frame mapping compartments to baseline parameters
  output_mappings = NULL        # Data frame mapping Phase 1 outputs to Phase 2 outputs
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

# ========== Generate Treatment Switch Toggle UI (Parallel + multi-model only) ==========
output$treatment_switch_toggle_ui <- renderUI({
  # Show toggle only if: Parallel trial AND multi-model
  if (input$trial_design != "parallel" || n_models <= 1) {
    return(NULL)
  }
  
  tagList(
    tags$h4("Treatment Switch",
      `data-toggle` = "tooltip",
      `data-placement` = "right",
      title = "Study the effect of switching between two or more compounds during the trial. This allows you to simulate scenarios where patients transition from one treatment to another (e.g., escalation, switch due to efficacy or safety, or sequential therapy)."
    ),
    checkboxInput(
      inputId = "enable_treatment_switch",
      label = HTML("Enable Treatment Switch During Trial <span class='cts-info-icon' data-tooltip='Enable to simulate a two-phase trial where subjects switch from one drug/model to another at a specified timepoint. Useful for evaluating sequential or switching therapies.'>?</span>"),
      value = isTRUE(trial_presets$enable_switch)
    )
  )
})

output$treatment_switch_details_ui <- renderUI({
  # Show details only if: Parallel trial AND treatment switch enabled AND multi-model
  if (input$trial_design != "parallel" || !isTRUE(input$enable_treatment_switch) || n_models <= 1) {
    return(NULL)
  }
  
  tagList(
    tags$h5("Switch Configuration"),
    tags$p("Specify which compounds to switch between and when."),
    
    # Initial treatment selection
    selectInput(
      inputId = "initial_treatment_model",
      label = "Initial Treatment (Model)",
      choices = setNames(1:n_models, sapply(1:n_models, function(i) {
        tools::file_path_sans_ext(basename(models_metadata[[i]]$filename))
      })),
      selected = 1
    ),
    
    # Switched treatment selection
    selectInput(
      inputId = "switch_to_model",
      label = "Switch To (Model)",
      choices = setNames(1:n_models, sapply(1:n_models, function(i) {
        tools::file_path_sans_ext(basename(models_metadata[[i]]$filename))
      })),
      selected = if (n_models > 1) 2 else 1
    ),
    
    # Switch time input with tooltip info icon
    numericInput(
      inputId = "switch_time_value",
      label = tagList(
        "Time of Treatment Switch ",
        tags$i(
          class = "fa fa-info-circle",
          `data-toggle` = "tooltip",
          `data-placement` = "right",
          title = "The simulation will run the initial model until this time point, then use those values as the baseline for the switched model.",
          style = "color: #0275d8; cursor: help; margin-left: 5px;"
        )
      ),
      value = 12,
      min = 0
    ),
    
    # Switch time unit (matches trial time unit)
    selectInput(
      inputId = "switch_time_unit",
      label = "Time Unit",
      choices = c("Hours" = "hours", "Days" = "days", "Weeks" = "weeks", "Months" = "months"),
      selected = "weeks"
    ),
    
    # Baseline Parameter Mappings
    tags$hr(),
    tags$h5("Baseline Parameter Mappings"),
    tags$p("Map Phase 1 compartment values to Phase 2 baseline parameters:"),
    uiOutput("parameter_mappings_suggestions_ui"),
    
    # Output Mappings (auto-derived from parameter mappings)
    tags$hr(),
    tags$h5("Output Mappings"),
    tags$p("Outputs from both models that measure the same physiological entity will be combined across the entire study period:"),
    uiOutput("output_mappings_for_treatment_switch_ui")
  )
})

# Output: Display suggested parameter mappings for treatment switch
output$parameter_mappings_suggestions_ui <- renderUI({
  # Only show if treatment switch is relevant
  if (!isTRUE(input$enable_treatment_switch) || n_models <= 1) {
    return(NULL)
  }
  
  # Get selected models
  initial_idx <- as.numeric(input$initial_treatment_model %||% 1)
  switch_idx <- as.numeric(input$switch_to_model %||% 2)
  
  if (initial_idx < 1 || initial_idx > n_models || 
      switch_idx < 1 || switch_idx > n_models || 
      initial_idx == switch_idx) {
    return(NULL)
  }
  
  # Get metadata for both models
  meta1 <- models_metadata[[initial_idx]]
  meta2 <- models_metadata[[switch_idx]]
  
  # Generate suggestions for compartment → parameter mappings
  suggestions <- suggest_compartment_to_parameter_mappings(meta1, meta2)
  
  if (nrow(suggestions) == 0) {
    # No automatic matches found - show manual mapping interface
    # Load Model 1 to extract ALL compartments (not just input compartments)
    model1_path <- file.path(models_dir, meta1$filename)
    mod1 <- tryCatch({
      mread(model1_path)
    }, error = function(e) {
      NULL
    })
    
    compartments1 <- c()
    if (!is.null(mod1)) {
      tryCatch({
        compartments1 <- mod1@cmtL
      }, error = function(e) {
        compartments1 <- meta1$input_compartment %||% c()
      })
    } else {
      compartments1 <- meta1$input_compartment %||% c()
    }
    
    if (length(compartments1) == 0) {
      return(
        tags$div(
          class = "alert alert-warning",
          "No compartments found in Phase 1 model. Please check model configuration."
        )
      )
    }
    
    # Load Model 2 to extract available parameters and compartments
    model2_path <- file.path(models_dir, meta2$filename)
    mod2 <- tryCatch({
      mread(model2_path)
    }, error = function(e) {
      NULL
    })
    
    # Extract parameters and compartments from Model 2
    model2_params <- c()
    model2_compartments <- c()
    
    if (!is.null(mod2)) {
      # Extract parameter names using param() - convert to data.frame first
      tryCatch({
        params_df <- as.data.frame(param(mod2))
        if (!is.null(params_df)) {
          model2_params <- names(params_df)
        }
      }, error = function(e) {
        NULL
      })
      
      # Get all compartment names from the model using @cmtL
      tryCatch({
        model2_compartments <- mod2@cmtL
      }, error = function(e) {
        # Fallback to input_compartments
        model2_compartments <- meta2$input_compartment %||% c()
      })
    }
    
    # Create manual mapping dropdowns
    manual_mapping_rows <- lapply(seq_along(compartments1), function(comp_idx) {
      comp <- compartments1[comp_idx]
      checkbox_id <- paste0("manual_param_map_enabled_", initial_idx, "_", switch_idx, "_", comp_idx)
      select_id <- paste0("manual_param_map_select_", initial_idx, "_", switch_idx, "_", comp_idx)
      text_id <- paste0("manual_param_map_custom_", initial_idx, "_", switch_idx, "_", comp_idx)
      
      # Build choices: parameters first (highest priority), then compartments
      phase2_choices <- c()
      
      # First priority: Model 2 parameters (extracted via param())
      if (length(model2_params) > 0) {
        phase2_choices <- c(phase2_choices, setNames(model2_params, paste0(model2_params, " [Parameter]")))
      }
      
      # Second: All Model 2 compartments (via comp_names() or fallback)
      if (length(model2_compartments) > 0) {
        phase2_choices <- c(phase2_choices, setNames(model2_compartments, paste0(model2_compartments, " [Compartment]")))
      }
      
      tags$div(
        class = "suggestion-row",
        style = "padding: 10px; margin: 8px 0; border-left: 4px solid #0066cc; background: #f0f8ff; border-radius: 4px;",
        fluidRow(
          column(1,
            checkboxInput(checkbox_id, "", value = FALSE, width = "100%")
          ),
          column(5,
            tags$strong(comp, style = "color: #000;"),
            tags$br(),
            tags$small("Phase 1 Compartment", style = "color: #666;")
          ),
          column(1,
            tags$span("→", style = "text-align: center; font-weight: bold; display: block; color: #000;")
          ),
          column(5,
            tags$div(
              style = "display: flex; flex-direction: column; gap: 5px;",
              selectInput(
                select_id,
                label = "Dropdown",
                choices = if (length(phase2_choices) > 0) c("(Select)" = "", phase2_choices) else c("(No parameters or compartments found)"),
                selected = "",
                width = "100%"
              ),
              tags$small("OR type custom:", style = "color: #666; margin-top: 5px;"),
              textInput(
                text_id,
                label = NULL,
                placeholder = "e.g., BL_ABSORPTION",
                width = "100%"
              )
            )
          )
        )
      )
    })
    
    return(
      tagList(
        tags$div(
          class = "alert alert-primary",
          style = "border: 2px solid #0066cc;",
          tags$strong("Manual Parameter Mappings"),
          tags$p("No automatic matches found. Map Phase 1 compartments to Phase 2 parameters or compartments:"),
          tags$p(
            tags$small(
              "Select from dropdown (Model 2 parameters or compartments) OR type a custom parameter name if needed.",
              style = "color: #555; font-style: italic;"
            )
          ),
          manual_mapping_rows
        ),
        
        # Show reference info
        if (length(model2_params) > 0 || length(model2_compartments) > 0) {
          tagList(
            tags$hr(),
            tags$div(
              class = "alert alert-info",
              style = "margin-top: 15px;",
              tags$strong("Available in Model 2"),
              if (length(model2_params) > 0) {
                tags$div(
                  tags$strong("Parameters:", style = "color: #004085;"),
                  tags$code(
                    style = "display: block; padding: 8px; background: #f5f5f5; border-radius: 4px; color: #333; font-size: 12px; word-break: break-word;",
                    paste(model2_params, collapse = ", ")
                  )
                )
              },
              if (length(model2_compartments) > 0) {
                tags$div(
                  tags$strong("Compartments:", style = "color: #28a745; margin-top: 10px;"),
                  tags$code(
                    style = "display: block; padding: 8px; background: #f5f5f5; border-radius: 4px; color: #333; font-size: 12px; word-break: break-word;",
                    paste(model2_compartments, collapse = ", ")
                  )
                )
              }
            )
          )
        } else {
          NULL
        },
        
        actionButton(
          "apply_manual_parameter_mappings",
          "Apply Manual Mappings",
          class = "btn btn-primary btn-sm",
          style = "margin-top: 10px;"
        )
      )
    )
  }
  
  # Automatic suggestions found - display them
  # Also load Model 2 to show actual parameter info
  model2_path <- file.path(models_dir, meta2$filename)
  mod2 <- tryCatch({
    mread(model2_path)
  }, error = function(e) {
    NULL
  })
  
  model2_params_info <- c()
  model2_compartments_info <- c()
  if (!is.null(mod2)) {
    tryCatch({
      params_df <- as.data.frame(param(mod2))
      if (!is.null(params_df)) {
        model2_params_info <- names(params_df)
      }
    }, error = function(e) {
      NULL
    })
    
    tryCatch({
      model2_compartments_info <- mod2@cmtL
    }, error = function(e) {
      NULL
    })
  }
  
  suggestion_rows <- lapply(1:nrow(suggestions), function(i) {
    suggestion <- suggestions[i, ]
    checkbox_id <- paste0("param_map_", initial_idx, "_", switch_idx, "_", i)
    
    # Check if the suggested parameter actually exists in Model 2
    param_exists <- suggestion$baseline_parameter %in% model2_params_info
    param_status <- if (param_exists) {
      tags$span("✓ Found", style = "color: green; font-weight: bold;")
    } else {
      tags$span("⚠ Not found", style = "color: orange; font-weight: bold;")
    }
    
    tags$div(
      class = "suggestion-row",
      style = "padding: 10px; margin: 8px 0; border-left: 4px solid #28a745; background: #f8f9fa; border-radius: 4px;",
      fluidRow(
        column(1,
          checkboxInput(checkbox_id, "", value = TRUE, width = "100%")
        ),
        column(5,
          tags$strong(suggestion$compartment, style = "color: #000;")
        ),
        column(1,
          tags$span("→", style = "text-align: center; font-weight: bold; display: block; color: #000;")
        ),
        column(5,
          tags$strong(suggestion$baseline_parameter, style = "color: #000;"),
          tags$br(),
          param_status
        )
      ),
      fluidRow(
        column(12,
          tags$small(
            style = "color: #666;",
            paste0("Confidence: ", suggestion$confidence, "% — ", suggestion$reason)
          )
        )
      )
    )
  })
  
  tagList(
    tags$div(
      class = "alert alert-success",
      style = "border: 2px solid #28a745;",
      tags$strong("Suggested Parameter Mappings"),
      tags$p("These outputs will be combined across the entire study period (Phase 1 + Phase 2):"),
      suggestion_rows
    ),
    actionButton(
      "apply_parameter_mappings",
      "Apply Selected Mappings",
      class = "btn btn-success btn-sm",
      style = "margin-top: 10px;"
    ),
    
    # Reference section showing available parameters in Model 2
    if (length(model2_params_info) > 0) {
      tagList(
        tags$hr(),
        tags$div(
          class = "alert alert-info",
          style = "margin-top: 15px;",
          tags$strong("Available Parameters in Model 2"),
          tags$p("These are the parameters available in the switched model:"),
          tags$code(
            style = "display: block; padding: 10px; background: #f5f5f5; border-radius: 4px; color: #333; font-size: 12px;",
            paste(model2_params_info, collapse = ", ")
          )
        )
      )
    } else {
      NULL
    }
  )
})

# Output: Display derived output mappings for treatment switch
output$output_mappings_for_treatment_switch_ui <- renderUI({
  # Only show if treatment switch is enabled and parameter mappings have been applied
  if (!isTRUE(input$enable_treatment_switch) || n_models <= 1) {
    return(NULL)
  }
  
  # Get the stored parameter mappings
  parameter_mappings <- treatment_switch_settings$parameter_mappings
  
  if (is.null(parameter_mappings) || nrow(parameter_mappings) == 0) {
    return(
      tags$div(
        class = "alert alert-secondary",
        "Apply parameter mappings above to see which outputs will be combined across the study period."
      )
    )
  }
  
  # Get selected models
  initial_idx <- as.numeric(input$initial_treatment_model %||% 1)
  switch_idx <- as.numeric(input$switch_to_model %||% 2)
  
  # Get metadata
  meta1 <- models_metadata[[initial_idx]]
  meta2 <- models_metadata[[switch_idx]]
  
  # Derive output mappings from parameter mappings
  output_suggestions <- derive_output_mappings_from_parameter_mappings(parameter_mappings, meta1, meta2)
  
  if (nrow(output_suggestions) == 0) {
    return(
      tags$div(
        class = "alert alert-warning",
        "No output mappings could be auto-derived. Phase-specific outputs will be shown separately."
      )
    )
  }
  
  # Display output mapping suggestions
  output_rows <- lapply(1:nrow(output_suggestions), function(i) {
    sugg <- output_suggestions[i, ]
    checkbox_id <- paste0("output_map_", initial_idx, "_", switch_idx, "_", i)
    
    tags$div(
      class = "suggestion-row",
      style = "padding: 10px; margin: 8px 0; border-left: 4px solid #0066cc; background: #f0f7ff; border-radius: 4px;",
      fluidRow(
        column(1,
          checkboxInput(checkbox_id, "", value = TRUE, width = "100%")
        ),
        column(5,
          tags$strong(sugg$phase1_output_label, style = "color: #000;"),
          tags$br(),
          tags$small(paste0("Model 1: ", sugg$phase1_output), style = "color: #666;")
        ),
        column(1,
          tags$span("⟷", style = "text-align: center; font-weight: bold; display: block; color: #000;")
        ),
        column(5,
          tags$strong(sugg$phase2_output_label, style = "color: #000;"),
          tags$br(),
          tags$small(paste0("Model 2: ", sugg$phase2_output), style = "color: #666;")
        )
      ),
      fluidRow(
        column(12,
          tags$small(
            style = "color: #666;",
            paste0("Confidence: ", sugg$confidence, "% | Compartment: ", sugg$mapped_compartment)
          )
        )
      )
    )
  })
  
  tagList(
    tags$div(
      class = "alert alert-success",
      style = "border: 2px solid #28a745;",
      tags$strong("Auto-Derived Output Mappings"),
      tags$p("These outputs will be combined across the entire study period (Phase 1 + Phase 2):"),
      output_rows
    ),
    actionButton(
      "apply_output_mappings",
      "Apply Selected Output Mappings",
      class = "btn btn-success btn-sm",
      style = "margin-top: 10px;"
    )
  )
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

# ========== Observers for Treatment Switch Settings ==========
observeEvent(input$enable_treatment_switch, {
  treatment_switch_settings$enabled <- input$enable_treatment_switch
})

observeEvent(input$initial_treatment_model, {
  treatment_switch_settings$initial_model <- as.numeric(input$initial_treatment_model)
})

observeEvent(input$switch_to_model, {
  treatment_switch_settings$switch_model <- as.numeric(input$switch_to_model)
})

observeEvent(input$switch_time_value, {
  treatment_switch_settings$switch_time <- input$switch_time_value
})

observeEvent(input$switch_time_unit, {
  treatment_switch_settings$switch_time_unit <- input$switch_time_unit
})

# Handle applying parameter mappings from suggestions
observeEvent(input$apply_parameter_mappings, {
  initial_idx <- as.numeric(input$initial_treatment_model %||% 1)
  switch_idx <- as.numeric(input$switch_to_model %||% 2)
  
  if (initial_idx < 1 || initial_idx > n_models || 
      switch_idx < 1 || switch_idx > n_models) {
    return()
  }
  
  # Get suggestions
  meta1 <- models_metadata[[initial_idx]]
  meta2 <- models_metadata[[switch_idx]]
  suggestions <- suggest_compartment_to_parameter_mappings(meta1, meta2)
  
  if (nrow(suggestions) == 0) {
    return()
  }
  
  # Collect selected mappings
  selected_mappings <- list()
  for (i in 1:nrow(suggestions)) {
    checkbox_id <- paste0("param_map_", initial_idx, "_", switch_idx, "_", i)
    if (isTRUE(input[[checkbox_id]])) {
      selected_mappings[[i]] <- suggestions[i, ]
    }
  }
  
  # Combine selected mappings into a data frame
  # Filter out NULL entries (unselected mappings)
  selected_mappings <- Filter(Negate(is.null), selected_mappings)
  
  if (length(selected_mappings) > 0) {
    mapping_df <- do.call(rbind, selected_mappings)
    rownames(mapping_df) <- NULL
    treatment_switch_settings$parameter_mappings <- mapping_df
    
    # Auto-derive output mappings and immediately create combined plot entry.
    # Using exact intersection of output_var (not fuzzy matching) so this always works.
    shared_outs <- intersect(meta1$output_var, meta2$output_var)
    if (length(shared_outs) > 0) {
      auto_om <- data.frame(phase1_output = shared_outs, phase2_output = shared_outs,
                            stringsAsFactors = FALSE)
    } else {
      auto_om <- derive_output_mappings_from_parameter_mappings(mapping_df, meta1, meta2)
    }
    treatment_switch_settings$output_mappings <- auto_om
    
    if (nrow(auto_om) > 0) {
      new_id <- output_mappings$next_id
      output_mappings$mappings[[new_id]] <- list(
        created = TRUE,
        treatment_switch_auto = TRUE,
        output_mapping_df = auto_om
      )
      output_mappings$count <- output_mappings$count + 1
      output_mappings$next_id <- output_mappings$next_id + 1
    }
    
  } else {
    treatment_switch_settings$parameter_mappings <- NULL
    treatment_switch_settings$output_mappings <- NULL
  }
})

# Handle applying output mappings from suggestions
observeEvent(input$apply_manual_parameter_mappings, {
  initial_idx <- as.numeric(input$initial_treatment_model %||% 1)
  switch_idx <- as.numeric(input$switch_to_model %||% 2)
  
  if (initial_idx < 1 || initial_idx > n_models || 
      switch_idx < 1 || switch_idx > n_models) {
    return()
  }
  
  # Get metadata
  meta1 <- models_metadata[[initial_idx]]
  meta2 <- models_metadata[[switch_idx]]
  
  # Load Model 1 to get ALL compartments (same as in UI)
  model1_path <- file.path(models_dir, meta1$filename)
  
  mod1 <- tryCatch({
    mread(model1_path)
  }, error = function(e) {
    NULL
  })
  
  compartments1 <- c()
  if (!is.null(mod1)) {
    tryCatch({
      compartments1 <- mod1@cmtL
    }, error = function(e) {
      compartments1 <- meta1$input_compartment %||% c()
    })
  } else {
    compartments1 <- meta1$input_compartment %||% c()
  }
  
  # Collect manually selected mappings
  manual_mappings <- list()
  
  for (comp_idx in seq_along(compartments1)) {
    comp <- compartments1[comp_idx]
    checkbox_id <- paste0("manual_param_map_enabled_", initial_idx, "_", switch_idx, "_", comp_idx)
    select_id <- paste0("manual_param_map_select_", initial_idx, "_", switch_idx, "_", comp_idx)
    text_id <- paste0("manual_param_map_custom_", initial_idx, "_", switch_idx, "_", comp_idx)
    
    # Check if user enabled this mapping
    if (isTRUE(input[[checkbox_id]])) {
      # Prefer custom text input if provided, otherwise use dropdown
      custom_text <- input[[text_id]]
      selected_output <- input[[select_id]]
      
      # Use custom text if not empty, otherwise use dropdown selection
      target_param <- NA
      if (!is.null(custom_text) && custom_text != "") {
        target_param <- custom_text
      } else if (!is.null(selected_output) && selected_output != "") {
        target_param <- selected_output
      }
      
      # Only add if target parameter was provided
      if (!is.na(target_param) && target_param != "") {
        manual_mappings[[length(manual_mappings) + 1]] <- data.frame(
          compartment = comp,
          baseline_parameter = target_param,
          confidence = 100,
          reason = "User-selected mapping",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(manual_mappings) > 0) {
    mapping_df <- do.call(rbind, manual_mappings)
    rownames(mapping_df) <- NULL
    treatment_switch_settings$parameter_mappings <- mapping_df
    
    # Auto-derive output mappings and immediately create combined plot entry.
    shared_outs <- intersect(meta1$output_var, meta2$output_var)
    if (length(shared_outs) > 0) {
      auto_om <- data.frame(phase1_output = shared_outs, phase2_output = shared_outs,
                            stringsAsFactors = FALSE)
    } else {
      auto_om <- derive_output_mappings_from_parameter_mappings(mapping_df, meta1, meta2)
    }
    treatment_switch_settings$output_mappings <- auto_om
    
    if (nrow(auto_om) > 0) {
      new_id <- output_mappings$next_id
      output_mappings$mappings[[new_id]] <- list(
        created = TRUE,
        treatment_switch_auto = TRUE,
        output_mapping_df = auto_om
      )
      output_mappings$count <- output_mappings$count + 1
      output_mappings$next_id <- output_mappings$next_id + 1
    }
    
  } else {
    treatment_switch_settings$parameter_mappings <- NULL
    treatment_switch_settings$output_mappings <- NULL
  }
})

# Handle applying output mappings from suggestions
observeEvent(input$apply_output_mappings, {
  initial_idx <- as.numeric(input$initial_treatment_model %||% 1)
  switch_idx <- as.numeric(input$switch_to_model %||% 2)
  
  if (initial_idx < 1 || initial_idx > n_models || 
      switch_idx < 1 || switch_idx > n_models) {
    return()
  }
  
  # Get the auto-derived output mappings
  output_suggestions <- treatment_switch_settings$output_mappings
  
  if (is.null(output_suggestions) || nrow(output_suggestions) == 0) {
    return()
  }
  
  # Collect selected output mappings
  selected_output_mappings <- list()
  for (i in 1:nrow(output_suggestions)) {
    checkbox_id <- paste0("output_map_", initial_idx, "_", switch_idx, "_", i)
    if (isTRUE(input[[checkbox_id]])) {
      selected_output_mappings[[i]] <- output_suggestions[i, ]
    }
  }
  
  # Combine selected output mappings
  selected_output_mappings <- Filter(Negate(is.null), selected_output_mappings)
  
  if (length(selected_output_mappings) > 0) {
    output_mapping_df <- do.call(rbind, selected_output_mappings)
    rownames(output_mapping_df) <- NULL
    treatment_switch_settings$output_mappings <- output_mapping_df
    
    # Automatically create a custom mapping entry so the plots are rendered
    # This ensures the custom mapping observe block picks it up
    new_id <- output_mappings$next_id
    output_mappings$mappings[[new_id]] <- list(
      created = TRUE,
      treatment_switch_auto = TRUE,
      output_mapping_df = output_mapping_df
    )
    output_mappings$count <- output_mappings$count + 1
    output_mappings$next_id <- output_mappings$next_id + 1
  } else {
    treatment_switch_settings$output_mappings <- NULL
  }
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

# ========== Function to suggest compartment-to-parameter mappings for treatment switch ==========
suggest_compartment_to_parameter_mappings <- function(meta_phase1, meta_phase2) {
  # Load Model 1 and use its actual compartment list (includes $CMT-declared variables
  # like TTR, not just the dosing input compartment stored in meta$input_compartment).
  model1_path <- file.path(models_dir, meta_phase1$filename)
  mod1 <- tryCatch(mread(model1_path), error = function(e) NULL)
  compartments <- if (!is.null(mod1)) tryCatch(mod1@cmtL, error = function(e) c()) else c()
  # Fallback to JSON input_compartment if model can't be loaded
  if (length(compartments) == 0) compartments <- meta_phase1$input_compartment %||% c()
  
  # Load Model 2 and extract its $PARAM names (BL_* params are the typical state-transfer targets).
  model2_path <- file.path(models_dir, meta_phase2$filename)
  mod2 <- tryCatch(mread(model2_path), error = function(e) NULL)
  all_params <- if (!is.null(mod2)) {
    tryCatch(names(as.data.frame(param(mod2))), error = function(e) c())
  } else c()
  # Keep all params for matching; prefer BL_* params but fall back to any params
  bl_params  <- all_params[grepl("^BL_", all_params, ignore.case = TRUE)]
  search_pool <- if (length(bl_params) > 0) bl_params else all_params
  
  empty_df <- data.frame(
    compartment = character(),
    baseline_parameter = character(),
    confidence = numeric(),
    reason = character(),
    stringsAsFactors = FALSE
  )
  
  if (length(compartments) == 0 || length(search_pool) == 0) return(empty_df)
  
  # For each compartment, try exact BL_<comp> match first, then fuzzy
  suggestions <- list()
  
  for (comp in compartments) {
    # Priority 1: exact BL_<comp> match (e.g. TTR -> BL_TTR)
    exact_bl <- paste0("BL_", comp)
    if (exact_bl %in% all_params) {
      suggestions[[length(suggestions) + 1]] <- data.frame(
        compartment = comp,
        baseline_parameter = exact_bl,
        confidence = 100L,
        reason = paste0("Exact BL_ match (", comp, " \u2192 BL_", comp, ")"),
        stringsAsFactors = FALSE
      )
      next
    }
    
    # Priority 2: fuzzy match against BL_* pool (strip BL_ prefix before comparing)
    if (length(search_pool) > 0) {
      stripped_pool <- sub("^BL_", "", search_pool, ignore.case = TRUE)
      norm_comp     <- tolower(gsub("[_-]", "", comp))
      norm_stripped <- tolower(gsub("[_-]", "", stripped_pool))
      
      exact_idx <- which(norm_stripped == norm_comp)
      if (length(exact_idx) > 0) {
        suggestions[[length(suggestions) + 1]] <- data.frame(
          compartment = comp,
          baseline_parameter = search_pool[exact_idx[1]],
          confidence = 95L,
          reason = "Normalized name match after stripping BL_ prefix",
          stringsAsFactors = FALSE
        )
        next
      }
      
      # Partial contains match
      partial_idx <- which(grepl(norm_comp, norm_stripped) | grepl(norm_stripped, norm_comp))
      if (length(partial_idx) > 0) {
        suggestions[[length(suggestions) + 1]] <- data.frame(
          compartment = comp,
          baseline_parameter = search_pool[partial_idx[1]],
          confidence = 80L,
          reason = "Partial name overlap with BL_ parameter",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(suggestions) > 0) {
    suggestions_df <- do.call(rbind, suggestions)
    rownames(suggestions_df) <- NULL
    return(suggestions_df)
  } else {
    return(empty_df)
  }
}

# ========== Function to derive output mappings from parameter mappings ==========
derive_output_mappings_from_parameter_mappings <- function(parameter_mappings, meta1, meta2) {
  # Purpose: For each parameter mapping (compartment → baseline_parameter),
  # find corresponding outputs in Phase 1 and Phase 2 that measure the same entity
  
  if (is.null(parameter_mappings) || nrow(parameter_mappings) == 0) {
    return(data.frame(
      phase1_output = character(),
      phase2_output = character(),
      phase1_output_label = character(),
      phase2_output_label = character(),
      mapped_compartment = character(),
      confidence = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  outputs1 <- meta1$output_var %||% c()
  outputs2 <- meta2$output_var %||% c()
  labels1 <- meta1$output_label %||% c()
  labels2 <- meta2$output_label %||% c()
  
  if (length(outputs1) == 0 || length(outputs2) == 0) {
    return(data.frame(
      phase1_output = character(),
      phase2_output = character(),
      phase1_output_label = character(),
      phase2_output_label = character(),
      mapped_compartment = character(),
      confidence = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Normalize names for comparison
  normalize_name <- function(name) {
    tolower(gsub("[_-]", "", name))
  }
  
  # String similarity function
  string_similarity <- function(s1, s2) {
    s1_norm <- normalize_name(s1)
    s2_norm <- normalize_name(s2)
    
    if (s1_norm == s2_norm) return(1.0)
    if (grepl(s2_norm, s1_norm) || grepl(s1_norm, s2_norm)) return(0.8)
    return(0)
  }
  
  # For each parameter mapping, find corresponding outputs
  output_mappings <- list()
  
  for (i in 1:nrow(parameter_mappings)) {
    comp <- parameter_mappings$compartment[i]
    
    # Find Phase 1 outputs related to this compartment
    sims1 <- sapply(outputs1, function(out) {
      string_similarity(comp, out)
    })
    best_phase1_idx <- which.max(sims1)
    best_phase1_sim <- sims1[best_phase1_idx]
    
    # Find Phase 2 outputs related to this compartment
    # (Likely the same outputs as Phase 1, or with similar names)
    sims2 <- sapply(outputs2, function(out) {
      string_similarity(comp, out)
    })
    best_phase2_idx <- which.max(sims2)
    best_phase2_sim <- sims2[best_phase2_idx]
    
    # If both have reasonable matches, create a mapping
    if (best_phase1_sim > 0.5 && best_phase2_sim > 0.5) {
      # Average confidence
      avg_confidence <- mean(c(best_phase1_sim, best_phase2_sim))
      
      output_mappings[[length(output_mappings) + 1]] <- data.frame(
        phase1_output = outputs1[best_phase1_idx],
        phase2_output = outputs2[best_phase2_idx],
        phase1_output_label = labels1[best_phase1_idx],
        phase2_output_label = labels2[best_phase2_idx],
        mapped_compartment = comp,
        confidence = round(avg_confidence * 100),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(output_mappings) > 0) {
    return(do.call(rbind, output_mappings))
  } else {
    return(data.frame(
      phase1_output = character(),
      phase2_output = character(),
      phase1_output_label = character(),
      phase2_output_label = character(),
      mapped_compartment = character(),
      confidence = numeric(),
      stringsAsFactors = FALSE
    ))
  }
}

# ========== Function to suggest compartment mappings between two models ==========
suggest_compartment_mappings <- function(meta1, meta2) {
  # Get all available variables from both models
  vars1 <- meta1$output_var %||% c()
  vars2 <- meta2$output_var %||% c()
  
  # If either model has no variables, return empty
  if (length(vars1) == 0 || length(vars2) == 0) {
    return(data.frame(
      compartment_from = character(),
      compartment_to = character(),
      confidence = numeric(),
      reason = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Function to normalize names for comparison
  normalize_name <- function(name) {
    tolower(gsub("[_-]", "", name))
  }
  
  # Function to calculate similarity between two strings (0 to 1)
  string_similarity <- function(s1, s2) {
    s1_norm <- normalize_name(s1)
    s2_norm <- normalize_name(s2)
    
    # Exact match (normalized)
    if (s1_norm == s2_norm) return(1.0)
    
    # Partial match - one contains the other
    if (grepl(s2_norm, s1_norm) || grepl(s1_norm, s2_norm)) {
      return(0.8)
    }
    
    # Levenshtein-like distance (simple version)
    # Count matching characters in order
    matches <- 0
    min_len <- min(nchar(s1_norm), nchar(s2_norm))
    if (min_len > 0) {
      for (i in 1:min_len) {
        if (substr(s1_norm, i, i) == substr(s2_norm, i, i)) {
          matches <- matches + 1
        }
      }
      similarity <- matches / max(nchar(s1_norm), nchar(s2_norm))
      return(max(0, similarity))
    }
    
    return(0)
  }
  
  # Find best matches
  suggestions <- list()
  used_vars2 <- c()
  
  for (var1 in vars1) {
    similarities <- sapply(vars2, function(var2) {
      string_similarity(var1, var2)
    })
    
    best_match_idx <- which.max(similarities)
    best_match <- vars2[best_match_idx]
    best_similarity <- similarities[best_match_idx]
    
    # Only suggest if similarity is reasonably high (>0.5)
    if (best_similarity > 0.5) {
      # Determine reason
      reason <- if (best_similarity == 1.0) {
        "Exact match (normalized)"
      } else if (best_similarity >= 0.8) {
        "Strong match (partial overlap)"
      } else {
        "Possible match (character similarity)"
      }
      
      suggestions[[length(suggestions) + 1]] <- data.frame(
        compartment_from = var1,
        compartment_to = best_match,
        confidence = round(best_similarity * 100),
        reason = reason,
        stringsAsFactors = FALSE
      )
      
      used_vars2 <- c(used_vars2, best_match)
    }
  }
  
  # Convert list to data frame
  if (length(suggestions) > 0) {
    suggestions_df <- do.call(rbind, suggestions)
    rownames(suggestions_df) <- NULL
    return(suggestions_df)
  } else {
    return(data.frame(
      compartment_from = character(),
      compartment_to = character(),
      confidence = numeric(),
      reason = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# ========== Helper function for treatment switch simulation ==========
simulate_with_treatment_switch <- function(
  models, 
  models_metadata, 
  treatment_groups,
  switch_settings,
  study_length_weeks,
  model_time_unit,
  input_compartment,
  dose_unit,
  model_dose_unit,
  n_trials,
  trial_design,
  time_unit_display,
  parameter_mappings = NULL  # Data frame with columns: compartment, baseline_parameter
) {
  # Extract switch settings
  initial_model_idx <- switch_settings$initial_model
  switch_model_idx <- switch_settings$switch_model
  switch_time_value <- switch_settings$switch_time
  switch_time_unit <- switch_settings$switch_time_unit
  
  # Convert switch time to model time units
  switch_time_model_units <- convert_time(
    switch_time_value, 
    switch_time_unit, 
    models_metadata[[initial_model_idx]]$model_time_unit, 
    switch_time_unit
  )
  
  # Convert switch time to weeks for generate_input_dataset
  switch_time_weeks <- switch (switch_time_unit,
    "hours" = switch_time_value / (7 * 24),
    "days" = switch_time_value / 7,
    "weeks" = switch_time_value,
    "months" = switch_time_value * 4.345,  # Average weeks per month
    switch_time_value  # Fallback
  )
  
  # Phase 1: Simulate initial model until switch time
  
  meta1 <- models_metadata[[initial_model_idx]]
  mod1 <- models[[initial_model_idx]]
  treatment_groups_df1 <- lapply(1:n_groups, function(i) {
    data.frame(
      GroupName  = input[[paste0("m", initial_model_idx, "_group_name_",  i)]] %||% paste("Group", i),
      SampleSize = input[[paste0("m", initial_model_idx, "_sample_size_", i)]] %||% 100L,
      Treatment  = input[[paste0("m", initial_model_idx, "_treatment_",   i)]] %||% paste("Drug", i),
      Dose       = input[[paste0("m", initial_model_idx, "_dose_",        i, "_1")]] %||%
                     if (!is.na(meta1$therapeutic_dose)) meta1$therapeutic_dose else 10,
      Frequency  = input[[paste0("m", initial_model_idx, "_frequency_",   i, "_1")]] %||%
                     if (!is.na(meta1$therapeutic_frequency)) meta1$therapeutic_frequency else "Weekly",
      Compartment = meta1$input_compartment[1],
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
  
  input_dataset1 <- generate_input_dataset(
    treatment_groups_df1,
    switch_time_weeks,  # Only run until switch time for phase 1
    design = trial_design,
    washout_weeks = 0,  # No washout in treatment switch simulation
    model_time_unit = meta1$model_time_unit,
    input_compartment = meta1$input_compartment[1],
    dose_unit = meta1$dose_unit[1],
    model_dose_unit = meta1$model_dose_unit[1],
    molecular_weight_Da = meta1$molecular_weight_Da
  )
  
  # Create group mapping for phase 1
  group_mapping1 <- treatment_groups_df1 %>%
    distinct(GroupName) %>%
    mutate(GroupID = row_number())
  
  # Add GroupID to input_dataset1 so it has the mapping for later extraction
  input_dataset1 <- input_dataset1 %>%
    left_join(group_mapping1, by = "GroupName")
  
  # Run Phase 1 simulation with GroupID carried through
  results_phase1 <- mod1 %>%
    data_set(input_dataset1) %>%
    mrgsim(end = switch_time_model_units) %>%
    as_tibble() %>%
    # Add GroupName by matching IDs with input_dataset1
    left_join(
      input_dataset1 %>% distinct(ID, GroupName),
      by = "ID"
    )
  
  # Phase 2: Simulate switched model using phase 1 outputs as baseline
  
  meta2 <- models_metadata[[switch_model_idx]]
  mod2 <- models[[switch_model_idx]]
  
  n_groups2 <- treatment_groups[[paste0("model_", switch_model_idx, "_count")]]
  treatment_groups_df2 <- lapply(1:n_groups2, function(i) {
    data.frame(
      GroupName  = input[[paste0("m", switch_model_idx, "_group_name_",  i)]] %||% paste("Group", i),
      SampleSize = input[[paste0("m", switch_model_idx, "_sample_size_", i)]] %||% 100L,
      Treatment  = input[[paste0("m", switch_model_idx, "_treatment_",   i)]] %||% paste("Drug", i),
      Dose       = input[[paste0("m", switch_model_idx, "_dose_",        i, "_1")]] %||%
                     if (!is.na(meta2$therapeutic_dose)) meta2$therapeutic_dose else 10,
      Frequency  = input[[paste0("m", switch_model_idx, "_frequency_",   i, "_1")]] %||%
                     if (!is.na(meta2$therapeutic_frequency)) meta2$therapeutic_frequency else "Weekly",
      Compartment = meta2$input_compartment[1],
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
  
  # Remaining time after switch
  remaining_weeks <- study_length_weeks - switch_time_weeks
  
  # Extract Phase 1 final compartment values for each ID
  phase1_final_states <- results_phase1 %>%
    group_by(ID) %>%
    filter(row_number() == n()) %>%
    ungroup()
  
  # Create group mapping for phase 2
  group_mapping2 <- treatment_groups_df2 %>%
    distinct(GroupName) %>%
    mutate(GroupID = row_number())
  
  # Create Phase 2 input dataset (normal dosing for Phase 2)
  input_dataset2 <- generate_input_dataset(
    treatment_groups_df2,
    remaining_weeks,
    design = trial_design,
    washout_weeks = 0,  # No washout in phase 2
    model_time_unit = meta2$model_time_unit,
    input_compartment = meta2$input_compartment[1],
    dose_unit = meta2$dose_unit[1],
    model_dose_unit = meta2$model_dose_unit[1],
    molecular_weight_Da = meta2$molecular_weight_Da
  )
  
  # Add GroupID to input_dataset2 so it has the mapping for later extraction
  input_dataset2 <- input_dataset2 %>%
    left_join(group_mapping2, by = "GroupName")
  
  # Update IDs in input_dataset2 to match phase 1 IDs if needed
  input_dataset2 <- input_dataset2 %>%
    mutate(ID = as.numeric(ID)) %>%
    arrange(ID, time)
  
  # Transfer state: for each mapping, compute the per-ID mean of the Phase 1
  # final compartment value and add it as a named column in input_dataset2.
  # mrgsolve reads columns in data_set that match $PARAM names and overrides
  # the parameter value at each record — this is the standard covariate pattern.
  if (!is.null(parameter_mappings) && nrow(parameter_mappings) > 0) {
    for (i in seq_len(nrow(parameter_mappings))) {
      comp_name  <- parameter_mappings$compartment[i]         # column in Phase 1 results
      param_name <- parameter_mappings$baseline_parameter[i]  # $PARAM name in Phase 2 model
      if (comp_name %in% colnames(phase1_final_states)) {
        # Average across groups if duplicate IDs exist (IDs restart per group)
        phase1_vals <- phase1_final_states %>%
          dplyr::group_by(ID) %>%
          dplyr::summarise(v = mean(.data[[comp_name]], na.rm = TRUE), .groups = "drop") %>%
          dplyr::rename(!!param_name := v)
        input_dataset2 <- input_dataset2 %>%
          dplyr::left_join(phase1_vals, by = "ID")
      }
    }
  }
  
  # Run Phase 2 simulation with GroupID carried through
  end_time_phase2 <- convert_time(
    remaining_weeks * 7 * 24,
    "hours",
    meta2$model_time_unit,
    "hours"
  )
  
  phase2_sim <- mod2 %>% data_set(input_dataset2)
  results_phase2 <- phase2_sim %>%
    mrgsim(end = end_time_phase2) %>%
    as_tibble() %>%
    mutate(time = time + switch_time_model_units) %>%
    # Add GroupName by matching IDs with input_dataset2
    left_join(
      input_dataset2 %>% distinct(ID, GroupName),
      by = "ID"
    )
  
  # Combine phase 1 and phase 2 results
  combined_results <- bind_rows(
    results_phase1[results_phase1$time > 0, ],  # Exclude initial time=0 from phase 1
    results_phase2
  ) %>%
    arrange(ID, time) %>%
    mutate(study = 1)  # Add study column for compatibility with summaries_converted
  
  # Create combined metadata with treatment switch information
  # This allows the custom mapping to work with the combined timeline
  combined_metadata <- meta2
  combined_metadata$treatment_switch <- TRUE
  combined_metadata$initial_model <- initial_model_idx
  combined_metadata$switch_model <- switch_model_idx
  combined_metadata$switch_time_model_units <- switch_time_model_units
  combined_metadata$meta1 <- meta1
  combined_metadata$meta2 <- meta2
  combined_metadata$results_phase1_only <- results_phase1 %>% filter(time > 0) %>% mutate(study = 1)
  combined_metadata$results_phase2_only <- results_phase2 %>% mutate(study = 1)
  combined_metadata$group_mapping1 <- group_mapping1
  combined_metadata$group_mapping2 <- group_mapping2
  
  # Return combined results with treatment switch flag
  # The combined results are used for custom mapping and summary generation
  # The phase-specific results in metadata are used to create separate plots
  return(list(
    results = combined_results,
    metadata = combined_metadata
  ))
}

  observeEvent(input$run_sim, {
    cat("=============== RUN_SIM TRIGGERED ===============\n")
    
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
    washout_weeks <- if (input$trial_design == "cross-over") input$washout_weeks else 0
    n_studies <- input$n_trials
    interval_type <- input$interval_type
    include_variability <- input$include_variability
    
    cat("Study config: design=", input$trial_design, "enabled=", input$enable_treatment_switch, "n_models=", n_models, "\n")
    cat("treatment_switch_settings$enabled =", treatment_switch_settings$enabled, "\n")
    cat("treatment_switch_settings$parameter_mappings =", nrow(treatment_switch_settings$parameter_mappings %||% data.frame()), "rows\n")

    # Initialize switch result variable
    switch_result <- NULL
    is_treatment_switch_sim <- FALSE
    
    # ========== Check for treatment switch ==========
    if (n_models > 1 && input$trial_design == "parallel" && treatment_switch_settings$enabled) {
      cat("TREATMENT SWITCH ENABLED - Running treatment switch simulation\n")
      is_treatment_switch_sim <- TRUE
      
      # Auto-generate parameter mappings if none have been explicitly set by the user.
      # This ensures state transfer (e.g. TTR -> BL_TTR) always happens, even when the
      # user hasn't interacted with the "Apply Parameter Mappings" UI section.
      # IMPORTANT: use the already-loaded `models` list — do NOT call mread() again here
      # because that would recompile and invalidate the DLLs we're about to simulate with.
      if (is.null(treatment_switch_settings$parameter_mappings) ||
          nrow(treatment_switch_settings$parameter_mappings) == 0) {
        initial_idx_auto <- treatment_switch_settings$initial_model %||% 1
        switch_idx_auto  <- treatment_switch_settings$switch_model  %||% 2
        if (initial_idx_auto >= 1 && initial_idx_auto <= n_models &&
            switch_idx_auto  >= 1 && switch_idx_auto  <= n_models) {
          mod1_loaded <- models[[initial_idx_auto]]
          mod2_loaded <- models[[switch_idx_auto]]
          
          # Compartments from Phase 1 model (includes $CMT-declared vars like TTR)
          comps_auto <- tryCatch(mod1_loaded@cmtL, error = function(e) c())
          if (length(comps_auto) == 0)
            comps_auto <- models_metadata[[initial_idx_auto]]$input_compartment %||% c()
          
          # Parameters from Phase 2 model
          params_auto <- tryCatch(names(as.data.frame(param(mod2_loaded))), error = function(e) c())
          bl_params_auto <- params_auto[grepl("^BL_", params_auto, ignore.case = TRUE)]
          
          cat("Auto-mapping: Phase 1 compartments:", paste(comps_auto, collapse = ", "), "\n")
          cat("Auto-mapping: Phase 2 BL_ params:", paste(bl_params_auto, collapse = ", "), "\n")
          
          auto_suggestions <- list()
          for (.comp in comps_auto) {
            # Exact BL_<comp> match first
            exact_bl <- paste0("BL_", .comp)
            if (exact_bl %in% params_auto) {
              auto_suggestions[[length(auto_suggestions) + 1]] <- data.frame(
                compartment = .comp, baseline_parameter = exact_bl,
                confidence = 100L, reason = paste0("Exact BL_ match"),
                stringsAsFactors = FALSE
              )
            } else if (length(bl_params_auto) > 0) {
              # Fuzzy match against stripped BL_ names
              stripped  <- sub("^BL_", "", bl_params_auto, ignore.case = TRUE)
              norm_comp <- tolower(gsub("[_-]", "", .comp))
              norm_str  <- tolower(gsub("[_-]", "", stripped))
              idx <- which(norm_str == norm_comp)
              if (length(idx) > 0) {
                auto_suggestions[[length(auto_suggestions) + 1]] <- data.frame(
                  compartment = .comp, baseline_parameter = bl_params_auto[idx[1]],
                  confidence = 95L, reason = "Normalized name match",
                  stringsAsFactors = FALSE
                )
              }
            }
          }
          
          if (length(auto_suggestions) > 0) {
            auto_param_df <- do.call(rbind, auto_suggestions)
            rownames(auto_param_df) <- NULL
            treatment_switch_settings$parameter_mappings <- auto_param_df
            cat("Auto-applied", nrow(auto_param_df), "parameter mapping(s) before simulation:\n")
            for (.i in seq_len(nrow(auto_param_df))) {
              cat("  ", auto_param_df$compartment[.i], "->",
                  auto_param_df$baseline_parameter[.i], "\n")
            }
          }
        }
      }
      
      # Run treatment switch simulation
      switch_result <- simulate_with_treatment_switch(
        models = models,
        models_metadata = models_metadata,
        treatment_groups = treatment_groups,
        switch_settings = treatment_switch_settings,
        study_length_weeks = study_length_weeks,
        model_time_unit = model_time_unit,
        input_compartment = input_compartment,
        dose_unit = dose_unit,
        model_dose_unit = model_dose_unit,
        n_trials = n_studies,
        trial_design = input$trial_design,
        time_unit_display = input$time_unit,
        parameter_mappings = treatment_switch_settings$parameter_mappings
      )
      
      # Unpack treatment switch results into two separate model results (for plotting)
      meta1 <- switch_result$metadata$meta1
      meta2 <- switch_result$metadata$meta2
      
      # Shared PD outputs (appear in both models) — used to hide drug-specific PK traces
      shared_ts_outputs <- intersect(meta1$output_var, meta2$output_var)
      
      meta1_ts <- meta1; meta1_ts$treatment_switch <- TRUE; meta1_ts$treatment_switch_phase <- 1
      meta1_ts$shared_ts_outputs <- shared_ts_outputs
      meta2_ts <- meta2; meta2_ts$treatment_switch <- TRUE; meta2_ts$treatment_switch_phase <- 2
      meta2_ts$shared_ts_outputs <- shared_ts_outputs
      
      all_results <- list(
        # Phase 1 model results
        list(
          model_name = paste0(tools::file_path_sans_ext(basename(meta1$filename)), " (Phase 1)"),
          results = switch_result$metadata$results_phase1_only,
          metadata = meta1_ts,
          group_mapping = switch_result$metadata$group_mapping1
        ),
        # Phase 2 model results  
        list(
          model_name = paste0(tools::file_path_sans_ext(basename(meta2$filename)), " (Phase 2)"),
          results = switch_result$metadata$results_phase2_only,
          metadata = meta2_ts,
          group_mapping = switch_result$metadata$group_mapping2
        )
      )
      
      cat("Treatment switch results: Phase 1 nrow =", nrow(all_results[[1]]$results), 
          ", Phase 2 nrow =", nrow(all_results[[2]]$results), "\n")
      cat("all_results: length(all_results) =", length(all_results), "\n")
      
      # Store combined treatment switch results for custom mapping to access
      # Create a pseudo-model entry with combined Phase 1+Phase 2 data
      combined_data_for_mapping <- list(
        results = switch_result$results,  # Combined Phase 1 + Phase 2 timeline
        metadata = switch_result$metadata,  # Contains meta1 and meta2
        group_mapping = NULL
      )
      treatment_switch_combined(combined_data_for_mapping)
      
      # Set n_models to 2 for treatment switch (Phase 1 and Phase 2)
      n_models_for_results <- 2
    } else {
      cat("NOT using treatment switch (condition failed)\n")
      cat("  n_models > 1?", n_models > 1, "\n")
      cat("  trial_design == 'parallel'?", input$trial_design == "parallel", "\n")
      cat("  treatment_switch_settings$enabled?", treatment_switch_settings$enabled, "\n")
      
      # Regular multi-model or single model simulation
      n_models_for_results <- n_models
      
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
          GroupName  = input[[paste0("m", model_idx, "_group_name_",  i)]]  %||% paste("Group", i),
          SampleSize = input[[paste0("m", model_idx, "_sample_size_",  i)]] %||% 100L,
          Treatment  = input[[paste0("m", model_idx, "_treatment_",    i)]] %||% paste("Drug", i),
          Dose       = input[[paste0("m", model_idx, "_dose_",         i, "_", j)]] %||%
                         if (!is.na(meta$therapeutic_dose)) meta$therapeutic_dose else 10,
          Frequency  = input[[paste0("m", model_idx, "_frequency_",    i, "_", j)]] %||%
                         if (!is.na(meta$therapeutic_frequency)) meta$therapeutic_frequency else "Weekly",
          Compartment = meta$input_compartment[j],
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()
    }) %>% bind_rows()
    
    # For dose-response mode: assign log-evenly-spaced doses (0.25x – 4x therapeutic)
    if (isTRUE(question_type() == "dose_response")) {
      base_dose <- if (!is.null(meta$therapeutic_dose) && !is.na(meta$therapeutic_dose) && meta$therapeutic_dose > 0)
        meta$therapeutic_dose else mean(treatment_groups_df$Dose, na.rm = TRUE)
      unique_grps <- unique(treatment_groups_df$GroupName)
      n_grps      <- length(unique_grps)
      dose_levels <- round(exp(seq(log(base_dose * 0.25), log(base_dose * 4), length.out = n_grps)), 4)
      dose_lu <- data.frame(GroupName = unique_grps, .dr_dose = dose_levels, stringsAsFactors = FALSE)
      treatment_groups_df <- treatment_groups_df %>%
        left_join(dose_lu, by = "GroupName") %>%
        mutate(Dose = .dr_dose) %>%
        select(-.dr_dose)
    }

    # Create GroupID mapping
    group_mapping <- treatment_groups_df %>%
      distinct(GroupName) %>%
      mutate(GroupID = row_number())
    
    # Generate input dataset for this model
    input_dataset <- generate_input_dataset(
      treatment_groups = treatment_groups_df,
      study_length_weeks = study_length_weeks,
      design = input$trial_design,
      washout_weeks = 0,
      model_time_unit = meta$model_time_unit,
      input_compartment = meta$input_compartment,
      dose_unit = paste(meta$dose_unit, collapse = ","),
      model_dose_unit = paste(meta$model_dose_unit, collapse = ","),
      molecular_weight_Da = meta$molecular_weight_Da
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
      group_mapping = group_mapping,
      treatment_groups_df = treatment_groups_df
    )
  })
    }
    
    # Store results as LIST (not combined)
    cat("Storing results: length(all_results) =", length(all_results), "\n")
    cat("  all_results[[1]]$results: nrow =", nrow(all_results[[1]]$results), "\n")
    cat("  all_results[[1]]$results: ncol =", ncol(all_results[[1]]$results), "\n")
    cat("  all_results[[1]]$metadata$output_var = c(", paste(all_results[[1]]$metadata$output_var, collapse=", "), ")\n")
    cat("  all_results[[1]]$metadata$output_label = c(", paste(all_results[[1]]$metadata$output_label, collapse=", "), ")\n")
    cat("  all_results[[1]]$metadata$model_time_unit =", all_results[[1]]$metadata$model_time_unit, "\n")
    
    sim_results(all_results)

    # For treatment switch: auto-create the combined plot entry using shared outputs.
    # This runs regardless of whether the user clicked Apply, so the plot always appears.
    if (is_treatment_switch_sim) {
      ts_meta1 <- all_results[[1]]$metadata
      ts_meta2 <- all_results[[2]]$metadata
      shared_outs <- intersect(ts_meta1$output_var, ts_meta2$output_var)
      # Exclude PK/concentration variables from the combined switching study plot;
      # those are drug-specific and not meaningful across a treatment transition.
      pk_pattern <- "^DV_|^Cp|^C_|conc|plasma|serum|AUC"
      shared_outs <- shared_outs[!grepl(pk_pattern, shared_outs, ignore.case = TRUE)]
      if (length(shared_outs) > 0) {
        auto_om <- data.frame(phase1_output = shared_outs, phase2_output = shared_outs,
                              stringsAsFactors = FALSE)
        # Clear any stale entries and create a fresh one
        output_mappings$mappings  <- list()
        output_mappings$count     <- 0
        output_mappings$next_id   <- 1
        output_mappings$mappings[[1]] <- list(
          created = TRUE,
          treatment_switch_auto = TRUE,
          output_mapping_df = auto_om
        )
        output_mappings$count   <- 1
        output_mappings$next_id <- 2
        treatment_switch_settings$output_mappings <- auto_om
        cat("Auto-created combined plot for treatment switch with",
            nrow(auto_om), "shared output(s):", paste(shared_outs, collapse = ", "), "\n")
      }
    }

    # Detect shared output variables across models for comparison mode
    if (isTRUE(trial_presets$auto_run) && length(all_results) > 1 && !is_treatment_switch_sim) {
      model_var_data <- lapply(seq_along(all_results), function(mi) {
        m <- all_results[[mi]]$metadata
        data.frame(
          var   = tolower(trimws(m$output_var)),
          label = m$output_label,
          stringsAsFactors = FALSE
        )
      })
      shared_var_names <- Reduce(intersect, lapply(model_var_data, function(x) x$var))
      if (length(shared_var_names) > 0) {
        shared_labels <- model_var_data[[1]]$label[
          match(shared_var_names, model_var_data[[1]]$var)
        ]
        shared_vars_list(data.frame(
          var   = shared_var_names,
          label = shared_labels,
          stringsAsFactors = FALSE
        ))
        comparison_mode(TRUE)
      }
    }

  # Store global metadata
  sim_metadata(list(
    n_models = n_models_for_results,
    n_studies = n_studies,
    interval_type = interval_type,
    include_variability = include_variability
  ))
  
  cat("Simulation stored successfully. sim_results() length =", length(sim_results()), "\n")
  cat("sim_metadata() $n_models =", sim_metadata()$n_models, "\n")
  }) 

  # Reactive expression that converts time and summarizes results
summaries_converted <- reactive({
  cat("\n========== summaries_converted REACTIVE TRIGGERED ==========\n")
  
  tryCatch({
    # Check if data exists
    if (is.null(sim_results()) || is.null(sim_metadata())) {
      cat("No sim_results or sim_metadata\n")
      return(NULL)
    }
    
    all_results <- sim_results()
    global_metadata <- sim_metadata()
    
    cat("Processing", length(all_results), "model(s)\n")
    cat("global_metadata$n_studies =", global_metadata$n_studies, "\n")
    cat("input$interval_type =", input$interval_type, "\n")
  
  # Process each model's results separately
  all_summaries <- lapply(seq_along(all_results), function(model_idx) {
    cat("\nProcessing model", model_idx, "\n")
    
    model_data <- all_results[[model_idx]]
    results <- model_data$results
    meta <- model_data$metadata
    
    cat("  results: nrow =", nrow(results), "ncol =", ncol(results), "\n")
    cat("  meta$output_var:", paste(meta$output_var, collapse=", "), "\n")
    cat("  meta$model_time_unit =", meta$model_time_unit, "\n")
    
    # Handle missing filename gracefully
    model_name <- if (!is.null(meta$filename)) {
      tools::file_path_sans_ext(basename(meta$filename))
    } else {
      paste0("Model_", model_idx)
    }
    cat("  model_name =", model_name, "\n")
    
    # Check if results exist
    if (is.null(results) || nrow(results) == 0) {
      cat("  ERROR: No results for model", model_idx, "\n")
      return(NULL)
    }
    
    tryCatch({
      # Convert time to selected unit (using THIS model's time unit)
      results_converted <- results %>%
        mutate(time = convert_time(time, meta$model_time_unit, input$time_unit, meta$model_time_unit))
      
      cat("  Time conversion successful. nrow =", nrow(results_converted), "\n")
      
      # Create output mapping for this model
      output_mapping <- tibble(
        variable = meta$output_var,
        label = meta$output_label
      )
      
      cat("  output_mapping: nrow =", nrow(output_mapping), "\n")
      
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
    }, error = function(e) {
      cat("ERROR processing model", model_idx, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  # Return as list (one entry per model)
  cat("Returning", length(all_summaries), "summaries\n")
  all_summaries
  }, error = function(e) {
    cat("ERROR in summaries_converted reactive:\n")
    cat("  Message:", e$message, "\n")
    cat("  Call:", deparse(e$call), "\n")
    return(NULL)
  })
})
  
  # Generate separate plot outputs for each model
observe({
  cat("\n========== PLOT GENERATION OBSERVE TRIGGERED ==========\n")
  
  # Force dependency on sim_results() directly (not through summaries_converted)
  sim_data <- sim_results()
  sim_meta <- sim_metadata()
  
  cat("sim_data is null?", is.null(sim_data), "\n")
  cat("sim_meta is null?", is.null(sim_meta), "\n")
  
  # Only proceed if we have data
  if (!is.null(sim_data) && !is.null(sim_meta)) {
    cat("Computing summaries...\n")
    summaries_list <- summaries_converted()
    global_metadata <- sim_metadata()
    
    cat("summaries_list is null?", is.null(summaries_list), "\n")
    cat("global_metadata is null?", is.null(global_metadata), "\n")
  
    if (!is.null(summaries_list) && !is.null(global_metadata)) {
      cat("Both not null. Generating plots for", global_metadata$n_models, "model(s)\n")
      
      lapply(seq_len(global_metadata$n_models), function(model_idx) {
        output_name <- paste0("model_plot_", model_idx)
        
        cat("Creating plot:", output_name, "\n")
        
        output[[output_name]] <- renderPlotly({
          model_summary <- summaries_list[[model_idx]]
          summaries <- model_summary$summaries
          model_name <- model_summary$model_name
          meta <- model_summary$metadata

          # In dose-response mode, replace generic GroupName with dose labels
          if (isTRUE(question_type() == "dose_response")) {
            tg_df <- sim_data[[model_idx]]$treatment_groups_df
            if (!is.null(tg_df)) {
              dose_lbls <- tg_df %>%
                dplyr::group_by(GroupName) %>%
                dplyr::summarise(dose = first(Dose), .groups = "drop") %>%
                dplyr::mutate(GroupLabel = paste0(round(dose, 2), " ", meta$dose_unit[1]))
              summaries <- summaries %>%
                dplyr::left_join(dose_lbls %>% dplyr::select(GroupName, GroupLabel), by = "GroupName") %>%
                dplyr::mutate(GroupName = dplyr::coalesce(GroupLabel, GroupName)) %>%
                dplyr::select(-GroupLabel)
            }
          }
          
          # In treatment switch, each phase shows its own model's outputs.
          # Cross-phase visualization is handled by the custom mapping panel.

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

          p <- ggplotly(g)
          seen_names <- c()
          for (ii in seq_along(p$x$data)) {
            nm <- p$x$data[[ii]]$name
            if (!is.null(nm) && nzchar(nm))
              p$x$data[[ii]]$name <- sub("^\\((.+),\\d+\\)$", "\\1", nm)
            p$x$data[[ii]]$legendgroup      <- NULL
            p$x$data[[ii]]$legendgrouptitle <- NULL
            clean_nm <- p$x$data[[ii]]$name
            if (!is.null(clean_nm) && nzchar(clean_nm)) {
              if (clean_nm %in% seen_names) {
                p$x$data[[ii]]$showlegend <- FALSE
              } else {
                seen_names <- c(seen_names, clean_nm)
              }
            }
          }
          p
        })
      })
      
      cat("Plot generation complete.\n")
    } else {
      cat("Cannot generate plots: summaries_list null?", is.null(summaries_list), "global_metadata null?", is.null(global_metadata), "\n")
    }
  }
})

# Generate dose-response plots when the clinical question is Dose-Response Modeling
# Derives endpoint summaries from summaries_converted() so median/CI match the time-course plots exactly.
observe({
  if (!isTRUE(question_type() %in% c("dose_response"))) return()

  sim_data   <- sim_results()
  sim_meta   <- sim_metadata()
  sums_list  <- summaries_converted()
  if (is.null(sim_data) || is.null(sim_meta) || is.null(sums_list)) return()

  lapply(seq_len(sim_meta$n_models), function(model_idx) {
    res_entry  <- sim_data[[model_idx]]
    tg_df      <- res_entry$treatment_groups_df
    if (is.null(tg_df) || nrow(tg_df) == 0) return()

    meta       <- res_entry$metadata
    model_sums <- sums_list[[model_idx]]
    if (is.null(model_sums)) return()

    summaries  <- model_sums$summaries

    # Dose lookup per group (first compartment row)
    dose_per_group <- tg_df %>%
      group_by(GroupName) %>%
      summarise(dose = first(Dose), .groups = "drop")

    # Filter to last timepoint (already in display time units)
    last_time <- max(summaries$time, na.rm = TRUE)
    endpoint  <- summaries %>% filter(time == last_time)

    # Exclude concentration variables
    conc_pattern <- "conc|plasma|serum|Cp|Cc|C_plasma|C_central|AUC"
    endpoint <- endpoint %>%
      filter(!grepl(conc_pattern, variable, ignore.case = TRUE))

    dr <- endpoint %>%
      left_join(dose_per_group, by = "GroupName")

    output[[paste0("dose_response_plot_", model_idx)]] <- renderPlotly({
      if (nrow(dr) == 0) return(plotly_empty())
      dose_lbl <- if (!is.null(meta$dose_unit) && length(meta$dose_unit) > 0 && !is.na(meta$dose_unit[1]))
        paste0("Dose (", meta$dose_unit[1], ")") else "Dose"

      g <- ggplot(dr, aes(x = dose, y = median, color = label, group = label)) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = label), alpha = 0.2, color = NA, show.legend = FALSE) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        facet_wrap(~ label, scales = "free_y") +
        labs(
          title = paste0(res_entry$model_name, " \u2013 Dose-Response at Study End"),
          x     = dose_lbl,
          y     = "Response"
        ) +
        theme_bw() +
        theme(
          plot.title      = element_text(hjust = 0.5, face = "bold", size = 14),
          legend.position = "none"
        )
      ggplotly(g)
    })
  })
})

# Generate plots for custom mappings
observe({
  # Force direct dependency on sim_results() (not through nested summaries_converted)
  sim_data <- sim_results()
  sim_meta <- sim_metadata()
  ts_combined <- treatment_switch_combined()  # Check for combined treatment switch data
  
  # Only compute summaries if we have data
  if (!is.null(sim_data) && !is.null(sim_meta)) {
    summaries_list <- summaries_converted()
    global_metadata <- sim_metadata()
    
    # For treatment switch, use combined data for custom mapping
    use_combined_data <- !is.null(ts_combined)
    
    if (use_combined_data && !is.null(summaries_list) && !is.null(global_metadata)) {
      cat("Using combined treatment switch data for custom mapping\n")
      # Create summaries from combined data for custom mapping to use
      ts_results <- ts_combined$results
      ts_metadata <- ts_combined$metadata
      
      # Convert time
      ts_results_converted <- ts_results %>%
        mutate(time = convert_time(time, ts_metadata$model_time_unit, input$time_unit, ts_metadata$model_time_unit))
      
      # Convert switch time to display units for phase labelling
      switch_t_display <- convert_time(
        ts_metadata$switch_time_model_units,
        ts_metadata$model_time_unit, input$time_unit,
        ts_metadata$model_time_unit
      )
      
      # Compound names for each phase (fall back to model filename)
      compound1_name <- ts_metadata$meta1$compound %||%
        tools::file_path_sans_ext(basename(ts_metadata$meta1$filename))
      compound2_name <- ts_metadata$meta2$compound %||%
        tools::file_path_sans_ext(basename(ts_metadata$meta2$filename))
      
      # Tag each row with phase and a human-readable series label.
      # Prefer just the compound name (e.g. "Eplontersen"); only prepend the group name
      # when there are multiple treatment groups to disambiguate.
      n_groups_ts <- length(unique(ts_results_converted$GroupName))
      ts_results_tagged <- ts_results_converted %>%
        mutate(
          phase = ifelse(time <= switch_t_display, 1L, 2L),
          compound_label = ifelse(time <= switch_t_display, compound1_name, compound2_name),
          series = if (n_groups_ts <= 1)
            compound_label
          else
            paste0(GroupName, " \u2013 ", compound_label)
        )
      
      # Create simplified summaries for custom mapping
      # For percent-change variables: recompute from the true simulation start (time=0 of Phase 1)
      # so that Phase 2 values are expressed vs the Phase 1 baseline, not their own model start.
      pct_vars <- ts_metadata$meta1$output_var[
        grepl("pct|percent|change", ts_metadata$meta1$output_var, ignore.case = TRUE)
      ]
      
      # Identify the corresponding absolute variables for each pct var
      # by stripping common suffixes (e.g. TTR_pct_change -> TTR)
      abs_var_for_pct <- function(pvar) {
        stripped <- sub("_pct_change$|_pct$|_percent_change$|_percent$", "", pvar, ignore.case = TRUE)
        stripped
      }
      
      ts_summaries_for_mapping <- lapply(
        union(ts_metadata$meta1$output_var, ts_metadata$meta2$output_var),
        function(yvar) {
          if (!yvar %in% colnames(ts_results_tagged)) return(NULL)
          
          is_pct_var <- yvar %in% pct_vars
          
          if (is_pct_var) {
            # Recompute pct change from Phase 1 time=0 baseline per ID
            abs_v <- abs_var_for_pct(yvar)
            if (abs_v %in% colnames(ts_results_tagged)) {
              # Baseline = mean of absolute variable at the very first time point per subject
              baselines <- ts_results_tagged %>%
                group_by(ID) %>%
                arrange(time) %>%
                slice(1) %>%
                ungroup() %>%
                select(ID, .baseline = !!abs_v)
              
              recomputed <- ts_results_tagged %>%
                left_join(baselines, by = "ID") %>%
                mutate(!!yvar := ((.data[[abs_v]] - .baseline) / .baseline) * 100)
              
              recomputed %>%
                group_by(series, GroupName, compound_label, phase, time) %>%
                summarise(
                  median = mean(.data[[yvar]], na.rm = TRUE),
                  lower  = quantile(.data[[yvar]], 0.025, na.rm = TRUE),
                  upper  = quantile(.data[[yvar]], 0.975, na.rm = TRUE),
                  variable = yvar,
                  .groups = "drop"
                )
            } else {
              # Fallback: use as-is
              ts_results_tagged %>%
                group_by(series, GroupName, compound_label, phase, time) %>%
                summarise(
                  median = mean(.data[[yvar]], na.rm = TRUE),
                  lower  = quantile(.data[[yvar]], 0.025, na.rm = TRUE),
                  upper  = quantile(.data[[yvar]], 0.975, na.rm = TRUE),
                  variable = yvar,
                  .groups = "drop"
                )
            }
          } else {
            ts_results_tagged %>%
              group_by(series, GroupName, compound_label, phase, time) %>%
              summarise(
                median = mean(.data[[yvar]], na.rm = TRUE),
                lower  = quantile(.data[[yvar]], 0.025, na.rm = TRUE),
                upper  = quantile(.data[[yvar]], 0.975, na.rm = TRUE),
                variable = yvar,
                .groups = "drop"
              )
          }
        }
      ) %>%
        bind_rows() %>%
        mutate(
          # Build human-readable label from meta1 first (both models share PD var names).
          # This is used as the plot title so must be correct per variable.
          label = {
            vars1   <- ts_metadata$meta1$output_var   %||% character(0)
            labs1   <- ts_metadata$meta1$output_label %||% character(0)
            vars2   <- ts_metadata$meta2$output_var   %||% character(0)
            labs2   <- ts_metadata$meta2$output_label %||% character(0)
            # Combine, meta1 takes precedence (first unique match)
            all_v <- c(vars1, vars2)
            all_l <- c(labs1, labs2)
            keep  <- !duplicated(all_v)
            lkp   <- setNames(all_l[keep], all_v[keep])
            dplyr::coalesce(lkp[variable], variable)
          }
        ) %>%
        select(series, GroupName, compound_label, phase, time, median, lower, upper, variable, label)
      
      # Use this for custom mapping instead of summaries_list
      temp_summaries <- list(
        list(
          model_name = "Treatment Switch (Combined)",
          summaries = ts_summaries_for_mapping,
          metadata = ts_metadata
        )
      )
    } else if (!is.null(summaries_list) && !is.null(global_metadata) && global_metadata$n_models > 1) {
      temp_summaries <- summaries_list
    } else {
      temp_summaries <- NULL
    }
    
    if (!is.null(temp_summaries) && !is.null(global_metadata)) {
      # Get valid mapping IDs - only if there are mappings
      valid_ids <- c()
      if (!is.null(output_mappings$mappings) && length(output_mappings$mappings) > 0) {
        valid_ids <- which(sapply(output_mappings$mappings, function(x) !is.null(x)))
      }
      
      if (length(valid_ids) > 0) {
        cat("Creating custom mapping plots for", length(valid_ids), "mapping(s)\n")
        lapply(valid_ids, function(mapping_id) {
          mapping_obj <- output_mappings$mappings[[mapping_id]]
          is_auto_generated <- !is.null(mapping_obj$treatment_switch_auto) && mapping_obj$treatment_switch_auto
          
          cat("Creating custom_mapping_plot_", mapping_id, 
              if(is_auto_generated) " (auto-generated treatment switch)" else " (user-created)", "\n")
          output_name <- paste0("custom_mapping_plot_", mapping_id)
        
        output[[output_name]] <- renderPlotly({
          # For auto-generated treatment switch mappings, use the predefined pairs
          if (is_auto_generated && !is.null(mapping_obj$output_mapping_df)) {
            output_mapping_df <- mapping_obj$output_mapping_df
            cat("Using auto-generated output mapping with", nrow(output_mapping_df), "pairs\n")
            
            # Determine switch time in display units for the vertical marker
            ts_meta_raw   <- treatment_switch_combined()
            switch_time_display <- if (!is.null(ts_meta_raw)) {
              switch_model_units <- ts_meta_raw$metadata$switch_time_model_units %||% NA
              mt_unit            <- ts_meta_raw$metadata$model_time_unit %||% "hours"
              if (!is.na(switch_model_units))
                convert_time(switch_model_units, mt_unit, input$time_unit, mt_unit)
              else NA
            } else NA
            
            time_label <- switch(input$time_unit,
              "hours"  = "Time (hours)",
              "days"   = "Time (days)",
              "weeks"  = "Time (weeks)",
              "months" = "Time (months)",
              "Time"
            )
            
            # For each mapped output variable, draw one continuous timeline per group,
            # coloured by series (GroupName + compound/phase) so Phase 1 and Phase 2
            # appear in distinct colours.
            # Combine all mapped variables into one data frame and use facet_wrap.
            # This avoids subplot() which causes duplicate legend entries and broken titles.
            plot_data_all <- lapply(seq_len(nrow(output_mapping_df)), function(row_idx) {
              temp_summaries[[1]]$summaries %>%
                dplyr::filter(variable == output_mapping_df$phase1_output[row_idx])
            }) %>% dplyr::bind_rows()
            
            if (nrow(plot_data_all) == 0) {
              plotly::plot_ly() %>% plotly::layout(title = "No data available")
            } else {
              color_col <- if ("series" %in% colnames(plot_data_all)) "series" else "GroupName"
              
              # For pct-change facets use a different y label; keep the strip title as-is
              plot_data_all <- plot_data_all %>%
                dplyr::mutate(
                  y_axis_label = dplyr::if_else(
                    grepl("pct|percent|change", variable, ignore.case = TRUE),
                    "% Change from Baseline",
                    label
                  )
                )
              
              g <- ggplot(plot_data_all,
                          aes(x = time, y = median,
                              color = .data[[color_col]],
                              fill  = .data[[color_col]])) +
                geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20, color = NA) +
                geom_line(linewidth = 1) +
                facet_wrap(~ label, scales = "free_y", nrow = 1) +
                labs(x = time_label, y = NULL, color = NULL, fill = NULL) +
                theme_bw() +
                theme(
                  strip.text      = element_text(face = "bold", size = 11),
                  legend.position = "bottom"
                )
              
              if (!is.na(switch_time_display)) {
                g <- g + geom_vline(xintercept = switch_time_display, linetype = "dashed",
                                    color = "grey40", linewidth = 0.8)
              }
              
              p <- ggplotly(g, tooltip = c("x", "y", "colour"))
              # Deduplicate legend entries across the combined plot
              seen <- c()
              for (ii in seq_along(p$x$data)) {
                nm <- p$x$data[[ii]]$name
                if (!is.null(nm) && nzchar(nm))
                  p$x$data[[ii]]$name <- sub("^\\((.+),\\d+\\)$", "\\1", nm)
                p$x$data[[ii]]$legendgroup      <- NULL
                p$x$data[[ii]]$legendgrouptitle <- NULL
                clean_nm <- p$x$data[[ii]]$name
                if (!is.null(clean_nm) && nzchar(clean_nm)) {
                  if (clean_nm %in% seen) {
                    p$x$data[[ii]]$showlegend <- FALSE
                  } else {
                    seen <- c(seen, clean_nm)
                  }
                }
              }
              p
            }
          } else {
            # User-created custom mapping - use the dropdown selection approach
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
              
              # Use temp_summaries if available (for treatment switch), otherwise summaries_list
              target_summaries <- if (!is.null(temp_summaries)) temp_summaries else summaries_list
              
              # For treatment switch combined data, use first (and only) entry
              if (use_combined_data && length(target_summaries) == 1) {
                model_summary <- target_summaries[[1]]
                model_name <- "Combined Treatment Switch"
              } else {
                model_summary <- target_summaries[[model_idx]]
                model_name <- model_summary$model_name
              }
              
              summaries <- model_summary$summaries
              
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
          }
        })
        })
      }
    }
  }
})

# Generate head-to-head comparison plots for shared output variables
observe({
  comp_mode <- comparison_mode()
  if (!isTRUE(comp_mode)) return()
  svl  <- shared_vars_list()
  sums <- summaries_converted()
  if (is.null(svl) || is.null(sums)) return()

  lapply(seq_len(nrow(svl)), function(vi) {
    yvar  <- svl$var[vi]
    ylab  <- svl$label[vi]
    oname <- paste0("auto_comparison_plot_", vi)

    output[[oname]] <- renderPlotly({
      combined <- lapply(seq_along(sums), function(mi) {
        s    <- sums[[mi]]
        mvar <- s$metadata$output_var[tolower(trimws(s$metadata$output_var)) == yvar]
        if (length(mvar) == 0) return(NULL)
        {
          compound_lbl <- if (!is.null(s$metadata$compound) && nchar(s$metadata$compound) > 0)
            s$metadata$compound else s$model_name
          sdf <- s$summaries %>% dplyr::filter(variable == mvar[1])
          n_grps <- length(unique(sdf$GroupName))
          sdf %>% dplyr::mutate(
            series = if (n_grps <= 1) compound_lbl
                     else paste0(compound_lbl, " \u2013 ", GroupName)
          )
        }
      })
      if (length(combined) == 0) return(plotly::plot_ly())
      combined <- dplyr::bind_rows(combined)

      time_label <- switch(input$time_unit,
        "hours"  = "Time (hours)",
        "days"   = "Time (days)",
        "weeks"  = "Time (weeks)",
        "months" = "Time (months)",
        "Time"
      )

      g <- ggplot(combined, aes(x = time, y = median, color = series)) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = series),
                    alpha = 0.18, color = NA) +
        geom_line(linewidth = 0.9) +
        labs(title = ylab, x = time_label, y = ylab, color = NULL, fill = NULL) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      p <- ggplotly(g)
      seen_names <- c()
      for (ii in seq_along(p$x$data)) {
        nm <- p$x$data[[ii]]$name
        if (!is.null(nm) && nzchar(nm))
          p$x$data[[ii]]$name <- sub("^\\((.+),\\d+\\)$", "\\1", nm)
        p$x$data[[ii]]$legendgroup      <- NULL
        p$x$data[[ii]]$legendgrouptitle <- NULL
        clean_nm <- p$x$data[[ii]]$name
        if (!is.null(clean_nm) && nzchar(clean_nm)) {
          if (clean_nm %in% seen_names) {
            p$x$data[[ii]]$showlegend <- FALSE
          } else {
            seen_names <- c(seen_names, clean_nm)
          }
        }
      }
      p
    })
  })
})

# Create dynamic UI with separate plots
output$sim_result <- renderUI({
  cat("\n========== OUTPUT$SIM_RESULT RENDERUI TRIGGERED ==========\n")
  
  sim_res <- sim_results()
  sim_meta <- sim_metadata()
  
  cat("sim_results() is null?", is.null(sim_res), "\n")
  cat("sim_metadata() is null?", is.null(sim_meta), "\n")
  
  if (!is.null(sim_res)) {
    cat("  length(sim_results()) =", length(sim_res), "\n")
    cat("  nrow(sim_results()[[1]]$results) =", nrow(sim_res[[1]]$results), "\n")
  }
  
  if (!is.null(sim_meta)) {
    cat("  sim_metadata()$n_models =", sim_meta$n_models, "\n")
  }
  
  if (is.null(sim_res) || is.null(sim_meta)) {
    cat("Returning 'no results' UI\n")
    if (isTRUE(trial_presets$auto_run)) {
      div(
        style = "text-align: center; padding: 50px;",
        tags$span(class = "fa fa-spinner fa-spin",
                  style = "font-size:1.6em; margin-right:10px; color:#2563eb;"),
        h4("Preparing simulation...", style = "display:inline; color:#64748b;")
      )
    } else {
      div(
        style = "text-align: center; padding: 50px;",
        h4("No simulation results yet. Click 'Run Simulation' to begin.")
      )
    }
  } else if (isTRUE(comparison_mode())) {
    cat("Returning comparison mode UI\n")

    global_metadata <- sim_meta
    svl <- shared_vars_list()

    valid_mapping_ids <- if (length(output_mappings$mappings) > 0) {
      which(sapply(output_mappings$mappings, function(x) !is.null(x)))
    } else {
      integer(0)
    }

    tagList(
      # Comparison plots section
      div(
        style = "margin-bottom: 24px; padding: 20px; background: #f8faff; border: 1px solid #bfdbfe; border-radius: 12px;",
        h4(
          tags$span(class = "fa fa-chart-line", style = "margin-right: 8px; color: #2563eb;"),
          "Head-to-Head Comparison",
          style = "margin: 0 0 16px 0; color: #1e3a5f; font-weight: 700;"
        ),
        if (!is.null(svl) && nrow(svl) > 0) {
          lapply(seq_len(nrow(svl)), function(vi) {
            div(
              style = "margin-bottom: 20px;",
              plotlyOutput(paste0("auto_comparison_plot_", vi), height = "420px")
            )
          })
        } else {
          p("No shared output variables detected across models.", style = "color: #64748b;")
        }
      ),

      # Toggle button
      div(
        style = "text-align: center; margin: 20px 0;",
        actionButton("show_full_results", "Show full simulation results \u25bc",
          class = "btn btn-default btn-sm",
          style = "border-radius: 6px; font-weight: 500; color: #64748b; border-color: #cbd5e1;")
      ),

      # Hidden full results panel
      shinyjs::hidden(
        div(
          id = "full_results_panel",
          h4("Individual Model Results",
             style = "margin: 8px 0 20px 0; color: #1e3a5f; border-top: 1px solid #e2e8f0; padding-top: 20px;"),
          lapply(seq_len(global_metadata$n_models), function(model_idx) {
            div(
              style = "margin-bottom: 30px; border: 1px solid #ddd; padding: 15px; border-radius: 8px;",
              plotlyOutput(paste0("model_plot_", model_idx), height = "400px")
            )
          }),
          if (length(valid_mapping_ids) > 0) {
            tagList(
              h4("Custom Output Combinations",
                 style = "margin-top: 30px; border-top: 2px solid #337ab7; padding-top: 20px;"),
              lapply(valid_mapping_ids, function(mapping_id) {
                div(
                  style = "margin-bottom: 30px; border: 2px solid #337ab7; padding: 15px; border-radius: 8px; background: #f0f8ff;",
                  plotlyOutput(paste0("custom_mapping_plot_", mapping_id), height = "400px")
                )
              })
            )
          }
        )
      )
    )
  } else {
    cat("Returning results UI with", sim_meta$n_models, "model(s)\n")

    global_metadata <- sim_meta
    is_ts_ui <- !is.null(treatment_switch_combined())

    # Get valid custom mapping IDs - safely handle empty mappings
    valid_mapping_ids <- if (length(output_mappings$mappings) > 0) {
      which(sapply(output_mappings$mappings, function(x) !is.null(x)))
    } else {
      integer(0)
    }

    # ---- Treatment Switch layout ----
    if (is_ts_ui) {
      tagList(
        # Primary: Switching Study combined plots
        div(
          style = "margin-bottom: 24px; padding: 20px; background: #f8faff; border: 1px solid #bfdbfe; border-radius: 12px;",
          h4(
            tags$span(class = "fa fa-exchange-alt", style = "margin-right: 8px; color: #2563eb;"),
            "Switching Study",
            style = "margin: 0 0 16px 0; color: #1e3a5f; font-weight: 700;"
          ),
          if (length(valid_mapping_ids) > 0) {
            lapply(valid_mapping_ids, function(mapping_id) {
              div(
                style = "margin-bottom: 20px;",
                plotlyOutput(paste0("custom_mapping_plot_", mapping_id), height = "420px")
              )
            })
          } else {
            p("Run simulation to see combined treatment switch results.",
              style = "color: #64748b;")
          }
        ),

        # Toggle for individual phase details
        div(
          style = "text-align: center; margin: 20px 0;",
          actionButton("show_full_results", "Show individual phase simulations \u25bc",
            class = "btn btn-default btn-sm",
            style = "border-radius: 6px; font-weight: 500; color: #64748b; border-color: #cbd5e1;")
        ),

        # Hidden: individual Phase 1 and Phase 2 plots
        shinyjs::hidden(
          div(
            id = "full_results_panel",
            h4("Individual Phase Simulations",
               style = "margin: 8px 0 20px 0; color: #1e3a5f; border-top: 1px solid #e2e8f0; padding-top: 20px;"),
            lapply(seq_len(global_metadata$n_models), function(model_idx) {
              div(
                style = "margin-bottom: 30px; border: 1px solid #ddd; padding: 15px; border-radius: 8px;",
                plotlyOutput(paste0("model_plot_", model_idx), height = "400px")
              )
            })
          )
        )
      )

    # ---- Dose-response layout ----
    } else if (question_type() %in% c("dose_response")) {
      tagList(
        h3(paste("Simulation Results -", global_metadata$n_models, "Model(s)"),
           style = "text-align: center;"),
        div(
          style = "margin-bottom: 24px; padding: 20px; background: #f0f9ff; border: 1px solid #bae6fd; border-radius: 12px;",
          h4(
            tags$span(class = "fa fa-chart-bar", style = "margin-right: 8px; color: #0284c7;"),
            "Dose-Response at Study End",
            style = "margin: 0 0 16px 0; color: #0c4a6e; font-weight: 700;"
          ),
          lapply(seq_len(global_metadata$n_models), function(model_idx) {
            div(
              style = "margin-bottom: 16px;",
              plotlyOutput(paste0("dose_response_plot_", model_idx), height = "380px")
            )
          })
        ),
        div(
          style = "text-align: center; margin: 20px 0;",
          actionButton("show_full_results", "Show full simulation results \u25bc",
            class = "btn btn-default btn-sm",
            style = "border-radius: 6px; font-weight: 500; color: #64748b; border-color: #cbd5e1;")
        ),
        shinyjs::hidden(
          div(
            id = "full_results_panel",
            h4("Time-Course Simulation Results",
               style = "margin: 8px 0 20px 0; color: #1e3a5f; border-top: 1px solid #e2e8f0; padding-top: 20px;"),
            lapply(seq_len(global_metadata$n_models), function(model_idx) {
              div(
                style = "margin-bottom: 30px; border: 1px solid #ddd; padding: 15px; border-radius: 8px;",
                plotlyOutput(paste0("model_plot_", model_idx), height = "400px")
              )
            }),
            if (length(valid_mapping_ids) > 0) {
              tagList(
                h4("Custom Output Combinations",
                   style = "margin-top: 30px; border-top: 2px solid #337ab7; padding-top: 20px;"),
                lapply(valid_mapping_ids, function(mapping_id) {
                  div(
                    style = "margin-bottom: 30px; border: 2px solid #337ab7; padding: 15px; border-radius: 8px; background: #f0f8ff;",
                    plotlyOutput(paste0("custom_mapping_plot_", mapping_id), height = "400px")
                  )
                })
              )
            }
          )
        )
      )

    # ---- Default multi/single-model layout ----
    } else {
      tagList(
        h3(paste("Simulation Results -", global_metadata$n_models, "Model(s)"),
           style = "text-align: center;"),
        lapply(seq_len(global_metadata$n_models), function(model_idx) {
          div(
            style = "margin-bottom: 30px; border: 1px solid #ddd; padding: 15px; border-radius: 8px;",
            plotlyOutput(paste0("model_plot_", model_idx), height = "400px")
          )
        }),
        if (length(valid_mapping_ids) > 0) {
          tagList(
            h4("Custom Output Combinations",
               style = "margin-top: 30px; border-top: 2px solid #337ab7; padding-top: 20px;"),
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
  }
})

  
  # Toggle full simulation results panel
  observeEvent(input$show_full_results, {
    shinyjs::toggle("full_results_panel")
    is_ts <- !is.null(treatment_switch_combined())
    if (input$show_full_results %% 2 == 1) {
      new_label <- if (is_ts) "Hide individual phase simulations \u25b2" else "Hide full simulation results \u25b2"
    } else {
      new_label <- if (is_ts) "Show individual phase simulations \u25bc" else "Show full simulation results \u25bc"
    }
    updateActionButton(session, "show_full_results", label = new_label)
  })

  # Toggle sidebar parameters panel
  observeEvent(input$toggle_sidebar, {
    current_state <- sidebar_visible()
    sidebar_visible(!current_state)
    
    if (!current_state) {
      # Show sidebar - remove BOTH full-width classes (set by tab observer or auto_run)
      shinyjs::removeClass("cts_sidebar_col", "sidebar-hidden")
      shinyjs::removeClass("cts_main_col", "expanded")
      shinyjs::removeClass("cts_main_col", "full-width")
      # Trigger layout/widget refresh for any deferred Bootstrap/selectize init
      shinyjs::runjs("setTimeout(function(){$(window).trigger('resize');}, 50);")
    } else {
      # Hide sidebar - add hidden class
      shinyjs::addClass("cts_sidebar_col", "sidebar-hidden")
      shinyjs::addClass("cts_main_col", "expanded")
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
    # First, check how many models have this validation data type
    has_val_data <- sapply(seq_len(n_models), function(idx) {
      meta <- models_metadata[[idx]]
      if (val_data_source == "external") {
        !is.null(meta$external_validation_data)
      } else {
        !is.null(meta$internal_validation_data)
      }
    })
    
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
      return(NULL)
    }
    
    # Run simulations for each model - ALL study arms from JSON
    all_val_results <- lapply(seq_len(n_models), function(model_idx) {
      meta <- models_metadata[[model_idx]]
      mod <- models[[model_idx]]
      # safe_name is used for file system paths (cache dirs, data_location parsing)
      # model_name is the human-readable display name used in UI output
      safe_name  <- tools::file_path_sans_ext(basename(meta$filename))
      model_name <- meta$display_name %||% safe_name
      
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
        
        arms <- study$arms
        
        # ===== STUDY-LEVEL CACHING LOGIC =====
        # Determine study cache directory from first arm's data_location
        study_cache_dir <- NULL
        study_folder_name <- NULL
        study_config_hash <- NULL
        cache_hit <- FALSE
        
        first_arm <- arms[[1]]
        if (!is.null(first_arm) && is.list(first_arm) && !is.null(first_arm$data_location)) {
          data_location <- first_arm$data_location
          parts <- strsplit(data_location, "/")[[1]]
          model_idx_in_path <- which(parts == safe_name)
          if (length(model_idx_in_path) > 0 && model_idx_in_path + 1 <= length(parts)) {
            study_folder_name <- parts[model_idx_in_path + 1]
            # Include study_id in cache path to avoid conflicts between multiple studies from same publication
            study_cache_dir <- file.path("data", "derived", "validation", safe_name, study_folder_name, study_id)
            # Create the directory immediately so arm summaries can be saved
            dir.create(study_cache_dir, recursive = TRUE, showWarnings = FALSE)
          }
        }
        
        # Compute hash of the complete study configuration (all arms)
        if (!is.null(study_cache_dir)) {
          study_config_json <- jsonlite::toJSON(study, auto_unbox = TRUE, pretty = FALSE)
          study_config_hash <- digest::digest(study_config_json, algo = "sha256")
          
          # Check if cache exists and hash matches
          hash_file <- file.path(study_cache_dir, "config_hash.txt")
          if (file.exists(hash_file)) {
            stored_hash <- trimws(readLines(hash_file, n = 1))
            if (stored_hash == study_config_hash) {
              cache_hit <- TRUE
            }
          }
        }
        
        # If cache hits, load all arm summaries from subdirectories
        if (cache_hit && !is.null(study_cache_dir)) {
          cached_results <- list()
          
          for (arm_idx in seq_along(arms)) {
            arm <- arms[[arm_idx]]
            arm_name <- if (is.list(arm) && !is.null(arm$arm_name)) arm$arm_name else paste("Arm", arm_idx)
            arm_cache_subdir <- file.path(study_cache_dir, paste0("arm_", arm_idx))
            summaries_file <- file.path(arm_cache_subdir, "summaries.rds")
            
            if (file.exists(summaries_file)) {
              tryCatch({
                summaries <- readRDS(summaries_file)
                
                cached_results[[arm_idx]] <- list(
                  model_name = model_name,
                  study_id = study_id,
                  study_name = study_name,
                  study_data_source = study$study_data_source %||% NULL,
                  study_design      = study$study_design      %||% NULL,
                  population        = study$population        %||% NULL,
                  primary_endpoint  = study$primary_endpoint  %||% NULL,
                  n_subjects_total  = study$n_subjects_total  %||%
                    tryCatch(sum(sapply(arms, function(a) if (!is.null(a$n_subjects)) a$n_subjects else 0L)), error = function(e) NULL),
                  study_length_days  = study$study_length_days  %||% NULL,
                  study_length_weeks = study$study_length_weeks %||% NULL,
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
                NULL
              })
            }
          }
          
          # Filter out NULL results
          cached_results <- Filter(function(x) !is.null(x), cached_results)

          # Backfill Sheets with locally cached arms — only for tabs not already present
          if (!is.null(study_folder_name)) {
            existing_tabs <- tryCatch(
              googlesheets4::sheet_names(SHEETS_CACHE_SS_ID),
              error = function(e) character(0)
            )
            for (ai in seq_along(arms)) {
              local_rds <- file.path(study_cache_dir, paste0("arm_", ai), "summaries.rds")
              tab <- .sheets_tab_name(safe_name, study_folder_name, study_id, ai)
              if (file.exists(local_rds) && !tab %in% existing_tabs) {
                cat("SHEETS CACHE: Backfilling", tab, "\n")
                sheets_cache_upload(safe_name, readRDS(local_rds), tab)
              }
            }
          }

          return(cached_results)
        }
        
        # L2: Try Google Sheets if local cache missed
        if (!cache_hit && !is.null(study_cache_dir) && !is.null(study_folder_name)) {
          all_arms_in_sheets <- all(sapply(seq_along(arms), function(ai) {
            tab       <- .sheets_tab_name(safe_name, study_folder_name, study_id, ai)
            local_rds <- file.path(study_cache_dir, paste0("arm_", ai), "summaries.rds")
            df <- sheets_cache_download(safe_name, tab)
            if (is.null(df)) return(FALSE)
            dir.create(dirname(local_rds), recursive = TRUE, showWarnings = FALSE)
            saveRDS(as.data.frame(df), local_rds)
            TRUE
          }))
          if (all_arms_in_sheets) {
            writeLines(study_config_hash, file.path(study_cache_dir, "config_hash.txt"))
            cache_hit <- TRUE
            cat("SHEETS CACHE: Study", study_name, "- all arms restored from Sheets\n")
            # Load the just-restored RDS files and return — same path as L1 hit
            cached_results <- list()
            for (arm_idx in seq_along(arms)) {
              arm <- arms[[arm_idx]]
              arm_name <- if (is.list(arm) && !is.null(arm$arm_name)) arm$arm_name else paste("Arm", arm_idx)
              summaries_file <- file.path(study_cache_dir, paste0("arm_", arm_idx), "summaries.rds")
              if (file.exists(summaries_file)) {
                tryCatch({
                  summaries <- readRDS(summaries_file)
                  cached_results[[arm_idx]] <- list(
                    model_name = model_name, study_id = study_id, study_name = study_name,
                    study_data_source = study$study_data_source %||% NULL,
                    study_design      = study$study_design      %||% NULL,
                    population        = study$population        %||% NULL,
                    primary_endpoint  = study$primary_endpoint  %||% NULL,
                    n_subjects_total  = study$n_subjects_total  %||%
                      tryCatch(sum(sapply(arms, function(a) if (!is.null(a$n_subjects)) a$n_subjects else 0L)), error = function(e) NULL),
                    study_length_days  = study$study_length_days  %||% NULL,
                    study_length_weeks = study$study_length_weeks %||% NULL,
                    arm_name = arm_name, arm_idx = arm_idx,
                    dose      = if (is.list(arm) && !is.null(arm$dose))      arm$dose      else 10,
                    frequency = if (is.list(arm) && !is.null(arm$frequency)) arm$frequency else NA_character_,
                    frequency_found = !is.na(if (is.list(arm) && !is.null(arm$frequency)) arm$frequency else NA_character_),
                    results = NULL, sim_pipeline = NULL, output_times = NULL,
                    metadata = meta,
                    data_location = if (is.list(arm) && !is.null(arm$data_location)) arm$data_location else NULL,
                    summaries = summaries, cached = TRUE
                  )
                }, error = function(e) cat("SHEETS CACHE: Error loading arm", arm_idx, "after restore:", e$message, "\n"))
              }
            }
            return(Filter(function(x) !is.null(x), cached_results))
          }
        }

        # ===== CACHE MISS: Run all arms =====
        
        all_study_arm_results <- lapply(seq_along(arms), function(arm_idx) {
        arm <- study$arms[[arm_idx]]
        
        # Ensure arm is a list (defensive check)
        if (!is.list(arm)) {
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
        dosing_days_from_data <- NULL  # To store dosing_days from data file
        
        if (!is.null(data_location) && data_location != "") {
          data_path <- file.path(".", data_location)
          if (file.exists(data_path)) {
            tryCatch({
              obs_raw <- read.csv(data_path, sep = ";", stringsAsFactors = FALSE)
              # Trim whitespace from column names to handle CSV issues
              names(obs_raw) <- trimws(names(obs_raw))
              
              # Find the Time column and get maximum value, accounting for time unit
              if ("Time" %in% names(obs_raw)) {
                # Read time_unit from CSV if available
                time_unit_from_data <- "days"  # default
                if ("Time_unit" %in% names(obs_raw)) {
                  time_unit_from_data <- unique(obs_raw$Time_unit)[1]
                }
                
                max_time <- max(as.numeric(obs_raw$Time), na.rm = TRUE)
                if (!is.na(max_time) && max_time > 0) {
                  # Convert from data time unit to weeks for generate_input_dataset
                  study_length_weeks <- convert_time(max_time, time_unit_from_data, "weeks", "weeks")
                }
              }
              
              # Get the arm's dose and frequency from JSON
              arm_dose <- if (is.list(arm) && !is.null(arm$dose)) arm$dose else NULL
              arm_frequency_json <- if (is.list(arm) && !is.null(arm$frequency)) arm$frequency else NULL
              
              # STEP 1: Filter CSV by dose (always do this first)
              obs_filtered_by_dose <- obs_raw
              if (!is.null(arm_dose) && "Dose_mg" %in% names(obs_raw)) {
                obs_filtered_by_dose <- obs_raw %>%
                  filter(Dose_mg == arm_dose)
                if (nrow(obs_filtered_by_dose) == 0) {
                  obs_filtered_by_dose <- obs_raw
                }
              }
              
              # STEP 2: Filter by frequency (JSON first, then extract from data if needed)
              obs_filtered_by_dose_and_freq <- obs_filtered_by_dose
              
              if (!is.null(arm_frequency_json)) {
                # JSON has frequency - filter CSV by BOTH dose and JSON frequency
                possible_freq_cols <- c("Frequency", "Dose_Frequency", "DoseFrequency", "FREQ")
                freq_col_found <- FALSE
                for (col in possible_freq_cols) {
                  matching_col <- grep(paste0("^", col, "$"), names(obs_filtered_by_dose), ignore.case = TRUE, value = TRUE)
                  if (length(matching_col) > 0) {
                    obs_filtered_by_dose_and_freq <- obs_filtered_by_dose %>%
                      filter(.data[[matching_col[1]]] == arm_frequency_json)
                    if (nrow(obs_filtered_by_dose_and_freq) > 0) {
                      freq_col_found <- TRUE
                      break
                    } else {
                      obs_filtered_by_dose_and_freq <- obs_filtered_by_dose
                    }
                  }
                }
              } else {
                # JSON doesn't have frequency - extract it from CSV and filter by it
                possible_freq_cols <- c("Frequency", "Dose_Frequency", "DoseFrequency", "FREQ")
                for (col in possible_freq_cols) {
                  matching_col <- grep(paste0("^", col, "$"), names(obs_filtered_by_dose), ignore.case = TRUE, value = TRUE)
                  if (length(matching_col) > 0) {
                    freq_val <- unique(obs_filtered_by_dose[[matching_col[1]]])[1]
                    if (!is.na(freq_val) && freq_val != "") {
                      frequency_from_data <- as.character(freq_val)
                      obs_filtered_by_dose_and_freq <- obs_filtered_by_dose %>%
                        filter(.data[[matching_col[1]]] == frequency_from_data)
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
                      break
                    }
                  }
                }
              }
              
              # Extract dosing_days (array of specific days for dose administration)
              dosing_days_from_data <- NULL
              possible_dosing_days_cols <- c("dosing_days", "Dosing_Days", "DosingDays", "dose_days", "Dose_Days")
              for (col in possible_dosing_days_cols) {
                matching_col <- grep(paste0("^", col, "$"), names(obs_filtered_for_metadata), ignore.case = TRUE, value = TRUE)
                if (length(matching_col) > 0) {
                  dosing_days_val <- unique(obs_filtered_for_metadata[[matching_col[1]]])[1]
                  if (!is.na(dosing_days_val) && dosing_days_val != "") {
                    dosing_days_from_data <- as.character(dosing_days_val)
                    break
                  }
                }
              }
            }, error = function(e) {
              NULL
            })
          }
        }
        
        # Create treatment group data frame from arm
        arm_name <- if (is.list(arm) && !is.null(arm$arm_name)) arm$arm_name else paste("Arm", arm_idx)
        
        # Check for dosing_days: JSON > data file > NO DEFAULT
        arm_dosing_days <- if (is.list(arm) && !is.null(arm$dosing_days)) {
          arm$dosing_days
        } else if (!is.null(dosing_days_from_data)) {
          dosing_days_from_data
        } else {
          NULL
        }
        
        # Determine frequency: JSON > data file (if JSON not specified) > NO DEFAULT
        # If JSON has frequency, use it. Otherwise, extract from data for this dose.
        # Do NOT use a default if neither JSON nor data have frequency!
        arm_frequency <- if (is.list(arm) && !is.null(arm$frequency)) {
          arm$frequency
        } else if (!is.null(frequency_from_data)) {
          frequency_from_data
        } else {
          NA_character_  # Use NA instead of a default
        }
        
        arm_frequency_found <- !is.na(arm_frequency)  # Track whether frequency was actually found
        
        # Determine n_subjects: data file > JSON > default
        arm_n_subjects <- if (!is.null(n_subjects_from_data)) {
          n_subjects_from_data
        } else if (is.list(arm) && !is.null(arm$n_subjects)) {
          arm$n_subjects
        } else {
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
        
        # Create treatment group data frame from arm
        treatment_groups_df <- data.frame(
          GroupName = arm_name,
          SampleSize = arm_n_subjects,
          Treatment = arm_name,
          Dose = if (is.list(arm) && !is.null(arm$dose)) arm$dose else 10,
          Frequency = arm_frequency,
          Compartment = meta$input_compartment[1],
          stringsAsFactors = FALSE
        )
        
        # Add DosingDays column if dosing_days are provided
        if (!is.null(arm_dosing_days)) {
          treatment_groups_df$DosingDays <- as.character(arm_dosing_days)
        }
        
        # Add Addl column if we have no_doses information (only if not using DosingDays)
        if (!is.null(arm_no_doses) && is.null(arm_dosing_days)) {
          treatment_groups_df$Addl <- as.numeric(arm_no_doses) - 1
        }
        
        # Generate input dataset
        tryCatch({
          input_dataset <- generate_input_dataset(
            treatment_groups = treatment_groups_df,
            study_length_weeks = study_length_weeks,
            design = "parallel",
            washout_weeks = 0,
            model_time_unit = meta$model_time_unit,
            input_compartment = meta$input_compartment[1],
            dose_unit = paste(meta$dose_unit, collapse = ","),
            model_dose_unit = paste(meta$model_dose_unit, collapse = ","),
            molecular_weight_Da = meta$molecular_weight_Da
          )
          
          # Verify addl values are correctly set
          if ("addl" %in% names(input_dataset)) {
            unique_addl <- unique(input_dataset$addl)
          }
        }, error = function(e) {
          stop(e)
        })
        
        # Run single study - apply zero_re() only for single individual
        tryCatch({
          # Build simulation pipeline - apply zero_re() only if single individual
          sim_pipeline <- if (arm_n_subjects == 1) {
            mod %>%
              zero_re() %>%
              data_set(input_dataset) %>%
              carry_out(GroupName)
          } else {
            mod %>%
              data_set(input_dataset) %>%
              carry_out(GroupName)
          }
          
          # Generate specific output times for smooth plotting (every 12 hours)
          sim_end <- convert_time(study_length_weeks, "weeks", meta$model_time_unit, meta$model_time_unit)
          time_interval <- convert_time(12, "hours", meta$model_time_unit, meta$model_time_unit)
          output_times <- seq(0, sim_end, by = time_interval)
          
          results <- sim_pipeline %>%
            mrgsim(tgrid = output_times) %>%
            as.data.frame()
          
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
              sheets_cache_upload(
                safe_name    = safe_name,
                summaries_df = summaries,
                tab_name     = .sheets_tab_name(safe_name, study_folder_name, study_id, arm_idx)
              )
            }, error = function(e) {
              NULL
            })
          }
          
          list(
            model_name = model_name,
            study_id = study_id,
            study_name = study_name,
            study_data_source = study$study_data_source %||% NULL,
            study_design      = study$study_design      %||% NULL,
            population        = study$population        %||% NULL,
            primary_endpoint  = study$primary_endpoint  %||% NULL,
            n_subjects_total  = study$n_subjects_total  %||%
              tryCatch(sum(sapply(arms, function(a) if (!is.null(a$n_subjects)) a$n_subjects else 0L)), error = function(e) NULL),
            study_length_days  = study$study_length_days  %||% NULL,
            study_length_weeks = study$study_length_weeks %||% NULL,
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
        })
      }
      
      # Filter out NULL results
      all_study_arm_results <- Filter(function(x) !is.null(x), all_study_arm_results)
      return(all_study_arm_results)
    })  # Close study lapply
    })  # Close model lapply
    
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
      return()
    }
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
          }
          
          # USE pre-computed summaries from initialization
          summaries <- val_res$summaries
          
          if (is.null(summaries) || nrow(summaries) == 0) {
            # Fallback: try to compute summaries if they're missing (only for non-cached results)
            if (!is_cached && !is.null(val_res$sim_pipeline) && !is.null(val_res$output_times) && !is.null(n_individuals)) {
              summaries <- compute_validation_summaries(
                sim_pipeline = val_res$sim_pipeline,
                output_times = val_res$output_times,
                meta = meta,
                n_subjects = n_individuals,
                time_unit = meta$model_time_unit
              )
            }
            if (is.null(summaries)) {
              return(ggplotly(ggplot() + theme_minimal() + ggtitle("No simulation data available")))
            }
          }
          
          # Convert time from model units to user's selected display unit (ALWAYS do this)
          if (nrow(summaries) > 0 && "time" %in% names(summaries)) {
            summaries <- summaries %>%
              mutate(time = convert_time(time, meta$model_time_unit, input$time_unit, meta$model_time_unit))
          }
          
          # Load observed data if available
          observed_data <- NULL
          observed_transformation <- NULL
          
          label_df <- data.frame(variable = meta$output_var, label = meta$output_label)
          if (!"label" %in% names(summaries) && "variable" %in% names(summaries)) {
            summaries <- left_join(summaries, label_df, by = "variable")
          }
          
          if (!is.null(data_location) && data_location != "") {
            data_path <- file.path(".", data_location)
            if (file.exists(data_path)) {
              tryCatch({
                # Read the CSV file with semicolon separator
                obs_raw <- read.csv(data_path, sep = ";", stringsAsFactors = FALSE) %>%
                  as_tibble()
                # Trim whitespace from column names
                names(obs_raw) <- trimws(names(obs_raw))
                
                # Extract the DV_unit to determine transformation needed
                if ("DV_unit" %in% names(obs_raw)) {
                  observed_transformation <- unique(obs_raw$DV_unit)[1]
                }
                
                # Find the output variable column (DV_CHR or similar)
                output_col <- NA
                possible_cols <- c("DV_CHR", "Output", "Variable")
                for (col in possible_cols) {
                  if (col %in% names(obs_raw)) {
                    output_col <- col
                    break
                  }
                }
                
                # If not found, use second to last column
                if (is.na(output_col)) {
                  output_col <- names(obs_raw)[ncol(obs_raw) - 1]
                }
                
                # Get the value column (DV or similar)
                value_col <- "DV"
                if (!("DV" %in% names(obs_raw))) {
                  if (ncol(obs_raw) >= 3) {
                    value_col <- names(obs_raw)[3]
                  } else {
                    value_col <- names(obs_raw)[2]
                  }
                }
                
                # Extract relevant columns and rename
                if ("Time" %in% names(obs_raw) && value_col %in% names(obs_raw) && output_col %in% names(obs_raw)) {
                  
                  # Read time_unit from CSV if available
                  time_unit_from_data <- "days"  # default
                  if ("Time_unit" %in% names(obs_raw)) {
                    time_unit_from_data <- unique(obs_raw$Time_unit)[1]
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
                  
                  # Match to our output variables - try exact match first against variable names
                  # Match CSV output_var against summaries$variable (actual model outputs), not labels
                  observed_data_exact <- observed_data %>%
                    filter(output_var %in% unique(summaries$variable))
                  
                  if (nrow(observed_data_exact) > 0) {
                    # Join with label_df to add the display label
                    observed_data <- observed_data_exact %>%
                      left_join(label_df, by = c("output_var" = "variable")) %>%
                      select(-output_var)
                  } else {
                    
                    observed_data <- obs_raw %>%
                      select(Time = "Time", 
                             observed_value = all_of(value_col),
                             output_var = all_of(output_col),
                             any_of(c("Frequency", "Dose_mg"))) %>%
                      mutate(
                        Time = as.numeric(Time),
                        observed_value = as.numeric(observed_value),
                        Time = convert_time(Time, time_unit_from_data, input$time_unit, time_unit_from_data),
                        # Try to match against actual model variable names (not labels)
                        # Strip common prefixes (DV_, DV) to get the core variable name
                        core_var = gsub("^DV_|^DV", "", output_var, ignore.case = TRUE),
                        matched_var = sapply(core_var, function(var) {
                          # Try to match against summaries$variable (actual model outputs)
                          direct_matches <- summaries$variable[grepl(var, summaries$variable, ignore.case = TRUE)]
                          
                          if (length(direct_matches) > 0) {
                            return(direct_matches[1])
                          }
                          
                          # If no direct match, try percent change variants
                          is_pct_change <- grepl("pct_change|percent_change|_pct|_pct_chg", var, ignore.case = TRUE)
                          if (is_pct_change) {
                            base_var <- gsub("_pct_change|_percent_change|_pct_chg|_pct", "", var, ignore.case = TRUE)
                            pct_matches <- summaries$variable[grepl(base_var, summaries$variable, ignore.case = TRUE)]
                            if (length(pct_matches) > 0) {
                              return(pct_matches[1])
                            }
                          }
                          NA_character_
                        })
                      ) %>%
                      filter(!is.na(matched_var)) %>%
                      left_join(label_df, by = c("matched_var" = "variable")) %>%
                      select(-output_var, -core_var, -matched_var)
                  }
                  
                  # No debug output - data extracted successfully
                } else {
                  observed_data <- NULL
                }
              }, error = function(e) {
                NULL
              })
            } else {
              observed_data <- NULL
            }
          } else {
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
              observed_data <- observed_data %>%
                filter(Dose_mg == val_res$dose)
            }
            
            # Filter by frequency ONLY if frequency was actually found (not a default)
            if (nrow(observed_data) > 0 && 
                val_res$frequency_found && 
                !is.na(val_res$frequency) &&
                ("Frequency" %in% names(observed_data) || "Dose_Frequency" %in% names(observed_data))) {
              freq_col <- if ("Frequency" %in% names(observed_data)) "Frequency" else "Dose_Frequency"
              before_freq_filter <- nrow(observed_data)
              observed_data <- observed_data %>%
                filter(get(freq_col) == val_res$frequency)
            }
            
            # If after filtering we have no data, set to NULL so we don't filter summaries
            if (nrow(observed_data) == 0) {
              observed_data <- NULL
            }
          }

          
          # Filter summaries to ONLY include variables present in observed data
          # Match on actual variable names, not display labels
          if (!is.null(observed_data) && nrow(observed_data) > 0) {
            summaries_filtered <- summaries %>%
              filter(label %in% unique(observed_data$label))
            
            if (nrow(summaries_filtered) > 0) {
              summaries <- summaries_filtered
            } else {
              return(ggplotly(
                ggplot() + 
                  geom_text(aes(x = 0.5, y = 0.5, label = "Validation data variables do not match model outputs"), 
                           hjust = 0.5, vjust = 0.5) +
                  theme_minimal() +
                  xlim(0, 1) + ylim(0, 1) +
                  theme(axis.title = element_blank(), axis.text = element_blank())
              ))
            }
            summaries <- summaries %>% filter(!is.na(label))
          }
          
          time_label <- switch(input$time_unit,
            "hours" = "Time (hours)",
            "days" = "Time (days)",
            "weeks" = "Time (weeks)",
            "months" = "Time (months)"
          )
          
          # CRITICAL CHECK: Do we have any data to plot?
          if (is.null(summaries) || nrow(summaries) == 0) {
            return(ggplotly(
              ggplot() + 
                geom_text(aes(x = 0.5, y = 0.5, label = paste("No simulation data for", arm_name)), 
                         hjust = 0.5, vjust = 0.5) +
                theme_minimal() +
                xlim(0, 1) + ylim(0, 1) +
                theme(axis.title = element_blank(), axis.text = element_blank())
            ))
          }
          
          # Build the plot
          g <- ggplot(summaries, aes(x = time, y = median)) +
            geom_line(aes(color = "Predicted", linetype = "Predicted"), size = 1)
          
          # Add confidence interval ribbon only if CIs are available (n > 1 individual)
          if (!all(is.na(summaries$lower)) && !all(is.na(summaries$upper))) {
            g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#337ab7")
          }
          
          # Add observed data points if available
          # observed_data already has the correct labels from the join above
          if (!is.null(observed_data) && nrow(observed_data) > 0) {
            # observed_data is already filtered to matching labels via the join
            observed_data_to_plot <- observed_data
            
            if (nrow(observed_data_to_plot) > 0) {
              g <- g + geom_point(data = observed_data_to_plot, 
                               aes(x = Time, y = observed_value, color = "Observed"),
                               size = 3, shape = 16, alpha = 0.7)
            }
          }
          
          g <- g +
            facet_wrap(~ label, scales = "free_y") +
            labs(
              title = arm_name,
              x = time_label,
              y = "Value"
            ) +
            scale_color_manual(
              name   = "Data Source",
              values = c("Predicted" = "#2563eb", "Observed" = "#e74c3c"),
              breaks = c("Predicted", "Observed")
            ) +
            scale_linetype_manual(
              values = c("Predicted" = "solid", "Observed" = "blank"),
              breaks = c("Predicted", "Observed"),
              guide  = "none"
            ) +
            theme_bw() +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
              legend.position = "bottom"
            )
          
          p <- ggplotly(g)
          # ggplotly creates one legendgroup per scale (color + linetype), both titled
          # "Data Source" → two headers. Fix: drop legendgroups entirely and deduplicate
          # by trace name so only "Predicted" and "Observed" appear once each.
          seen_names <- c()
          for (i in seq_along(p$x$data)) {
            # Strip "(Name,N)" suffix added by ggplotly
            nm <- p$x$data[[i]]$name
            if (!is.null(nm) && nzchar(nm)) {
              p$x$data[[i]]$name <- sub("^\\((.+),\\d+\\)$", "\\1", nm)
            }
            # Remove legend grouping so no group title is rendered
            p$x$data[[i]]$legendgroup      <- NULL
            p$x$data[[i]]$legendgrouptitle <- NULL
            # Keep only the first trace per name in the legend
            clean_nm <- p$x$data[[i]]$name
            if (!is.null(clean_nm) && nzchar(clean_nm)) {
              if (clean_nm %in% seen_names) {
                p$x$data[[i]]$showlegend <- FALSE
              } else {
                seen_names <- c(seen_names, clean_nm)
              }
            }
          }
          p
        })
      })
    }
  })
  
  # Display validation results
  # Helper: normalise a source string to a clickable URL
  .normalise_source_url <- function(src) {
    if (is.null(src) || nchar(trimws(src)) == 0) return(NULL)
    src <- trimws(src)
    # Strip common DOI prefixes and build full URL
    doi_clean <- sub("^(?:DOI:\\s*|doi:\\s*|https?://doi\\.org/)", "", src, ignore.case = TRUE)
    if (grepl("^10\\.", doi_clean)) {
      paste0("https://doi.org/", doi_clean)
    } else if (grepl("^https?://", src, ignore.case = TRUE)) {
      src
    } else {
      NULL  # Unrecognised format — don't linkify
    }
  }

  # Model info banner — single header across all loaded models (fills the orphaned validation_ui slot)
  output$validation_ui <- renderUI({
    # Collect banner entries for every model that has at least one display field
    banners <- lapply(models_metadata, function(meta) {
      has_any <- any(!is.null(c(meta$display_name, meta$description,
                                meta$clinical_application, meta$source)))
      if (!has_any) return(NULL)

      name_text   <- meta$display_name %||% tools::file_path_sans_ext(basename(meta$filename))
      source_url  <- .normalise_source_url(meta$source)

      div(
        style = paste0(
          "background: #f0f6ff; border: 1px solid #bfdbfe; border-radius: 10px; ",
          "padding: 18px 22px; margin-bottom: 12px;"
        ),
        tags$h5(
          name_text,
          style = "font-weight: 700; color: #1e3a5f; margin: 0 0 8px 0; font-size: 1.05em;"
        ),
        if (!is.null(meta$description))
          tags$p(meta$description,
            style = "color: #374151; font-size: 0.92em; margin: 0 0 6px 0; line-height: 1.55;"),
        if (!is.null(meta$clinical_application))
          tags$p(
            tags$span("Clinical use: ", style = "font-weight: 600; color: #1e3a5f;"),
            meta$clinical_application,
            style = "color: #374151; font-size: 0.92em; margin: 0 0 6px 0; line-height: 1.55;"
          ),
        if (!is.null(source_url))
          tags$p(
            tags$span("Model reference: ", style = "font-weight: 600; color: #1e3a5f;"),
            tags$a(meta$source, href = source_url, target = "_blank",
              style = "color: #2563eb; text-decoration: underline; font-size: 0.92em;"),
            style = "margin: 0;"
          )
      )
    })

    banners <- Filter(Negate(is.null), banners)
    if (length(banners) == 0) return(NULL)

    div(
      style = "margin-bottom: 20px;",
      tagList(banners)
    )
  })

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

      # Pull study-level metadata from first arm in this group
      first_res <- val_results[[result_indices[1]]]
      study_src_url <- .normalise_source_url(first_res$study_data_source)

      # Study duration label
      study_duration_label <- NULL
      if (!is.null(first_res$study_length_weeks)) {
        study_duration_label <- paste0(first_res$study_length_weeks, " weeks")
      } else if (!is.null(first_res$study_length_days)) {
        study_duration_label <- paste0(first_res$study_length_days, " days")
      }

      # Patient count
      n_total <- first_res$n_subjects_total

      # Add model heading (display_name form)
      result_elements[[length(result_elements) + 1]] <-
        h3(model_name, style = "margin-top: 30px; color: #1e3a5f; font-weight: 700;")

      # Add enriched study card
      result_elements[[length(result_elements) + 1]] <- div(
        style = paste0(
          "background: #fafafa; border: 1px solid #e2e8f0; border-left: 4px solid #3b82f6; ",
          "border-radius: 8px; padding: 14px 18px; margin: 6px 0 18px 0;"
        ),
        tags$h5(study_name,
          style = "font-weight: 700; color: #1e293b; margin: 0 0 8px 0; font-size: 0.98em;"),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 14px; font-size: 0.88em; color: #475569; margin-bottom: 6px;",
          if (!is.null(study_duration_label))
            tags$span(tags$b("Duration: "), study_duration_label),
          if (!is.null(n_total) && n_total > 0)
            tags$span(tags$b("Patients: "), as.character(n_total)),
          if (!is.null(study_src_url))
            tags$span(tags$b("Reference: "),
              tags$a(first_res$study_data_source, href = study_src_url, target = "_blank",
                style = "color: #2563eb; text-decoration: underline;"))
        ),
        if (!is.null(first_res$study_design))
          tags$p(tags$b("Study design: "), first_res$study_design,
            style = "font-size: 0.88em; color: #475569; margin: 4px 0 0 0;"),
        if (!is.null(first_res$population))
          tags$p(tags$b("Population: "), first_res$population,
            style = "font-size: 0.88em; color: #475569; margin: 4px 0 0 0;"),
        if (!is.null(first_res$primary_endpoint))
          tags$p(tags$b("Primary endpoint: "), first_res$primary_endpoint,
            style = "font-size: 0.88em; color: #475569; margin: 4px 0 0 0;")
      )

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
    validation_type(input$validation_type_selector)
    
    # Update displayed results based on selection
    if (input$validation_type_selector == "internal") {
      validation_results(internal_validation_results())
    } else if (input$validation_type_selector == "external") {
      validation_results(external_validation_results())
    }
    
    # Ensure metadata is still available
    if (is.null(validation_metadata())) {
      validation_metadata(list(n_studies = 1))
    }
  })
  
  # Pre-compute validation results on app initialization
  observeEvent(TRUE, {
    # Check which models have what validation data
    for (i in seq_len(n_models)) {
      meta <- models_metadata[[i]]
      model_name <- meta$display_name %||% tools::file_path_sans_ext(basename(meta$filename))
      has_int <- !is.null(meta$internal_validation_data)
      has_ext <- !is.null(meta$external_validation_data)
    }
    
    # Check if models have validation data
    has_internal <- any(sapply(models_metadata, function(m) !is.null(m$internal_validation_data)))
    has_external <- any(sapply(models_metadata, function(m) !is.null(m$external_validation_data)))
    
    if (has_internal) {
      tryCatch({
        run_validation_simulations("internal")
      }, error = function(e) {
        NULL
      })
    }
    
    if (has_external) {
      tryCatch({
        run_validation_simulations("external")
      }, error = function(e) {
        NULL
      })
    }
    
    # Ensure internal validation is displayed by default
    if (has_internal) {
      validation_results(internal_validation_results())
    } else if (has_external) {
      validation_results(external_validation_results())
    }
  }, once = TRUE)

  # Hide sidebar and expand main content when on Help & Guide or Model Evidence tabs
  observeEvent(input$cts_main_tabset, {
    current_tab <- input$cts_main_tabset
    
    # Show sidebar only for "Virtual Trial Results" tab, and only if user hasn't hidden it
    if (current_tab == "Virtual Trial Results") {
      if (sidebar_visible()) {
        shinyjs::removeClass(id = "cts_sidebar_col", class = "sidebar-hidden")
        shinyjs::removeClass(id = "cts_main_col", class = "full-width")
        shinyjs::removeClass(id = "cts_main_col", class = "expanded")
      }
    } else {
      # Hide sidebar for "Help & Guide", "Model Evidence", or any other tab
      shinyjs::addClass(id = "cts_sidebar_col", class = "sidebar-hidden")
      shinyjs::addClass(id = "cts_main_col", class = "full-width")
    }
  }, ignoreInit = TRUE)
  
  # ========== FEEDBACK SUBMISSION ==========
  collect_feedback_response <- function(input) {
    data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      user_role = input$fb_user_role %||% "Not answered",
      
      # Section 1: Workflow & Navigation
      q1_intuitive = input$fb_q1_intuitive %||% NA,
      q1_comments = input$fb_q1_comments %||% "",
      q2_flow = input$fb_q2_flow %||% NA,
      q2_comments = input$fb_q2_comments %||% "",
      
      # Section 2: Question-First Workflow
      q3_helpful = input$fb_q3_helpful %||% NA,
      q3_comments = input$fb_q3_comments %||% "",
      q4_guide = input$fb_q4_guide %||% NA,
      
      # Section 3: Trial Design
      q5_design_clear = input$fb_q5_design_clear %||% NA,
      q5_comments = input$fb_q5_comments %||% "",
      q6_missing = input$fb_q6_missing %||% "",
      
      # Section 4: Results & Visualization
      q7_plots_clear = input$fb_q7_plots_clear %||% NA,
      q7_comments = input$fb_q7_comments %||% "",
      q8_interpret = input$fb_q8_interpret %||% NA,
      q8_comments = input$fb_q8_comments %||% "",
      q9_confidence = input$fb_q9_confidence %||% NA,
      
      # Section 5: Model Evidence
      q10_validation = input$fb_q10_validation %||% NA,
      q10_comments = input$fb_q10_comments %||% "",
      
      # Section 6: Help & Documentation
      q11_help_useful = input$fb_q11_help_useful %||% NA,
      q11_comments = input$fb_q11_comments %||% "",
      q12_explanations = input$fb_q12_explanations %||% NA,
      
      # Section 7: Overall Experience
      q13_again = input$fb_q13_again %||% NA,
      q14_recommend = input$fb_q14_recommend %||% NA,
      q15_features = input$fb_q15_features %||% "",
      
      stringsAsFactors = FALSE
    )
  }
  
  observeEvent(input$submit_feedback, {
    # Collect feedback data
    fb_data <- collect_feedback_response(input)
    
    # Validate: require at least one substantive response
    has_response <- (
      !is.na(fb_data$q1_intuitive) ||
      !is.na(fb_data$q2_flow) ||
      !is.na(fb_data$q3_helpful) ||
      nchar(fb_data$q1_comments) > 0 ||
      nchar(fb_data$q2_comments) > 0 ||
      nchar(fb_data$q5_comments) > 0 ||
      nchar(fb_data$q6_missing) > 0 ||
      nchar(fb_data$q7_comments) > 0 ||
      nchar(fb_data$q15_features) > 0
    )
    
    if (!has_response) {
      shinyalert::shinyalert(
        title = "Feedback Required",
        text = "Please provide at least one response (rating or comment) before submitting.",
        type = "warning"
      )
      return()
    }
    
    # Upload to Google Sheets
    success <- feedback_sheet_upload(fb_data)
    
    if (success) {
      shinyalert::shinyalert(
        title = "Thank You!",
        text = "Your feedback has been saved successfully.",
        type = "success"
      )
      feedback_submitted(TRUE)
      
      # Reset form after 1.5 seconds
      shinyjs::delay(1500, {
        shinyjs::reset("feedback_form")
        feedback_submitted(FALSE)
      })
    } else {
      shinyalert::shinyalert(
        title = "Error",
        text = "Could not save feedback. Please check your internet connection and try again.",
        type = "error"
      )
    }
  })
  # ===================================================
}