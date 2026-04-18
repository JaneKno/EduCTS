# QuestionLibrary.R
# Question-first workflow for CTS app
# MIDD-aligned hierarchy: Development Phase → MIDD Approach → Clinical Question → Model Suggestions
# All model matching is data-driven from JSON metadata (therapeutic_area, indication, applications fields)
#
# HIERARCHY:
#   Therapeutic Area (pre-filter from meta$therapeutic_area)
#   └── Development Phase  [4 phases, simplified MIDD lifecycle]
#       └── MIDD Approach  [7 approaches, methodological framing]
#           └── Clinical Question  [13 questions, linked to trial presets + keyword matching]

# ============================================================================
# PHASES AND APPROACHES
# ============================================================================
# 4-level simplified MIDD drug development lifecycle

phases <- c(
  "Early Development",
  "Dose Optimisation",
  "Confirmatory (Phase III)",
  "Post-Marketing"
)

# Approaches per phase — the MIDD methodological framing
phase_approaches <- list(
  "Early Development"           = c("PK Characterization", "Mechanistic Exploration"),
  "Dose Optimisation"           = c("Dose-Response Modeling", "Regimen Optimization", "Population PK-PD"),
  "Confirmatory (Phase III)"    = c("Comparative Effectiveness", "Treatment Switching"),
  "Post-Marketing"              = c("Long-term Outcomes")
)

# ============================================================================
# PHASE DESCRIPTIONS
# ============================================================================
phase_descriptions <- list(
  "Early Development" =
    "Characterize the drug's PK/PD properties and understand disease mechanisms
    to inform early dose selection and identify key drivers of response.",

  "Dose Optimisation" =
    "Identify the optimal dose and dosing regimen using dose-response modeling and
    population PK-PD analysis to support Phase IIb dose-selection decisions.",

  "Confirmatory (Phase III)" =
    "Design and simulate confirmatory Phase III trials, including comparative effectiveness
    analysis, treatment switch protocols, and trial power assessment.",

  "Post-Marketing" =
    "Assess long-term treatment durability, relapse risk after de-escalation, and
    optimise treatment strategies in the real-world post-approval setting."
)

# ============================================================================
# APPROACH DESCRIPTIONS
# ============================================================================
approach_descriptions <- list(
  "PK Characterization" =
    "Characterize population PK/PD variability and identify key drivers of drug
    exposure across the target patient population. Supports early dose-range finding.",

  "Mechanistic Exploration" =
    "Use QSP, systems biology, or disease-progression models to understand underlying
    disease mechanisms, test biological hypotheses, and inform indication selection.",

  "Dose-Response Modeling" =
    "Quantify dose-response relationships and exposure-response variability to
    estimate the minimum effective dose. Supports go/no-go decisions for Phase III
    dose selection.",

  "Regimen Optimization" =
    "Compare dosing schedules (daily, weekly, monthly, quarterly) on biomarker
    profiles, accumulation, and trough levels. Identify the most patient-friendly
    regimen that balances efficacy and tolerability.",

  "Population PK-PD" =
    "Characterize inter-individual variability in PK and PD response. Identify patient
    covariates (age, weight, disease stage, biomarkers) that predict clinical outcomes
    and define responder subgroups.",

  "Comparative Effectiveness" =
    "Simulate head-to-head comparisons between two or more compounds in a parallel-group
    trial to assess relative efficacy, time-to-target, and biomarker profiles. Requires
    models from the same therapeutic area.",

  "Treatment Switching" =
    "Evaluate treatment policies involving mid-trial switches between compounds.
    Supports stepped-care, rescue therapy, and dose-escalation protocol design.
    Simulates rebound dynamics and biomarker trajectory after switching.",

  "Long-term Outcomes" =
    "Project biomarker and clinical outcomes over months to years. Quantify treatment
    durability, time-to-progression, and relapse risk after dose de-escalation.
    Informs post-marketing surveillance and label discussions."
)

# ============================================================================
# QUESTIONS DATA FRAME
# ============================================================================
# Flat data frame: phase × approach × question → model keywords + trial presets
# 13 questions across 4 phases and 7 approaches

questions <- data.frame(
  phase = c(
    # Early Development
    "Early Development",
    "Early Development",
    # Dose Optimisation
    "Dose Optimisation",
    "Dose Optimisation",
    "Dose Optimisation",
    "Dose Optimisation",
    "Dose Optimisation",
    # Confirmatory (Phase III)
    "Confirmatory (Phase III)",
    "Confirmatory (Phase III)",
    "Confirmatory (Phase III)",
    "Confirmatory (Phase III)",
    # Post-Marketing
    "Post-Marketing",
    "Post-Marketing"
  ),

  approach = c(
    "PK Characterization",
    "Mechanistic Exploration",
    "Dose-Response Modeling",
    "Dose-Response Modeling",
    "Regimen Optimization",
    "Population PK-PD",
    "Population PK-PD",
    "Comparative Effectiveness",
    "Comparative Effectiveness",
    "Treatment Switching",
    "Treatment Switching",
    "Long-term Outcomes",
    "Long-term Outcomes"
  ),

  question_id = c(
    "q_pk_char",
    "q_mech_trajectory",
    "q_dose_min",
    "q_dose_resp",
    "q_dose_freq",
    "q_pop_variability",
    "q_pop_responder",
    "q_comp_efficacy_1",
    "q_comp_efficacy_2",
    "q_switch_1",
    "q_switch_2",
    "q_long_trajectory",
    "q_long_relapse"
  ),

  question = c(
    "How variable is drug exposure and response across the patient population?",
    "What mechanistic factors drive long-term biomarker dynamics?",
    "What is the minimum effective dose?",
    "How variable is dose-response across patient populations?",
    "What is the optimal dosing frequency?",
    "How variable is response across patients?",
    "Which patient characteristics drive clinical response?",
    "How do two compounds compare head-to-head in efficacy?",
    "Which compound achieves the target endpoint faster?",
    "What happens when patients switch from one compound to another?",
    "What is the optimal time to switch treatments?",
    "What is the expected long-term biomarker trajectory?",
    "What is the relapse risk after dose de-escalation?"
  ),

  # Keywords matched against 'applications' JSON field
  # Validated against models/ JSON files (therapeutic_area + applications fields)
  keyword_patterns = c(
    "population PK|mechanistic PK|PK analysis|dose.?select|clinical trial",         # PK Characterization
    "mechanistic|QSP|systems biology|hypothesis|disease model|disease progression",  # Mechanistic Exploration
    "dose.?select|dose.?optim|dose.?response",                                       # Min effective dose
    "dose.?response|population|variability",                                         # Dose-response variability
    "dose.?select|clinical trial|dosing",                                            # Optimal frequency
    "population|subpopulation|variability|PK.?PD|stratification",                   # Population variability
    "population|biomarker|stratification|patient|exposure.?response",               # Responder characteristics
    "clinical trial|dose.?select",                                                   # Comparative efficacy (1)
    "clinical trial|dose.?select",                                                   # Comparative efficacy (2)
    "treatment switch|Treatment switching",                                          # Treatment switching
    "treatment switch|Treatment switching",                                          # Optimal switch time
    "modeling|progression|disease|trajectory|long.?term|mechanistic",               # Long-term trajectory
    "switching|de.?escalat|disease progression|trajectory"                          # Relapse risk
  ),

  require_multi_model = c(
    FALSE, FALSE,          # Early Development
    FALSE, FALSE, FALSE,   # Dose Optimisation
    FALSE, FALSE,
    TRUE,  TRUE,           # Confirmatory — comparative: need 2 models
    TRUE,  TRUE,           # Confirmatory — switching: need 2 models
    FALSE, FALSE           # Post-Marketing
  ),

  trial_design = rep("parallel", 13),

  n_arms = c(
    2, 1,
    3, 2, 2, 2, 2,
    2, 2,
    2, 2,
    1, 2
  ),

  enable_switch = c(
    FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE,
    TRUE,  TRUE,
    FALSE, TRUE
  ),

  suggested_weeks = c(
    24, 52,
    24, 52, 24, 52, 52,
    52, 52,
    52, 52,
    104, 52
  ),

  stringsAsFactors = FALSE
)

# ============================================================================
# CASCADE HELPER FUNCTIONS
# ============================================================================

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Returns vector of all development phases
get_phases <- function() phases

# Returns MIDD approaches available for a given development phase
get_approaches_for_phase <- function(phase) {
  if (is.null(phase) || phase == "") return(character(0))
  phase_approaches[[phase]] %||% character(0)
}

# Returns questions data frame filtered to a specific phase + approach
get_questions_for_approach <- function(phase, approach) {
  if (is.null(phase) || phase == "" || is.null(approach) || approach == "") {
    return(questions[0, ])
  }
  questions[questions$phase == phase & questions$approach == approach, ]
}

# ============================================================================
# QUESTION DESCRIPTIONS
# ============================================================================
# Contextual help text shown in sidebar when question is selected

question_descriptions <- list(
  q_pk_char =
    "Simulate population PK/PD to characterize how drug exposure and response
    varies across individual patients. Identify key sources of variability
    (body weight, renal function, age, baseline biomarker) and inform
    early dose-range finding studies.",

  q_mech_trajectory =
    "Use mechanistic or systems biology models to understand disease biological
    dynamics and how drug intervention alters long-term biomarker trajectories.
    Ideal for hypothesis generation and early indication exploration.",

  q_dose_min =
    "Find the minimum dose that achieves a predefined efficacy threshold or
    biomarker target. Simulate multiple dose arms and identify the lowest
    dose meeting your efficacy criterion across the population.",

  q_dose_resp =
    "Quantify the dose-response relationship and inter-individual variability
    across a full dose range. Characterize the distribution of PD responses
    in a virtual patient population to inform Phase III dose selection.",

  q_dose_freq =
    "Compare dosing schedules — daily, weekly, monthly, quarterly — on
    biomarker profile, accumulation, and trough levels. Identify the optimal
    frequency balancing efficacy, tolerability, and patient adherence.",

  q_pop_variability =
    "Characterize how PK and PD response varies across patient subgroups.
    Simulate a representative virtual population to quantify the proportion
    of high, moderate, and low responders under the proposed dosing regimen.",

  q_pop_responder =
    "Identify patient characteristics (age, weight, baseline biomarker, disease
    stage) that predict clinical response. Correlate individual PK/PD parameters
    with efficacy outcomes to support patient stratification and enrichment design.",

  q_comp_efficacy_1 =
    "Compare two treatment compounds head-to-head in a parallel-group simulated trial.
    Each subject receives one treatment; efficacy is measured at the same endpoints.
    Requires models from the same therapeutic area and indication.",

  q_comp_efficacy_2 =
    "Identify which compound reaches the defined clinical target (e.g. 50% reduction
    in biomarker) faster. Compare time-to-response distributions and milestone
    achievement probabilities across two parallel simulated treatment arms.",

  q_switch_1 =
    "Evaluate the effect of switching patients from one treatment to another at a
    predefined time point. Simulate washout dynamics, biomarker rebound, and
    response trajectory after the switch. Requires two compatible models.",

  q_switch_2 =
    "Determine the optimal timing for switching between compounds. Simulate multiple
    switch scenarios (e.g., week 12 vs week 24 vs week 52) and compare long-term
    biomarker trajectories and responder rates to find the optimal switch window.",

  q_long_trajectory =
    "Project biomarker and clinical outcomes over the long term (months to years).
    Assess treatment durability, time-to-progression, and need for re-dosing.
    Supports long-term extension study planning and Phase III duration decisions.",

  q_long_relapse =
    "Quantify the risk of disease relapse or biomarker rebound following dose
    reduction or treatment discontinuation. Simulate de-escalated dose arms and
    compare relapse trajectories to support label discussions and follow-up strategy."
)

# (category_descriptions removed — replaced by phase_descriptions + approach_descriptions)

# ============================================================================
# HELPER FUNCTION: Filter models by question
# ============================================================================
# Used by ModelLibrary.R to suggest models for a selected question
#
# FILTERING LOGIC:
#   1. (Optional) Therapeutic area pre-filter — narrows to selected TA
#   2. Keyword grep on 'applications' JSON field
#   3. For multi-model questions: group by therapeutic_area + output variables;
#      only return groups with 2+ compatible models so meaningful comparison is possible
#   4. Sort: Validated > Partially validated > Not validated, then by year descending

# Extract PD (pharmacodynamic, non-PK) output variables from a comma-separated string.
# Drops generic compartment concentration outputs (_out suffix, cp, cc, ce).
get_pd_vars <- function(s) {
  vars <- tolower(trimws(strsplit(s, ",\\s*")[[1]]))
  vars[!grepl("_out$|^cp$|^cc$|^ce$", vars)]
}

filter_models_by_question <- function(metadata, question_row, ta_filter = NULL, require_match = TRUE) {
  if (require_match && nrow(metadata) == 0) {
    return(metadata[0, ])
  }

  # Step 1: Optional therapeutic area pre-filter
  if (!is.null(ta_filter) && ta_filter != "" && ta_filter != "All Therapeutic Areas") {
    metadata <- metadata[grepl(ta_filter, metadata$therapeutic_area, ignore.case = TRUE), ]
  }
  if (nrow(metadata) == 0) return(metadata)

  # Step 2: Keyword filter on 'applications' field
  keyword_pattern <- question_row$keyword_patterns[1]
  keyword_match   <- grepl(keyword_pattern, metadata$applications, ignore.case = TRUE)
  matches <- metadata[keyword_match, ]

  if (nrow(matches) == 0) return(matches)

  # Step 3: For multi-model questions: require 2+ models sharing ≥1 non-PK output
  if (question_row$require_multi_model[1]) {
    # Coarse gate: need ≥2 models per therapeutic area before checking overlap
    ta_counts     <- table(matches$therapeutic_area)
    viable_ta     <- names(ta_counts[ta_counts >= 2])
    if (length(viable_ta) == 0) return(matches[0, ])
    matches <- matches[matches$therapeutic_area %in% viable_ta, ]

    # Keep only models that share ≥1 PD output with at least one other model in the same TA
    keep <- logical(nrow(matches))
    for (i in seq_len(nrow(matches))) {
      pd_i <- get_pd_vars(matches$output[i])
      if (length(pd_i) == 0) next
      same_ta <- which(matches$therapeutic_area == matches$therapeutic_area[i])
      for (j in same_ta) {
        if (j == i) next
        pd_j <- get_pd_vars(matches$output[j])
        if (any(pd_i %in% pd_j)) { keep[i] <- TRUE; break }
      }
    }
    if (!any(keep)) return(matches[0, ])
    matches <- matches[keep, ]
  }

  # Step 4: Sort by validation status (Fully Validated first) then year descending
  if (nrow(matches) > 0) {
    matches$validation_order <- ifelse(
      grepl("Fully Validated", matches$validation_status), 1,
      ifelse(grepl("Validated", matches$validation_status), 2, 3)
    )
    matches <- matches[order(matches$validation_order, -matches$year, na.last = TRUE), ]
    matches$validation_order <- NULL
  }

  return(matches)
}

# ============================================================================
# EXPORT NAMESPACE
# ============================================================================

export_question_library <- function() {
  list(
    questions             = questions,
    phases                = phases,
    phase_approaches      = phase_approaches,
    phase_descriptions    = phase_descriptions,
    approach_descriptions = approach_descriptions,
    question_descriptions = question_descriptions,
    get_phases                 = get_phases,
    get_approaches_for_phase   = get_approaches_for_phase,
    get_questions_for_approach = get_questions_for_approach,
    filter_models_by_question  = filter_models_by_question,
    get_pd_vars                = get_pd_vars
  )
}


