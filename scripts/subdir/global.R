model_filenames <- c("Olezarsen_PKPD_model.cpp", "Plozasiran_KPD_model.cpp")
trial_presets <- list(
  trial_design = "parallel",
  n_arms = 2,
  enable_switch = FALSE,
  suggested_weeks = 52,
  question_title = "How do two compounds compare head-to-head in efficacy?",
  auto_run = TRUE,
  cts_mode = "comparison"
)
