#!/usr/bin/env Rscript
# Reproduce every number in the paper from the inputs committed to this repo.
#
#   Rscript pipeline.R
#
# Run from the repository root. Each stage is a standalone script; they are run
# in dependency order and the pipeline stops at the first failure.
#
# SCOPE. This reproduces the analysis from the prepared inputs in data/ and
# output/pinch_runner/. It does not rebuild those inputs from raw Statcast data:
# that stage lives in the full project (SQL engine + scrapers) and its outputs
# ship here as committed files. See README.md for the exact boundary.

options(warn = 1)
root <- normalizePath(".", mustWork = TRUE)
if (!dir.exists(file.path(root, "scripts"))) {
  stop("Run this from the repository root (no scripts/ directory here).")
}

stages <- c(
  # race geometry -> output/reproducibility/race_parameters.csv
  "scripts/estimate_race_scale.R",
  # third-base race geometry -> output/reproducibility/third_base_parameters.csv
  "scripts/estimate_third_base_scale.R",
  # rewrites output/pinch_runner/elite_1_85_catcher_exact_rules.csv in place
  "scripts/regenerate_elite_rules.R",
  # the main model: rest effect, game-state solve, replay, report_values.csv
  "scripts/final_report_calculations.R",
  # steal-of-third value -> output/reproducibility/third_base_value.csv
  "scripts/third_base_value.R",
  # Monte Carlo -> output/reproducibility/marlins_case_study_2025.csv
  "scripts/marlins_case_study.R",
  # window-length sensitivity -> output/sensitivity/window_length/*.csv
  "scripts/window_length_sensitivity.R"
)

started <- Sys.time()
for (i in seq_along(stages)) {
  stage <- stages[[i]]
  if (!file.exists(stage)) stop("Missing pipeline stage: ", stage)
  message(sprintf("\n[%d/%d] %s", i, length(stages), stage))
  status <- system2("Rscript", stage)
  if (!identical(status, 0L)) {
    stop("Stage failed (exit ", status, "): ", stage)
  }
}

message(sprintf("\nAll %d stages completed in %.1f min.",
                length(stages),
                as.numeric(difftime(Sys.time(), started, units = "mins"))))
message("Render the paper with: quarto render olympian_vs_13th_man.qmd --to pdf")
