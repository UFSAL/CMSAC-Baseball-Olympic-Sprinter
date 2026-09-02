#!/usr/bin/env Rscript
# Rewrite the fixed-state value ceilings at the current derived safe rate.
#
# WHY THIS EXISTS
# ---------------
# output/pinch_runner/elite_1_85_catcher_exact_rules.csv was written by
# analyze_pinch_runner.py when the race model still gave 98.7% safe against a
# 1.85-second pop. The model now derives 93.1% against 1.86 s, but the file was
# never rewritten, so every consumer of its primary_* columns kept valuing a
# steal at the old rate. The WAR path was unaffected - it rebuilds gross value
# from safe_probability - but net_wpa read straight from this file.
#
# Re-running analyze_pinch_runner.py end to end is NOT the way to fix it: that
# script also rewrites roster_cost_2021_2025.csv from the *un-repaired* window
# parquet, which would regress the report's 13th-batter table and war_13 from
# -0.4984 back to -0.5250. This script touches only what the safe rate changes.
#
#   Rscript scripts/estimate_race_scale.R      # writes race_parameters.csv
#   Rscript scripts/regenerate_elite_rules.R
#
# WHAT CHANGES
# ------------
# Only the four columns that depend on the safe rate, and only on the 1B_to_2B
# rows: primary_success_probability, max_replacement_cost_wpa,
# max_replacement_cost_ubr_runs, max_replacement_cost_war (plus the
# worth_before_replacement_cost flag derived from them). Everything else - the
# win expectancies, both break-even rates, the 90% and adverse-battery
# sensitivity columns - is independent of the safe rate and is carried through
# untouched.
#
# analyze_pinch_runner.py filters this file to 1B_to_2B before writing it, so all
# 378 rows (9 innings x 2 halves x 7 score margins x 3 out counts) are rebuilt.
# The steal-of-third values the report uses are derived separately, in
# scripts/third_base_value.R, and do not come from here.
#
# The formulas are lifted from analyze_pinch_runner.py:
#   primary_we_gain      = p*success_we + (1-p)*caught_we - hold_we
#   primary_re24_ubr_runs= p*success_re + (1-p)*caught_re - hold_re
#   primary_baserunning_war = primary_re24_ubr_runs / RUNS_PER_WIN
# and this script verifies that reproducing the file at its *stored* rate
# returns the file, before rewriting it at the new one.
#
# NOTE ON THE FILE NAME: "1_85" is now historical. The catcher is Realmuto's
# 1.86 s. The name is kept because several scripts reference it by path.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

ROOT <- normalizePath(".", mustWork = TRUE)
need <- function(p) { if (!file.exists(p)) stop("Missing input: ", p); p }
IN <- file.path(ROOT, "output", "pinch_runner")
RULES <- file.path(IN, "elite_1_85_catcher_exact_rules.csv")
TOLERANCE <- 5e-6   # the file is stored at %.6f

safe_probability <- read_csv(
  need(file.path(ROOT, "output", "reproducibility", "race_parameters.csv")),
  show_col_types = FALSE
)$safe_probability[[1]]

rules <- read_csv(need(RULES), show_col_types = FALSE)
re24 <- read_csv(need(file.path(IN, "re24_2021_2025.csv")), show_col_types = FALSE)
rpw <- read_csv(need(file.path(IN, "runs_per_win_2021_2025.csv")), show_col_types = FALSE)
runs_per_win <- weighted.mean(rpw$runs_per_win, rpw$games)

run_expectancy <- function(outs, r1, r2, r3) {
  if (outs >= 3) return(0)
  value <- re24$exp_runs[re24$outs == outs & re24$r1 == r1 &
                           re24$r2 == r2 & re24$r3 == r3]
  if (length(value) != 1) stop("No RE24 entry for state ", outs, "-", r1, r2, r3)
  value
}

# The base-out states a steal of second moves between: hold on first, succeed to
# second, or be erased with one more out and the bases empty.
with_re <- rules |>
  rowwise() |>
  mutate(
    hold_re    = run_expectancy(outs, 1, 0, 0),
    success_re = run_expectancy(outs, 0, 1, 0),
    caught_re  = run_expectancy(outs + 1, 0, 0, 0)
  ) |>
  ungroup()

value_at <- function(d, p) {
  d |>
    mutate(
      wpa = p * success_we + (1 - p) * caught_we - hold_we,
      ubr = p * success_re + (1 - p) * caught_re - hold_re,
      war = ubr / runs_per_win
    )
}

## ---- prove the formulas against the file as it stands ----------------------
stored_rate <- unique(rules$primary_success_probability[rules$steal == "1B_to_2B"])
if (length(stored_rate) != 1) {
  stop("The 1B_to_2B rows do not share one stored safe rate.")
}
message("Stored safe rate in the file : ", sprintf("%.6f", stored_rate))
message("Derived safe rate now        : ", sprintf("%.6f", safe_probability))

check <- with_re |> filter(steal == "1B_to_2B") |> value_at(stored_rate)
drift <- c(
  wpa = max(abs(check$wpa - check$max_replacement_cost_wpa)),
  ubr = max(abs(check$ubr - check$max_replacement_cost_ubr_runs)),
  war = max(abs(check$war - check$max_replacement_cost_war))
)
message("\nReproducing the file at its own stored rate (must be within ",
        format(TOLERANCE), "):")
for (nm in names(drift)) message("  ", nm, ": ", sprintf("%.3g", drift[[nm]]))
if (any(drift > TOLERANCE)) {
  stop("The reconstruction does not reproduce the existing file; refusing to ",
       "rewrite it. The formulas here no longer match analyze_pinch_runner.py.")
}

if (abs(stored_rate - safe_probability) < 1e-12) {
  message("\nThe file already carries the derived safe rate. Nothing to do.")
  quit(save = "no", status = 0)
}

## ---- rewrite the rate-dependent columns ------------------------------------
updated <- with_re |>
  value_at(safe_probability) |>
  mutate(
    rebuild = steal == "1B_to_2B",
    primary_success_probability = if_else(
      rebuild, safe_probability, primary_success_probability),
    max_replacement_cost_wpa = if_else(
      rebuild, wpa, max_replacement_cost_wpa),
    max_replacement_cost_ubr_runs = if_else(
      rebuild, ubr, max_replacement_cost_ubr_runs),
    max_replacement_cost_war = if_else(
      rebuild, war, max_replacement_cost_war),
    worth_before_replacement_cost =
      max_replacement_cost_ubr_runs > 0 & max_replacement_cost_wpa > 0
  ) |>
  select(all_of(names(rules)))

if (!identical(dim(updated), dim(rules))) stop("The rewrite changed the shape.")

# analyze_pinch_runner.py wrote this with pandas: float_format="%.6f" applies to
# the float columns only, integers stay bare, and the flag prints True/False.
# Match all three so the file stays diffable against its own history.
INTEGER_COLUMNS <- c("inning", "batting_score_diff", "outs")
formatted <- updated |>
  mutate(
    across(all_of(INTEGER_COLUMNS), ~ formatC(.x, format = "d")),
    across(where(is.numeric), ~ sprintf("%.6f", .x)),
    across(where(is.logical), ~ if_else(.x, "True", "False"))
  )
write_csv(formatted, RULES)

changed <- updated |> filter(steal == "1B_to_2B")
message("\nRewritten at ", sprintf("%.4f", safe_probability), ": ",
        nrow(changed), " 1B_to_2B rows, ",
        nrow(updated) - nrow(changed), " 2B_to_3B rows left as they were.")
message("  mean gross RE24 gain  ", sprintf("%.5f", mean(check$max_replacement_cost_ubr_runs)),
        "  ->  ", sprintf("%.5f", mean(changed$max_replacement_cost_ubr_runs)))
message("  mean state WPA ceiling ", sprintf("%.5f", mean(check$max_replacement_cost_wpa)),
        "  ->  ", sprintf("%.5f", mean(changed$max_replacement_cost_wpa)))
message("\n", RULES)
message("Now re-run: Rscript scripts/final_report_calculations.R")
