#!/usr/bin/env Rscript

# Complete calculation workflow for the Olympic pinch-runner report.
#
# This file replaces the calculation logic that was split across Python, SQL,
# R, and the Quarto report. It starts from the project's prepared analytical
# data files. It does not download data. Run it from the project root:
#
#   Rscript scripts/final_report_calculations.R
#
# The script writes its checked results to output/reproducibility.

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("arrow", "dplyr", "readr", "sandwich")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install these R packages before you run the analysis: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(sandwich)
})

project_root <- normalizePath(".", mustWork = TRUE)
input_dir <- file.path(project_root, "output", "pinch_runner")
result_dir <- file.path(project_root, "output", "reproducibility")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

require_file <- function(path) {
  if (!file.exists(path)) stop("Required input file does not exist: ", path)
  path
}

weighted_mean_checked <- function(value, weight) {
  keep <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(keep)) stop("A weighted mean has no valid positive weights.")
  sum(value[keep] * weight[keep]) / sum(weight[keep])
}

message("1. Calculate the race distribution and stolen-base run value.")

# The race margin is runner arrival time minus tag arrival time.
# A negative value is safe. Zero or a positive value is an out.
#
# Both race parameters are DERIVED, not typed in. scripts/estimate_race_scale.R
# fits every MLB runner and Seville through one kinematic model, takes the timing
# edge from those fits, estimates the logistic scale from 8,772 tracked throws to
# second base, and calibrates the centre so a league runner in a contested race
# reproduces the observed safe rate. It writes race_parameters.csv, which is read
# below. Run it directly to see the full derivation.
race_parameter_file <- file.path(result_dir, "race_parameters.csv")
estimator <- file.path(project_root, "scripts", "estimate_race_scale.R")
if (!file.exists(estimator)) stop("Missing the race-scale estimator: ", estimator)
message("   deriving race parameters (scripts/estimate_race_scale.R)")
estimator_status <- system2("Rscript", estimator, stdout = FALSE, stderr = FALSE)
if (estimator_status != 0 || !file.exists(race_parameter_file)) {
  stop("The race-scale estimator failed; cannot continue without its parameters.")
}
race_parameters <- read_csv(race_parameter_file, show_col_types = FALSE)
race_center_seconds <- race_parameters$race_center_seconds[[1]]
race_spread_seconds <- race_parameters$race_spread_seconds[[1]]
if (!is.finite(race_center_seconds) || race_center_seconds >= 0) {
  stop("The derived race centre must be negative.")
}
safe_cutoff_seconds <- 0

if (race_spread_seconds <= 0) stop("The race spread must be positive.")

# R's plogis() is the logistic cumulative distribution function.
safe_probability <- plogis(
  safe_cutoff_seconds,
  location = race_center_seconds,
  scale = race_spread_seconds
)
out_probability <- 1 - safe_probability
expected_safe_per_100 <- 100 * safe_probability

league_constants <- read_csv(
  require_file(file.path(input_dir, "runs_per_win_2021_2025.csv")),
  show_col_types = FALSE
) |>
  filter(season >= 2021, season <= 2025) |>
  arrange(season)

if (nrow(league_constants) != 5L ||
    !all(as.integer(league_constants$season) == 2021:2025)) {
  stop("The league-constant input must contain one row for each season, 2021-2025.")
}

league_constants <- league_constants |>
  mutate(
    break_even_safe_rate = -run_cs / (run_sb - run_cs),
    gross_wsb_runs = safe_probability * run_sb + out_probability * run_cs,
    gross_wsb_war = gross_wsb_runs / runs_per_win
  )

runs_per_win_mean <- weighted_mean_checked(
  league_constants$runs_per_win,
  league_constants$games
)
gross_wsb_runs_mean <- weighted_mean_checked(
  league_constants$gross_wsb_runs,
  league_constants$games
)

message("2. Calculate the conventional 13th-player value.")

roster_windows <- read_parquet(
  require_file(file.path(
    project_root,
    "output",
    "thirteenth_man_6game_rolling_2021_2025_repaired.parquet"
  ))
) |>
  collect() |>
  filter(season >= 2021, season <= 2025)

if (nrow(roster_windows) != 23546) {
  stop("The six-game roster sample must contain 23,546 windows; found ",
       nrow(roster_windows), ".")
}

roster_by_season <- roster_windows |>
  group_by(season) |>
  summarise(
    windows = n(),
    fwar_6g = mean(fwar_window),
    fwar_per_game = mean(fwar_window) / 6,
    wpa_6g = mean(wpa_window),
    pa_6g = mean(pa_in_window),
    .groups = "drop"
  )

conventional_war <- weighted_mean_checked(
  roster_by_season$fwar_per_game,
  roster_by_season$windows
) * 162

message("3. Compare team batting performance with 12 and 13 position players.")

# The MLB Stats API active-roster endpoint applies retroactive transaction dates
# (an IL placement backdated up to 3 days removes a player from days he was still
# rostered). rosters_2021_2025_repaired.parquet undoes those premature drops and
# missed additions using the transaction log. distinct() is required: the raw
# snapshot contains duplicated player-dates that would otherwise inflate the
# position-player count by one.
rosters <- read_parquet(
  require_file(file.path(project_root, "data", "rosters_2021_2025_repaired.parquet"))
) |>
  collect() |>
  filter(season >= 2021, season <= 2025) |>
  distinct(season, team_id, roster_date, player_id, position_type) |>
  group_by(season, team_id, roster_date) |>
  summarise(
    position_players = sum(position_type != "Pitcher"),
    roster_total = n(),
    .groups = "drop"
  ) |>
  # Keep only snapshots sitting exactly at the MLB roster limit. A snapshot at the
  # limit cannot be missing a player, so the position-player count is trustworthy.
  mutate(roster_limit = ifelse(as.integer(format(roster_date, "%m")) >= 9L, 28L, 26L)) |>
  filter(roster_total == roster_limit) |>
  select(season, team_id, roster_date, position_players)

team_offense <- read_parquet(
  require_file(file.path(project_root, "data", "engine", "appearance_panel.parquet"))
) |>
  collect() |>
  filter(season >= 2021, season <= 2025) |>
  group_by(season, team_id, game_pk, d) |>
  summarise(
    pa = sum(pa, na.rm = TRUE),
    batting_runs = sum(bat_runs, na.rm = TRUE),
    .groups = "drop"
  )

rest_sample <- team_offense |>
  inner_join(
    rosters,
    by = c("season", "team_id", "d" = "roster_date")
  ) |>
  filter(pa > 0, position_players %in% c(12L, 13L)) |>
  mutate(
    runs_per_pa = batting_runs / pa,
    has_13 = as.integer(position_players == 13L),
    team_factor = factor(team_id),
    season_factor = factor(season)
  )

rest_counts <- rest_sample |>
  count(position_players, name = "team_games")

rest_raw <- rest_sample |>
  group_by(position_players) |>
  summarise(
    team_games = n(),
    pa = sum(pa),
    batting_runs = sum(batting_runs),
    runs_per_pa = batting_runs / pa,
    .groups = "drop"
  )

# Plate appearances are the regression weights. Team and season indicators
# account for stable team differences and league-wide season differences.
rest_model <- lm(
  runs_per_pa ~ has_13 + team_factor + season_factor,
  data = rest_sample,
  weights = pa
)

# Team-clustered standard errors allow results from the same club to be
# correlated across games. The coefficient itself is the weighted least-
# squares estimate above; clustering changes its uncertainty, not its value.
rest_vcov <- sandwich::vcovCL(
  rest_model,
  cluster = rest_sample$team_id,
  type = "HC1"
)
rest_effect <- unname(coef(rest_model)["has_13"])
rest_standard_error <- sqrt(unname(rest_vcov["has_13", "has_13"]))
rest_t <- rest_effect / rest_standard_error
rest_cluster_df <- length(unique(rest_sample$team_id)) - 1
rest_p_value <- 2 * pt(-abs(rest_t), df = rest_cluster_df)
rest_critical_value <- qt(0.975, df = rest_cluster_df)
rest_ci_low <- rest_effect - rest_critical_value * rest_standard_error
rest_ci_high <- rest_effect + rest_critical_value * rest_standard_error

# The null result is the rule used in the roster comparison.
rest_war <- 0

message("4. Solve the game-state model in R.")

transition_data <- read_csv(
  require_file(file.path(input_dir, "base_out_transitions_2021_2025.csv")),
  show_col_types = FALSE
)

# Rebuild the candidate-level substitution costs in R. These calculations were
# previously performed in analyze_pinch_runner.py.
all_candidates <- read_csv(
  require_file(file.path(input_dir, "all_candidate_states_2021_2025.csv")),
  show_col_types = FALSE
)
player_rates <- read_csv(
  require_file(file.path(input_dir, "player_value_rates_2021_2025.csv")),
  show_col_types = FALSE
)
replacement_rates <- read_csv(
  require_file(file.path(input_dir, "forced_replacement_rates_2021_2025.csv")),
  show_col_types = FALSE
)
sprint_speeds <- read_csv(
  require_file(file.path(input_dir, "sprint_speed_2021_2025.csv")),
  show_col_types = FALSE
)
state_value_limits <- read_csv(
  require_file(file.path(input_dir, "elite_1_85_catcher_exact_rules.csv")),
  show_col_types = FALSE
) |>
  filter(steal == "1B_to_2B") |>
  select(
    inning, half, batting_score_diff, outs,
    max_replacement_cost_ubr_runs,
    max_replacement_cost_war,
    max_replacement_cost_wpa
  )

league_offense_rate <- sum(player_rates$offense_runs) / sum(player_rates$pa)
league_defense_rate <-
  sum(player_rates$defense_runs) / sum(player_rates$total_innings)
league_field_share <-
  sum(player_rates$field_innings) / sum(player_rates$total_innings)

player_rates <- player_rates |>
  mutate(
    original_off_rate =
      (offense_runs + 100 * league_offense_rate) / (pa + 100),
    original_def_rate =
      (defense_runs + 100 * league_defense_rate) / (total_innings + 100),
    field_share = if_else(
      is.finite(field_innings / total_innings),
      field_innings / total_innings,
      league_field_share
    )
  ) |>
  select(season, player_id, original_off_rate, original_def_rate, field_share)

candidate_data <- all_candidates |>
  left_join(
    state_value_limits,
    by = c("inning", "half", "batting_score_diff", "outs")
  ) |>
  left_join(
    player_rates,
    by = c("season", "original_runner_id" = "player_id")
  ) |>
  left_join(
    sprint_speeds |>
      rename(original_runner_id = player_id),
    by = c("season", "original_runner_id")
  ) |>
  mutate(
    original_off_rate = coalesce(original_off_rate, league_offense_rate),
    original_def_rate = coalesce(original_def_rate, league_defense_rate),
    field_share = coalesce(field_share, league_field_share),
    expected_future_field_innings =
      future_defensive_half_innings * field_share,
    forced_substitution_cost_runs =
      expected_future_pa *
        (original_off_rate - replacement_rates$offense_runs_per_pa[[1]]) +
      expected_future_field_innings *
        (original_def_rate - replacement_rates$defense_runs_per_inning[[1]]),
    net_ubr_runs =
      max_replacement_cost_ubr_runs - forced_substitution_cost_runs,
    net_war = net_ubr_runs / runs_per_win_mean,
    state_win_value_per_run =
      max_replacement_cost_wpa / max_replacement_cost_ubr_runs,
    net_wpa = net_ubr_runs * state_win_value_per_run,
    plus_ev = net_ubr_runs > 0
  )

reference_candidates <- read_csv(
  require_file(file.path(input_dir, "net_substitution_candidate_states_2021_2025.csv")),
  show_col_types = FALSE
)
if (nrow(candidate_data) != nrow(reference_candidates)) {
  stop("The R candidate reconstruction has a different row count.")
}
candidate_check <- candidate_data |>
  select(season, game_pk, at_bat_index, half,
         forced_substitution_cost_runs) |>
  inner_join(
    reference_candidates |>
      select(season, game_pk, at_bat_index, half,
             reference_cost = forced_substitution_cost_runs),
    by = c("season", "game_pk", "at_bat_index", "half")
  )
if (nrow(candidate_check) != nrow(candidate_data) ||
    max(abs(candidate_check$forced_substitution_cost_runs -
            candidate_check$reference_cost), na.rm = TRUE) > 2e-6) {
  stop("The R candidate substitution-cost reconstruction failed validation.")
}

max_inning <- 12L
max_score_difference <- 12L
score_differences <- -max_score_difference:max_score_difference

# Preserve the exact state order from the original Python implementation:
# outs, runner on first, runner on second, runner on third.
base_states <- do.call(
  rbind,
  lapply(0:2, function(outs) {
    do.call(rbind, lapply(0:1, function(r1) {
      do.call(rbind, lapply(0:1, function(r2) {
        cbind(outs, r1, r2, r3 = 0:1)
      }))
    }))
  })
)
colnames(base_states) <- c("outs", "r1", "r2", "r3")
state_keys <- apply(base_states, 1, paste, collapse = "-")
state_index <- setNames(seq_len(nrow(base_states)), state_keys)

state_id <- function(outs, r1, r2, r3) {
  key <- paste(outs, r1, r2, r3, sep = "-")
  answer <- unname(state_index[key])
  if (is.na(answer)) stop("Invalid base-out state: ", key)
  answer
}

empty_state <- state_id(0, 0, 0, 0)
ghost_runner_state <- state_id(0, 0, 1, 0)

score_index <- function(score_difference) {
  bounded <- min(max(score_difference, -max_score_difference),
                 max_score_difference)
  bounded + max_score_difference + 1L
}

transition_kernel <- vector("list", nrow(base_states))
for (state_row in seq_len(nrow(base_states))) {
  s <- base_states[state_row, ]
  rows <- transition_data |>
    filter(
      outs == s["outs"], r1 == s["r1"],
      r2 == s["r2"], r3 == s["r3"]
    )
  if (nrow(rows) == 0) stop("A base-out state has no observed transitions.")
  rows$probability <- rows$n / sum(rows$n)
  rows$to_state <- NA_integer_
  live_rows <- rows$to_outs < 3
  rows$to_state[live_rows] <- mapply(
    state_id,
    rows$to_outs[live_rows],
    rows$to_r1[live_rows],
    rows$to_r2[live_rows],
    rows$to_r3[live_rows]
  )
  transition_kernel[[state_row]] <- rows |>
    select(probability, to_state, runs)
}

# The report uses an average MLB runner as the replaced runner. Therefore,
# every observed candidate is eligible when the future-use probability and
# substitution-cost schedules are estimated.
average_candidates <- candidate_data |>
  filter(season >= 2021, season <= 2025)
overall_substitution_cost <- mean(
  average_candidates$forced_substitution_cost_runs,
  na.rm = TRUE
)

cost_table <- average_candidates |>
  group_by(inning, half, outs) |>
  summarise(
    n = n(),
    cost_sum = sum(forced_substitution_cost_runs, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    smoothed_cost = (cost_sum + 30 * overall_substitution_cost) / (n + 30)
  )

substitution_cost <- function(inning, half_index, outs) {
  half_label <- if (half_index == 0L) "top" else "bottom"
  row <- cost_table |>
    filter(
      .data$inning == min(as.integer(.env$inning), 9L),
      .data$half == .env$half_label,
      .data$outs == as.integer(.env$outs)
    )
  if (nrow(row) == 0) return(overall_substitution_cost)
  row$smoothed_cost[[1]]
}

initial_values <- function() {
  values <- array(
    0,
    dim = c(max_inning, 2L, length(score_differences), nrow(base_states))
  )
  for (inning in seq_len(max_inning)) {
    for (half_index in 0:1) {
      for (score_difference in score_differences) {
        p <- plogis(0.7 * score_difference) + 0.025
        p <- min(max(p, 0.001), 0.999)
        values[inning, half_index + 1L, score_index(score_difference), ] <- p
      }
    }
  }
  values
}

transition_value <- function(values, inning, half_index, home_difference,
                             to_state, runs) {
  new_difference <- if (half_index == 0L) {
    home_difference - runs
  } else {
    home_difference + runs
  }

  if (new_difference > max_score_difference) return(1)
  if (new_difference < -max_score_difference) return(0)
  if (half_index == 1L && inning >= 9L && new_difference > 0) return(1)

  if (!is.na(to_state)) {
    return(values[
      inning, half_index + 1L, score_index(new_difference), to_state
    ])
  }

  if (half_index == 0L) {
    if (inning >= 9L && new_difference > 0) return(1)
    start_state <- if (inning >= 10L) ghost_runner_state else empty_state
    return(values[inning, 2L, score_index(new_difference), start_state])
  }

  if (inning >= 9L) {
    if (new_difference > 0) return(1)
    if (new_difference < 0) return(0)
    if (inning >= max_inning) return(0.52)
    return(values[
      inning + 1L, 1L, score_index(new_difference), ghost_runner_state
    ])
  }

  values[inning + 1L, 1L, score_index(new_difference), empty_state]
}

expected_pa_value <- function(values, inning, half_index,
                              home_difference, state_number) {
  transitions <- transition_kernel[[state_number]]
  sum(vapply(seq_len(nrow(transitions)), function(i) {
    transitions$probability[i] * transition_value(
      values,
      inning,
      half_index,
      home_difference,
      transitions$to_state[i],
      transitions$runs[i]
    )
  }, numeric(1)))
}

solve_baseline <- function(tolerance = 1e-10, max_iterations = 1000L) {
  values <- initial_values()
  final_change <- Inf

  for (iteration in seq_len(max_iterations)) {
    final_change <- 0
    for (inning in max_inning:1L) {
      for (half_index in c(1L, 0L)) {
        difference_order <- if (half_index == 1L) {
          rev(score_differences)
        } else {
          score_differences
        }
        for (home_difference in difference_order) {
          if (half_index == 1L && inning >= 9L && home_difference > 0) {
            values[
              inning, half_index + 1L, score_index(home_difference),
            ] <- 1
            next
          }
          for (state_number in seq_len(nrow(base_states))) {
            value <- expected_pa_value(
              values, inning, half_index, home_difference, state_number
            )
            old_value <- values[
              inning, half_index + 1L, score_index(home_difference), state_number
            ]
            values[
              inning, half_index + 1L, score_index(home_difference), state_number
            ] <- value
            final_change <- max(final_change, abs(value - old_value))
          }
        }
      }
    }
    if (final_change < tolerance) {
      return(list(values = values, iterations = iteration, change = final_change))
    }
  }
  stop("The baseline win-probability calculation did not converge.")
}

use_cache <- identical(Sys.getenv("USE_CALCULATION_CACHE"), "1")
baseline_cache <- file.path(result_dir, "baseline_solution.rds")
if (use_cache && file.exists(baseline_cache)) {
  baseline_solution <- readRDS(baseline_cache)
} else {
  baseline_solution <- solve_baseline()
  saveRDS(baseline_solution, baseline_cache)
}
baseline_values <- baseline_solution$values

team_we_from_home <- function(home_we, side) {
  if (side == "home") home_we else 1 - home_we
}

home_we_from_team <- function(team_we, side) {
  if (side == "home") team_we else 1 - team_we
}

use_now_home_value <- function(inning, half_index, home_difference,
                               outs, side, runner_on_third = 0L) {
  success_state <- state_id(outs, 0, 1, runner_on_third)
  success_home <- baseline_values[
    inning, half_index + 1L, score_index(home_difference), success_state
  ]

  if (outs < 2L) {
    caught_state <- state_id(outs + 1L, 0, 0, runner_on_third)
    caught_home <- baseline_values[
      inning, half_index + 1L, score_index(home_difference), caught_state
    ]
  } else {
    caught_home <- transition_value(
      baseline_values, inning, half_index, home_difference, NA_integer_, 0
    )
  }

  success_team <- team_we_from_home(success_home, side)
  caught_team <- team_we_from_home(caught_home, side)
  gross_team <- safe_probability * success_team + out_probability * caught_team

  current_state <- state_id(outs, 1, 0, runner_on_third)
  current_team <- team_we_from_home(
    baseline_values[
      inning, half_index + 1L, score_index(home_difference), current_state
    ],
    side
  )
  sign <- if (side == "home") 1L else -1L
  one_run_difference <- min(max(home_difference + sign,
                                -max_score_difference), max_score_difference)
  one_run_team <- team_we_from_home(
    baseline_values[
      inning, half_index + 1L, score_index(one_run_difference), current_state
    ],
    side
  )
  marginal_win_probability_per_run <- max(one_run_team - current_team, 0)
  net_team <- gross_team -
    substitution_cost(inning, half_index, outs) * marginal_win_probability_per_run
  net_team <- min(max(net_team, 0), 1)

  list(
    home_we = home_we_from_team(net_team, side),
    gross_team_we = gross_team,
    marginal_wp_per_run = marginal_win_probability_per_run
  )
}

solve_with_option <- function(side, tolerance = 1e-10,
                              max_iterations = 1000L) {
  values <- baseline_values
  batting_half <- if (side == "home") 1L else 0L
  final_change <- Inf

  for (iteration in seq_len(max_iterations)) {
    final_change <- 0
    for (inning in max_inning:1L) {
      for (half_index in c(1L, 0L)) {
        difference_order <- if (half_index == 1L) {
          rev(score_differences)
        } else {
          score_differences
        }
        for (home_difference in difference_order) {
          if (half_index == 1L && inning >= 9L && home_difference > 0) {
            values[
              inning, half_index + 1L, score_index(home_difference),
            ] <- 1
            next
          }
          for (state_number in seq_len(nrow(base_states))) {
            state <- base_states[state_number, ]
            wait_home <- expected_pa_value(
              values, inning, half_index, home_difference, state_number
            )
            value <- wait_home
            if (half_index == batting_half &&
                state["r1"] == 1L && state["r2"] == 0L) {
              use_home <- use_now_home_value(
                inning,
                half_index,
                home_difference,
                state["outs"],
                side,
                state["r3"]
              )$home_we
              if (side == "home") {
                value <- wait_home + max(use_home - wait_home, 0)
              } else {
                value <- wait_home - max(wait_home - use_home, 0)
              }
            }
            old_value <- values[
              inning, half_index + 1L, score_index(home_difference), state_number
            ]
            values[
              inning, half_index + 1L, score_index(home_difference), state_number
            ] <- value
            final_change <- max(final_change, abs(value - old_value))
          }
        }
      }
    }
    if (final_change < tolerance) break
  }

  if (final_change >= tolerance) {
    stop("The specialist-option calculation did not converge for the ", side,
         " team.")
  }

  decision_rows <- vector("list", 9L * 21L * 3L)
  row_number <- 0L
  for (inning in 1:9) {
    for (batting_difference in -10:10) {
      home_difference <- if (side == "home") {
        batting_difference
      } else {
        -batting_difference
      }
      for (outs in 0:2) {
        state_number <- state_id(outs, 1, 0, 0)
        wait_home <- expected_pa_value(
          values, inning, batting_half, home_difference, state_number
        )
        use_result <- use_now_home_value(
          inning, batting_half, home_difference, outs, side, 0
        )
        wait_team <- team_we_from_home(wait_home, side)
        use_team <- team_we_from_home(use_result$home_we, side)
        row_number <- row_number + 1L
        decision_rows[[row_number]] <- data.frame(
          team_side = side,
          inning = inning,
          half = if (batting_half == 0L) "top" else "bottom",
          batting_score_diff = batting_difference,
          outs = outs,
          use_now_team_we = use_team,
          wait_team_we = wait_team,
          use_minus_wait = use_team - wait_team,
          decision = if (use_team > wait_team + 1e-10) "USE" else "KEEP",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  list(
    values = values,
    decisions = bind_rows(decision_rows),
    iterations = iteration,
    change = final_change
  )
}

home_cache <- file.path(result_dir, "home_option_solution_v2.rds")
away_cache <- file.path(result_dir, "away_option_solution_v2.rds")
if (use_cache && file.exists(home_cache) && file.exists(away_cache)) {
  home_solution <- readRDS(home_cache)
  away_solution <- readRDS(away_cache)
} else {
  home_solution <- solve_with_option("home")
  away_solution <- solve_with_option("away")
  saveRDS(home_solution, home_cache)
  saveRDS(away_solution, away_cache)
}
decision_table <- bind_rows(away_solution$decisions, home_solution$decisions)

message("5. Replay the R policy over the 2021-2025 candidate states.")

average_cost_lookup <- function(inning, half, outs) {
  substitution_cost(inning, if (half == "top") 0L else 1L, outs)
}

replay <- average_candidates |>
  left_join(
    cost_table |>
      select(inning, half, outs,
             average_substitution_cost_runs = smoothed_cost),
    by = c("inning", "half", "outs")
  ) |>
  mutate(
    net_ubr_runs =
      max_replacement_cost_ubr_runs - average_substitution_cost_runs,
    net_wpa = net_ubr_runs * state_win_value_per_run,
    team_side = if_else(half == "top", "away", "home")
  ) |>
  inner_join(
    decision_table,
    by = c(
      "team_side", "inning", "half", "batting_score_diff", "outs"
    )
  ) |>
  filter(decision == "USE") |>
  arrange(season, game_pk, half, at_bat_index) |>
  group_by(season, game_pk, half) |>
  slice_head(n = 1) |>
  ungroup()

replay <- replay |>
  left_join(
    league_constants |>
      select(season, run_sb, run_cs, runs_per_win),
    by = "season"
  ) |>
  mutate(
    gross_wsb_runs = safe_probability * run_sb + out_probability * run_cs,
    net_wsb_runs = gross_wsb_runs - average_substitution_cost_runs,
    net_wsb_war = net_wsb_runs / runs_per_win
  )

policy_by_season <- replay |>
  group_by(season) |>
  summarise(
    uses = n(),
    net_wsb_runs = sum(net_wsb_runs),
    net_wsb_war = sum(net_wsb_war),
    net_wpa = sum(net_wpa),
    use_over_keep_wpa = sum(use_minus_wait),
    .groups = "drop"
  ) |>
  mutate(
    uses_per_team = uses / 30,
    net_wsb_runs_per_team = net_wsb_runs / 30,
    net_wsb_war_per_team = net_wsb_war / 30,
    net_wpa_per_team = net_wpa / 30,
    use_over_keep_wpa_per_team = use_over_keep_wpa / 30
  )

olympian_uses <- mean(policy_by_season$uses_per_team)
olympian_war <- mean(policy_by_season$net_wsb_war_per_team)
olympian_wpa <- mean(policy_by_season$net_wpa_per_team)
war_edge <- olympian_war - (conventional_war + rest_war)

if (nrow(policy_by_season) != 5L ||
    !all(as.integer(policy_by_season$season) == 2021:2025)) {
  stop("The calculated policy must contain one result for each season, 2021-2025.")
}

reported_results <- c(
  safe_probability,
  conventional_war,
  rest_effect,
  rest_p_value,
  olympian_uses,
  olympian_war,
  olympian_wpa,
  war_edge
)
if (!all(is.finite(reported_results))) {
  stop("At least one calculated report result is not finite.")
}

message("6. Write calculated outputs.")

report_values <- tibble(
  value = c(
    "race_center_seconds",
    "race_spread_seconds",
    "safe_probability",
    "expected_safe_per_100",
    "weighted_runs_per_win",
    "gross_wsb_runs_per_attempt",
    "six_game_windows",
    "conventional_war",
    "rest_team_games_12",
    "rest_team_games_13",
    "rest_raw_rpa_12",
    "rest_raw_rpa_13",
    "rest_effect_rpa",
    "rest_standard_error",
    "rest_ci_low",
    "rest_ci_high",
    "rest_p_value",
    "rest_war_used",
    "average_substitution_cost_runs",
    "olympian_uses_per_team",
    "olympian_net_war",
    "olympian_net_wpa",
    "war_edge"
  ),
  estimate = c(
    race_center_seconds,
    race_spread_seconds,
    safe_probability,
    expected_safe_per_100,
    runs_per_win_mean,
    gross_wsb_runs_mean,
    nrow(roster_windows),
    conventional_war,
    rest_counts$team_games[rest_counts$position_players == 12],
    rest_counts$team_games[rest_counts$position_players == 13],
    rest_raw$runs_per_pa[rest_raw$position_players == 12],
    rest_raw$runs_per_pa[rest_raw$position_players == 13],
    rest_effect,
    rest_standard_error,
    rest_ci_low,
    rest_ci_high,
    rest_p_value,
    rest_war,
    overall_substitution_cost,
    olympian_uses,
    olympian_war,
    olympian_wpa,
    war_edge
  )
)

write_csv(report_values, file.path(result_dir, "report_values.csv"))
write_csv(roster_by_season, file.path(result_dir, "roster_value_by_season.csv"))
write_csv(rest_raw, file.path(result_dir, "rest_raw_comparison.csv"))
write_csv(league_constants, file.path(result_dir, "stolen_base_value_by_season.csv"))
write_csv(decision_table, file.path(result_dir, "game_state_decisions.csv"))
write_csv(replay, file.path(result_dir, "selected_uses_2021_2025.csv"))
write_csv(policy_by_season, file.path(result_dir, "policy_value_by_season.csv"))

validation <- tibble(
  check = c(
    "Race-distribution scale is positive",
    "League constants cover 2021-2025",
    "Roster-window sample is not empty",
    "Calculated policy covers 2021-2025",
    "All reported results are finite"
  ),
  passed = c(
    race_spread_seconds > 0,
    nrow(league_constants) == 5L,
    nrow(roster_windows) > 0L,
    nrow(policy_by_season) == 5L,
    all(is.finite(reported_results))
  )
)
write_csv(validation, file.path(result_dir, "validation_checks.csv"))

message("")
message("All calculation checks passed.")
message("Safe probability: ", sprintf("%.6f", safe_probability))
message("Conventional-player WAR: ", sprintf("%.6f", conventional_war))
message("Rest coefficient: ", sprintf("%+.9f", rest_effect),
        "; p = ", sprintf("%.6f", rest_p_value))
message("Olympian uses per team-season: ", sprintf("%.3f", olympian_uses))
message("Olympian net WAR: ", sprintf("%.6f", olympian_war))
message("WAR edge: ", sprintf("%+.6f", war_edge))
message("Outputs: ", result_dir)
