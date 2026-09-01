# Derive the three inputs the Marlins case study needs from the Statcast
# pitch-level data. Run this once; the case study itself reads only the CSVs,
# so the reproducibility bundle does not have to carry the pitch data.
#
# Writes into output/pinch_runner/:
#   base_out_run_distribution_2021_2025.csv  runs scored in the rest of a half
#                                            inning, by base-out state
#   marlins_2025_line_scores.csv             runs in every half inning of the season
#   marlins_2025_use_states.csv              the states the decision rule selects
suppressPackageStartupMessages({library(arrow); library(dplyr); library(readr); library(tidyr)})

ROOT   <- normalizePath(".", mustWork = TRUE)
OUT    <- file.path(ROOT, "output", "pinch_runner")
TEAM   <- "MIA"
SEASON <- 2025
MAXRUN <- 15

cols <- c("game_pk", "game_year", "inning", "inning_topbot", "at_bat_number",
          "pitch_number", "game_type", "outs_when_up", "on_1b", "on_2b", "on_3b",
          "bat_score", "post_bat_score", "home_team", "away_team")

message("reading Statcast 2021-2025 ...")
pitches <- open_dataset(file.path(ROOT, "data", "statcast_2021_2025"),
                        partitioning = "game_year") |>
  select(all_of(cols)) |>
  filter(game_type == "R") |>
  collect() |>
  arrange(game_pk, inning, inning_topbot, at_bat_number, pitch_number)
message("  ", format(nrow(pitches), big.mark = ","), " pitches")

half_end <- pitches |>
  group_by(game_pk, inning, inning_topbot) |>
  summarise(half_start = min(bat_score), half_end = max(post_bat_score), .groups = "drop")

## ---- 1. runs in the rest of the half inning, by base-out state --------------
pa <- pitches |>
  group_by(game_pk, inning, inning_topbot, at_bat_number) |>
  slice_head(n = 1) |>
  ungroup() |>
  left_join(half_end, by = c("game_pk", "inning", "inning_topbot")) |>
  mutate(runs_rest = half_end - bat_score) |>
  filter(runs_rest >= 0, runs_rest <= MAXRUN, outs_when_up < 3) |>
  mutate(state = paste0(outs_when_up, "_",
                        as.integer(!is.na(on_1b)),
                        as.integer(!is.na(on_2b)),
                        as.integer(!is.na(on_3b))))

run_dist <- pa |>
  count(state, runs_rest) |>
  group_by(state) |>
  mutate(p = n / sum(n)) |>
  ungroup() |>
  select(state, runs_rest, p) |>
  complete(state, runs_rest = 0:MAXRUN, fill = list(p = 0)) |>
  pivot_wider(names_from = runs_rest, values_from = p, names_prefix = "p") |>
  arrange(state)
write_csv(run_dist, file.path(OUT, "base_out_run_distribution_2021_2025.csv"))
message("  ", nrow(run_dist), " base-out states")

## ---- 2. the team's line scores ---------------------------------------------
team_games <- pitches |>
  filter(game_year == SEASON, home_team == TEAM | away_team == TEAM)
played <- team_games |>
  group_by(game_pk, inning, inning_topbot) |>
  summarise(runs = max(post_bat_score) - min(bat_score), .groups = "drop") |>
  mutate(half = if_else(inning_topbot == "Top", "top", "bottom")) |>
  select(game_pk, inning, half, runs)
meta <- team_games |>
  group_by(game_pk) |>
  summarise(home = first(home_team), maxi = max(inning), .groups = "drop")
# Every half inning up to the last one played. A half inning that was never
# played (the home team leading after the top of the ninth) keeps runs = NA,
# which the case study reads as "not played".
lines <- meta |>
  rowwise() |>
  reframe(game_pk = game_pk, home = home, maxi = maxi,
          inning = rep(seq_len(maxi), each = 2),
          half = rep(c("top", "bottom"), times = maxi)) |>
  left_join(played, by = c("game_pk", "inning", "half")) |>
  arrange(game_pk, inning, desc(half))
write_csv(lines, file.path(OUT, "marlins_2025_line_scores.csv"))
message("  ", n_distinct(lines$game_pk), " games")

## ---- 3. the states the rule selects ----------------------------------------
selected <- read_csv(file.path(ROOT, "output", "reproducibility",
                               "selected_uses_2021_2025.csv"),
                     show_col_types = FALSE) |>
  filter(season == SEASON)

pa_scores <- pa |>
  left_join(half_end, by = c("game_pk", "inning", "inning_topbot")) |>
  transmute(game_pk, at_bat_number,
            pre = bat_score - half_start.x,
            actual_rest = half_end.x - bat_score)

uses <- selected |>
  inner_join(lines |> distinct(game_pk, home), by = "game_pk") |>
  filter((half == "bottom" & home == TEAM) | (half == "top" & home != TEAM)) |>
  mutate(at_bat_number = at_bat_index + 1L) |>
  left_join(pa_scores, by = c("game_pk", "at_bat_number")) |>
  transmute(game_pk, inning, half, outs, score_diff = batting_score_diff,
            pre, actual_rest) |>
  arrange(game_pk)
stopifnot(!any(is.na(uses$pre)), !any(is.na(uses$actual_rest)))
write_csv(uses, file.path(OUT, "marlins_2025_use_states.csv"))
message("  ", nrow(uses), " selected uses")
message("written to ", OUT)
