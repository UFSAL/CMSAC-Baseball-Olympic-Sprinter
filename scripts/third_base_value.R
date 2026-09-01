# Run and win value of adding a steal of third base to the programme
# (report section 3.9, the Results paragraph, and the case study's WPA split).
#
# Inputs   output/pinch_runner/re24_2021_2025.csv
#          output/pinch_runner/marlins_2025_use_states.csv
#          output/reproducibility/selected_uses_2021_2025.csv
#          output/reproducibility/race_parameters.csv
#          output/reproducibility/third_base_parameters.csv
# Output   output/reproducibility/third_base_value.csv
suppressPackageStartupMessages({library(dplyr); library(readr)})

ROOT <- normalizePath(".", mustWork = TRUE)
IN   <- file.path(ROOT, "output", "pinch_runner")
OUT  <- file.path(ROOT, "output", "reproducibility")
need <- function(p) { if (!file.exists(p)) stop("Missing input: ", p); p }

P2 <- read_csv(need(file.path(OUT, "race_parameters.csv")), show_col_types = FALSE)$safe_probability[[1]]
P3 <- read_csv(need(file.path(OUT, "third_base_parameters.csv")), show_col_types = FALSE)$safe_probability[[1]]
rpw <- read_csv(need(file.path(ROOT, "output", "pinch_runner", "runs_per_win_2021_2025.csv")),
                show_col_types = FALSE)
runs_per_win <- weighted.mean(rpw$runs_per_win, rpw$games)

re <- read_csv(need(file.path(IN, "re24_2021_2025.csv")), show_col_types = FALSE)
state <- function(o, r1, r2, r3)
  re$exp_runs[re$outs == o & re$r1 == r1 & re$r2 == r2 & re$r3 == r3][[1]]

# One attempt on third, from second base, by out count.
third <- tibble(outs = 0:2) |>
  rowwise() |>
  mutate(gain = state(outs, 0, 0, 1) - state(outs, 0, 1, 0),
         loss = if (outs < 2) state(outs + 1, 0, 0, 0) - state(outs, 0, 1, 0)
                else -state(outs, 0, 1, 0),
         break_even  = -loss / (gain - loss),
         exp_runs    = P3 * gain + (1 - P3) * loss) |>
  ungroup()

selected <- read_csv(need(file.path(OUT, "selected_uses_2021_2025.csv")), show_col_types = FALSE)
n_seasons <- n_distinct(selected$season) * 30

league <- selected |>
  left_join(third |> select(outs, exp_runs), by = "outs") |>
  summarise(uses_per_team      = n() / n_seasons,
            reach_second       = n() * P2 / n_seasons,
            third_runs_per_team = sum(P2 * exp_runs) / n_seasons,
            third_wpa_per_team  = sum(P2 * exp_runs * state_win_value_per_run) / n_seasons,
            second_wpa_per_team = sum(net_wpa) / n_seasons,
            second_war_per_team = sum(net_wsb_war) / n_seasons) |>
  mutate(third_war_per_team = third_runs_per_team / runs_per_win,
         combined_war       = second_war_per_team + third_war_per_team,
         combined_wpa       = second_wpa_per_team + third_wpa_per_team)

conventional <- read_csv(need(file.path(OUT, "report_values.csv")), show_col_types = FALSE)
war_13 <- conventional$estimate[conventional$value == "conventional_war"]

mia <- read_csv(need(file.path(IN, "marlins_2025_use_states.csv")), show_col_types = FALSE) |>
  left_join(selected |> filter(season == 2025) |>
              select(game_pk, inning, half, outs, net_wpa, state_win_value_per_run),
            by = c("game_pk", "inning", "half", "outs")) |>
  left_join(third |> select(outs, exp_runs), by = "outs") |>
  summarise(uses = n(),
            wpa_second = sum(net_wpa),
            wpa_third  = sum(P2 * exp_runs * state_win_value_per_run)) |>
  mutate(wpa_total = wpa_second + wpa_third)

cat(sprintf("\nsafe rate stealing third      : %.2f%%\n", 100 * P3))
cat("\nrun value of one attempt on third, by out count:\n")
print(third |> mutate(across(gain:exp_runs, ~round(.x, 4))) |> as.data.frame(), row.names = FALSE)

cat(sprintf("\nLEAGUE, per team-season\n"))
cat(sprintf("  uses                        %.2f\n", league$uses_per_team))
cat(sprintf("  reaching second safely      %.1f\n", league$reach_second))
cat(sprintf("  runs added at third         %.2f\n", league$third_runs_per_team))
cat(sprintf("  WAR added at third          %.3f\n", league$third_war_per_team))
cat(sprintf("  second-base WAR             %.3f\n", league$second_war_per_team))
cat(sprintf("  combined WAR                %.3f\n", league$combined_war))
cat(sprintf("  edge over the 13th batter   %+.3f\n", league$combined_war - war_13))

cat(sprintf("\n2025 MARLINS, %d uses\n", mia$uses))
cat(sprintf("  WPA, steal of second        %.3f\n", mia$wpa_second))
cat(sprintf("  WPA, steal of third         %.3f\n", mia$wpa_third))
cat(sprintf("  WPA, total                  %.3f\n", mia$wpa_total))

write_csv(bind_cols(league, mia |> rename_with(~paste0("mia_", .x))) |>
            mutate(safe_probability_third = P3, war_13 = war_13,
                   edge_over_13 = combined_war - war_13),
          file.path(OUT, "third_base_value.csv"))
cat(sprintf("\nwritten to %s\n", file.path(OUT, "third_base_value.csv")))
