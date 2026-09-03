#!/usr/bin/env Rscript
# Summarise the window-length sweep built by scripts/window_length_sensitivity.sh.
#
# The aggregation mirrors scripts/final_report_calculations.R step 2 exactly -
# per-season mean window fWAR, divided by the window length, then a
# window-count-weighted mean across seasons, times 162 - so the L=6 row is
# directly comparable to the published conventional_war.

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(readr)
  library(tidyr)
})

lengths <- 3:9
indir <- file.path("output", "sensitivity", "window_length")
outdir <- indir

# The sprinter's net WAR is independent of the 13th-man definition, so the
# sweep moves only the baseline it is compared against.
olympian_net_war <- read_csv(
  file.path("output", "reproducibility", "report_values.csv"),
  show_col_types = FALSE
) |>
  filter(value == "olympian_net_war") |>
  pull(estimate)

read_L <- function(L) {
  read_parquet(file.path(indir, sprintf("thirteenth_man_%dgame_rolling.parquet", L))) |>
    collect() |>
    filter(season >= 2021, season <= 2025) |>
    mutate(window_len = L)
}
all_w <- bind_rows(lapply(lengths, read_L))

# Self-check: L=6 must reproduce the committed repaired parquet.
committed <- read_parquet(
  "output/thirteenth_man_6game_rolling_2021_2025_repaired.parquet"
) |> collect()
six <- filter(all_w, window_len == 6)
stopifnot(nrow(six) == nrow(committed))
chk <- inner_join(
  select(six, season, team_id, window_idx, player_id, fwar_window),
  select(committed, season, team_id, window_idx,
         player_id_c = player_id, fwar_c = fwar_window),
  by = c("season", "team_id", "window_idx")
)
cat(sprintf("L=6 self-check: %d/%d windows matched, same player in %.4f%%, max |dWAR| = %.2e\n",
            nrow(chk), nrow(committed),
            100 * mean(chk$player_id == chk$player_id_c),
            max(abs(chk$fwar_window - chk$fwar_c))))

# Per-season, then window-weighted across seasons - the published recipe.
by_season <- all_w |>
  group_by(window_len, season) |>
  summarise(
    windows = n(),
    fwar_window_mean = mean(fwar_window),
    fwar_per_game = mean(fwar_window) / first(window_len),
    pa_mean = mean(pa_in_window),
    .groups = "drop"
  )

summary_tbl <- by_season |>
  group_by(window_len) |>
  summarise(
    war_per_162 = weighted.mean(fwar_per_game, w = windows) * 162,
    pa_in_window = weighted.mean(pa_mean, w = windows),
    windows = sum(windows),
    .groups = "drop"
  ) |>
  left_join(
    all_w |>
      group_by(window_len) |>
      summarise(
        pool_size = mean(pool_size),
        zero_pa_share = mean(pa_in_window == 0),
        distinct_players = n_distinct(paste(season, player_id)),
        .groups = "drop"
      ),
    by = "window_len"
  ) |>
  mutate(
    pa_per_game = pa_in_window / window_len,
    sprinter_advantage = olympian_net_war - war_per_162
  ) |>
  select(window_len, windows, pool_size, pa_in_window, pa_per_game,
         zero_pa_share, distinct_players, war_per_162, sprinter_advantage)

# Selection agreement with the six-game baseline, on the windows both define.
base_sel <- six |> select(season, team_id, window_idx, base_player = player_id)
agreement <- all_w |>
  inner_join(base_sel, by = c("season", "team_id", "window_idx")) |>
  group_by(window_len) |>
  summarise(
    shared_windows = n(),
    same_player_as_6g = mean(player_id == base_player),
    .groups = "drop"
  )
summary_tbl <- left_join(summary_tbl, agreement, by = "window_len")

write_csv(summary_tbl, file.path(outdir, "window_length_summary.csv"))
write_csv(by_season, file.path(outdir, "window_length_by_season.csv"))

cat("\n=== Window-length sensitivity, 2021-2025 ===\n")
summary_tbl |>
  transmute(
    L = window_len,
    windows,
    pool = round(pool_size, 2),
    `PA/win` = round(pa_in_window, 2),
    `PA/game` = round(pa_per_game, 3),
    `0-PA %` = round(100 * zero_pa_share, 1),
    players = distinct_players,
    `WAR/162` = round(war_per_162, 4),
    `vs 6g` = round(100 * same_player_as_6g, 1),
    `sprinter +` = round(sprinter_advantage, 4)
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n=== WAR/162 by season ===\n")
by_season |>
  mutate(war_162 = fwar_per_game * 162) |>
  select(window_len, season, war_162) |>
  pivot_wider(names_from = season, values_from = war_162) |>
  mutate(across(-window_len, ~ round(.x, 3))) |>
  as.data.frame() |>
  print(row.names = FALSE)

# --------------------------------------------------------------- uncertainty
# Rolling windows overlap almost completely, so they are nowhere near
# independent. Resample whole team-seasons (the natural independent cluster) to
# get a standard error on each length's WAR/162, and on the L=6 gap against the
# other lengths - the same clusters are resampled for every L, so the paired
# difference is what carries the signal.
set.seed(20260903)
B <- 2000

clusters <- all_w |>
  mutate(cl = paste(season, team_id)) |>
  select(cl, window_len, season, fwar_window)
cl_ids <- sort(unique(clusters$cl))

# Per-cluster, per-L sufficient statistics: WAR/162 is a ratio of sums, so the
# bootstrap only needs the summed fWAR and the window count in each cell.
cell <- clusters |>
  group_by(cl, window_len, season) |>
  summarise(sw = sum(fwar_window), n = n(), .groups = "drop")
season_n <- cell |>
  group_by(window_len, season) |>
  summarise(n_season = sum(n), .groups = "drop")

war162 <- function(d) {
  d |>
    group_by(window_len, season) |>
    summarise(sw = sum(sw), n = sum(n), .groups = "drop") |>
    group_by(window_len) |>
    summarise(
      war = weighted.mean(sw / n / first(window_len), w = n) * 162,
      .groups = "drop"
    )
}

boot <- vapply(seq_len(B), function(b) {
  draw <- sample(cl_ids, length(cl_ids), replace = TRUE)
  # A cluster drawn m times enters with weight m, so carry multiplicities.
  mult <- as.data.frame(table(draw), stringsAsFactors = FALSE)
  names(mult) <- c("cl", "m")
  d <- inner_join(cell, mult, by = "cl") |>
    mutate(sw = sw * m, n = n * m)
  w <- war162(d)
  w$war[match(lengths, w$window_len)]
}, numeric(length(lengths)))
rownames(boot) <- as.character(lengths)

point <- summary_tbl$war_per_162[match(lengths, summary_tbl$window_len)]
six_row <- which(lengths == 6)
# Pair within each bootstrap draw: subtract every row from the L=6 row
# column-by-column. A plain `vector - matrix` would recycle column-major.
gap <- -sweep(boot, 2, boot[six_row, ], "-")

unc <- tibble(
  window_len = lengths,
  war_per_162 = point,
  se = apply(boot, 1, sd),
  gap_vs_6g = point[six_row] - point,
  gap_se = apply(gap, 1, sd)
) |>
  mutate(
    gap_z = ifelse(window_len == 6, NA_real_, gap_vs_6g / gap_se),
    gap_p = 2 * pnorm(-abs(gap_z))
  )

write_csv(unc, file.path(outdir, "window_length_uncertainty.csv"))

cat("\n=== Cluster bootstrap (2,000 draws, resampling team-seasons) ===\n")
unc |>
  transmute(
    L = window_len,
    `WAR/162` = round(war_per_162, 4),
    SE = round(se, 4),
    `6g - L` = round(gap_vs_6g, 4),
    `gap SE` = round(gap_se, 4),
    z = round(gap_z, 2),
    p = round(gap_p, 3)
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)
