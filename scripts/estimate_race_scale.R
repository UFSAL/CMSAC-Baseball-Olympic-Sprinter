#!/usr/bin/env Rscript
# Derives the two race-margin parameters used by final_report_calculations.R and
# writes them to output/reproducibility/race_parameters.csv.
#
#   SCALE  - estimated from tracked throws. logit(P_caught) = mu / scale, and a
#            slower throw delays the tag one-for-one, so the pop-time slope is
#            -1 / scale.
#   CENTRE - calibrated. The league runner in a contested race must reproduce the
#            observed safe rate; Seville's centre is that value shifted by his
#            timing edge, which is computed here from the kinematic fits.
suppressPackageStartupMessages({library(arrow); library(dplyr)})

ROOT <- normalizePath(".", mustWork = TRUE)
need <- function(p) { if (!file.exists(p)) stop("Missing input: ", p); p }
FT <- 0.3048                       # metres per foot
BREAK_TO_RECEIPT <- 1.386          # runner's break to the catcher receiving the pitch
DECEL <- 13 * FT                   # slide friction, ft/s^2 -> m/s^2
ARRIVE <- 21.7 * FT                # highest controllable arrival speed
DIST <- 78.3 * FT                  # 90 ft less a normal primary lead
ELITE_POP <- 1.860                 # best 2025 pop time to second base
SEVILLE_REACTION <- 0.171          # his measured reaction, Paris 2024 100 m final
# The eight marks the report prints in its split table.
REPORT_MARKS_FT <- c(5, 10, 20, 30, 45, 60, 75, 90)

## ---- kinematics ------------------------------------------------------------
# v(t) = vmax - (vmax - v0) exp(-t/tau);  x(t) is its integral.
covered <- function(t, vmax, tau, v0) vmax * t - (vmax - v0) * tau * (1 - exp(-t / tau))
speed   <- function(t, vmax, tau, v0) vmax - (vmax - v0) * exp(-t / tau)

# Statcast running splits start at contact, with the batter already moving, so v0
# is fitted rather than assumed. Seville's splits start from a block, so his v0
# comes out near zero. Fitting it keeps the two sources on one footing.
fit_curve <- function(distances_m, times_s) {
  loss <- function(p) sum((covered(times_s, p[1], p[2], p[3]) - distances_m)^2)
  best <- optim(c(9, 1, 1), loss, method = "L-BFGS-B",
                lower = c(5, 0.2, -0.5), upper = c(16, 5, 4))
  best$par
}

# Time to steal second: accelerate from the common jump, then slide once the
# remaining distance is exactly what a controlled slide needs.
steal_time <- function(vmax, tau) {
  gap <- function(t) (DIST - covered(t, vmax, tau, JUMP)) -
                     (speed(t, vmax, tau, JUMP)^2 - ARRIVE^2) / (2 * DECEL)
  if (gap(0.05) < 0) return(NA_real_)
  t_slide <- if (gap(8) < 0) uniroot(gap, c(0.05, 8))$root else 8
  t_slide + (speed(t_slide, vmax, tau, JUMP) - ARRIVE) / DECEL
}

## ---- every MLB runner through that same model ------------------------------
splits <- read_parquet(need(file.path(ROOT, "data", "reference",
  "savant_leaderboards", "running_splits_2021_2025.parquet"))) |> collect()
split_cols <- grep("^seconds_since_hit_", names(splits), value = TRUE)
split_dist <- as.integer(sub("seconds_since_hit_", "", split_cols)) * FT

# A player traded mid-season gets one row per team stint. Average the stints so
# each runner-season enters the fit once.
splits <- splits |>
  group_by(season, player_id) |>
  summarise(across(all_of(split_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Fit every runner's curve once; the steal simulation needs the derived jump, so
# it happens after that is solved.
fitted_pars <- lapply(seq_len(nrow(splits)), function(i) {
  t <- unlist(splits[i, split_cols]); keep <- !is.na(t)
  if (sum(keep) < 10) NULL else fit_curve(split_dist[keep], t[keep])
})
ok <- !vapply(fitted_pars, is.null, logical(1))

throws_for_jump <- read.csv(need(file.path(ROOT, "data", "reference",
  "savant_leaderboards", "catcher_throws_tracked_2021_2025.csv"))) |>
  filter(pitchout == 0, distance > 40, distance < 70)
attempt_keys <- read.csv(need(file.path(ROOT, "data", "reference",
  "steal_attempts_2021_2025.csv")))
jump_index <- match(paste(attempt_keys$season, attempt_keys$runner_id),
                    paste(splits$season, splits$player_id))
jump_index <- jump_index[!is.na(jump_index) & ok[jump_index]]

## ---- the jump, derived rather than assumed ---------------------------------
# A base stealer is already shuffling when the clock starts, so the steal begins
# with him in motion. Savant records how far each runner still is from second
# when the catcher receives the pitch, which pins that starting speed: it is the
# value for which the fitted curves reproduce the observed receipt distance over
# the break-to-receipt interval. Reaction time does not set this - there is no
# gun on a steal, and the runner is moving before the pitch is released.
receipt_distance_m <- mean(throws_for_jump$distance) * FT
jump_gap <- function(v0_ft) {
  v0 <- v0_ft * FT
  mean(vapply(fitted_pars[jump_index], function(p)
    covered(BREAK_TO_RECEIPT, p[1], p[2], v0), numeric(1))) - (DIST - receipt_distance_m)
}
JUMP_FT <- uniroot(jump_gap, c(-2, 12))$root
JUMP <- JUMP_FT * FT

runner_time <- tibble(season = splits$season, player_id = splits$player_id,
                      steal = vapply(seq_along(fitted_pars), function(i)
                        if (is.null(fitted_pars[[i]])) NA_real_
                        else steal_time(fitted_pars[[i]][1], fitted_pars[[i]][2]),
                        numeric(1))) |>
  filter(!is.na(steal), steal > 2.5, steal < 5.5)

## ---- Seville through the identical model -----------------------------------
# His clock is read from the data file, not typed in. seville_90ft_splits.csv holds
# the Paris 2024 final converted to the basepath (cumulative seconds every 5 ft, on
# the same grid Statcast uses). That clock starts at the gun; a steal clock starts
# when the runner breaks, so his measured reaction time comes off every split. The
# curve is then fitted at the eight marks the report tabulates.
seville_splits <- read.csv(need(file.path(ROOT, "data", "reference",
  "savant_leaderboards", "seville_90ft_splits.csv")), check.names = FALSE)
sev_cols <- grep("^seconds_since_hit_", names(seville_splits), value = TRUE)
sev_marks_ft <- as.integer(sub("seconds_since_hit_", "", sev_cols))
sev_break_clock <- as.numeric(seville_splits[1, sev_cols]) - SEVILLE_REACTION

sev_at <- match(REPORT_MARKS_FT, sev_marks_ft)
if (anyNA(sev_at)) stop("seville_90ft_splits.csv is missing a reported split mark.")
seville_dist  <- REPORT_MARKS_FT * FT
seville_times <- round(sev_break_clock[sev_at], 2)   # as printed in the report

# The report prints these eight numbers, so fail loudly if the source file moves
# rather than letting the published table and the fitted curve drift apart.
if (!isTRUE(all.equal(seville_times,
                      c(0.52, 0.82, 1.27, 1.64, 2.14, 2.60, 3.04, 3.46),
                      tolerance = 1e-9))) {
  stop("Seville's derived splits no longer match the table printed in the report: ",
       paste(sprintf("%.2f", seville_times), collapse = " "))
}

sev_par  <- fit_curve(seville_dist, seville_times)
sev_time <- steal_time(sev_par[1], sev_par[2])

## ---- the edge, weighted by attempts actually made --------------------------
attempts <- read.csv(need(file.path(ROOT, "data", "reference",
                                    "steal_attempts_2021_2025.csv"))) |>
  inner_join(runner_time, by = c("season", "runner_id" = "player_id"),
             relationship = "many-to-one")
league_time <- mean(attempts$steal)
league_pop  <- mean(attempts$pop_time)
edge <- (league_time - sev_time) - (league_pop - ELITE_POP)

## ---- the scale, from tracked throws ----------------------------------------
throws <- read.csv(need(file.path(ROOT, "data", "reference", "savant_leaderboards",
                                  "catcher_throws_tracked_2021_2025.csv"))) |>
  filter(pitchout == 0, !is.na(is_cs), pop_time > 1.6, pop_time < 2.3,
         distance > 40, distance < 70)
fit   <- glm(is_cs ~ pop_time + distance, family = binomial, data = throws)
slope <- coef(fit)[["pop_time"]]
scale <- -1 / slope
if (scale <= 0) stop("The fitted pop-time slope has the wrong sign.")

## ---- the centre ------------------------------------------------------------
league_safe <- 1 - mean(throws$is_cs)
centre <- -scale * log(league_safe / (1 - league_safe)) - edge
tag_allowance <- sev_time - centre - 1.386 - ELITE_POP

cat(sprintf("jump velocity (derived)      : %.2f ft/s\n", JUMP_FT))
cat(sprintf("runner-seasons fitted        : %d\n", nrow(runner_time)))
cat(sprintf("attempts matched             : %d\n", nrow(attempts)))
cat(sprintf("league steal time (weighted) : %.3f s\n", league_time))
cat(sprintf("Seville steal time           : %.3f s\n", sev_time))
cat(sprintf("timing edge                  : %+.3f s\n", league_time - sev_time))
cat(sprintf("catcher penalty              : %+.3f s\n", -(league_pop - ELITE_POP)))
cat(sprintf("net edge                     : %+.3f s\n", edge))
cat(sprintf("tracked throws / caught      : %d / %.1f%%\n", nrow(throws), 100 * mean(throws$is_cs)))
cat(sprintf("pop-time slope               : %+.3f\n", slope))
cat(sprintf("SCALE                        : %.3f s\n", scale))
cat(sprintf("CENTRE                       : %+.3f s\n", centre))
cat(sprintf("implied tag allowance        : %.3f s\n", tag_allowance))
cat(sprintf("safe rate                    : %.1f%%\n", 100 * plogis(0, centre, scale)))

out <- file.path(ROOT, "output", "reproducibility")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(data.frame(
  race_center_seconds = centre, race_spread_seconds = scale,
  seville_steal_time = sev_time, league_steal_time = league_time,
  net_edge_seconds = edge, tag_allowance_seconds = tag_allowance,
  safe_probability = plogis(0, centre, scale)
), file.path(out, "race_parameters.csv"), row.names = FALSE)
