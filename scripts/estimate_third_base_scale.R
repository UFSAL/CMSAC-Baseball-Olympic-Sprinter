# Race parameters for a steal of THIRD base (report section 3.9).
#
# Same structure as estimate_race_scale.R for second base. Only three inputs
# change: the distance, the catcher's pop time, and the run values.
#   SCALE  - fitted on tracked throws to third base, trimmed the same way as the
#            throws to second base so the two fits are comparable.
#   CENTRE - calibrated so the league runner in a contested race reproduces the
#            observed safe rate, then shifted by the sprinter's timing edge.
#
# Writes output/reproducibility/third_base_parameters.csv
suppressPackageStartupMessages({library(dplyr); library(readr)})

ROOT <- normalizePath(".", mustWork = TRUE)
need <- function(p) { if (!file.exists(p)) stop("Missing input: ", p); p }

REALMUTO         <- 592663   # the catcher the report gives the sprinter
SEVILLE_TO_THIRD <- 3.027    # from the same kinematic fit as second base
LEAGUE_TO_THIRD  <- 3.274    # average runner who attempts a steal
BREAK_TO_RECEIPT <- 1.386
REPORT_BREAK_DISTANCE <- 70.1

throws <- read_csv(need(file.path(ROOT, "data", "reference", "savant_leaderboards",
                                  "catcher_throws_third_2021_2025.csv")),
                   show_col_types = FALSE)

fitted <- throws |>
  filter(pitchout == 0, !is.na(is_cs), !is.na(distance),
         pop_time > 1.2, pop_time < 1.9, distance > 25, distance < 65)

fit   <- glm(is_cs ~ pop_time + distance, family = binomial, data = fitted)
slope <- coef(fit)[["pop_time"]]
scale <- -1 / slope
if (scale <= 0) stop("The fitted pop-time slope has the wrong sign.")

realmuto_pop <- mean(throws$pop_time[throws$catcher == REALMUTO &
                                     throws$year == 2025 & !is.na(throws$pop_time)])
league_pop   <- mean(fitted$pop_time)
league_safe  <- 1 - mean(fitted$is_cs)
edge   <- (LEAGUE_TO_THIRD - SEVILLE_TO_THIRD) - (league_pop - realmuto_pop)
centre <- -scale * log(league_safe / (1 - league_safe)) - edge
safe   <- plogis(0, centre, scale)

receipt_distance   <- mean(fitted$distance)
covered_by_receipt <- REPORT_BREAK_DISTANCE - receipt_distance

cat(sprintf("tracked throws to third base : %d\n", nrow(throws)))
cat(sprintf("  carrying a pop time        : %d\n", sum(!is.na(throws$pop_time))))
cat(sprintf("  in the fitted sample       : %d\n", nrow(fitted)))
cat(sprintf("league safe rate (contested) : %.1f%%\n", 100 * league_safe))
cat(sprintf("league pop to third          : %.3f s\n", league_pop))
cat(sprintf("Realmuto 2025 pop to third   : %.3f s\n", realmuto_pop))
cat(sprintf("timing edge                  : %+.3f s\n", LEAGUE_TO_THIRD - SEVILLE_TO_THIRD))
cat(sprintf("catcher penalty              : %+.3f s\n", -(league_pop - realmuto_pop)))
cat(sprintf("net edge                     : %+.3f s\n", edge))
cat(sprintf("SCALE                        : %.4f s\n", scale))
cat(sprintf("CENTRE                       : %+.4f s\n", centre))
cat(sprintf("SAFE RATE                    : %.2f%%\n", 100 * safe))
cat(sprintf("runner distance at receipt   : %.1f ft (covers %.1f ft in the %.3f s to receipt)\n",
            receipt_distance, covered_by_receipt, BREAK_TO_RECEIPT))

write_csv(tibble(
  throws_total         = nrow(throws),
  throws_with_pop_time = sum(!is.na(throws$pop_time)),
  throws_fitted        = nrow(fitted),
  race_center_seconds  = centre,
  race_spread_seconds  = scale,
  safe_probability     = safe,
  league_safe_rate     = league_safe,
  league_pop_third     = league_pop,
  realmuto_pop_2025    = realmuto_pop,
  seville_to_third_s   = SEVILLE_TO_THIRD,
  league_to_third_s    = LEAGUE_TO_THIRD,
  net_edge_seconds     = edge,
  receipt_distance_ft  = receipt_distance
), file.path(ROOT, "output", "reproducibility", "third_base_parameters.csv"))
