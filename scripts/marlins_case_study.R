# Case study: what a pure pinch runner is worth to the 2025 Miami Marlins.
#
# For every state the decision rule selects, the inning is replayed twice. The
# first replay is the inning as it happened. The second puts the sprinter on
# first base and lets him attempt second and then third. The rest of the game is
# then played out inning by inning against the real line score, under the normal
# stopping rules, so a run in the eighth inning of a game that went to extra
# innings ends that game in nine.
#
# The per-use result is exact: the run distributions are discrete, so every
# outcome is enumerated rather than sampled. The season figures are then both
# convolved exactly and simulated, and the two must agree.
#
# Inputs   output/pinch_runner/base_out_run_distribution_2021_2025.csv
#          output/pinch_runner/marlins_2025_line_scores.csv
#          output/pinch_runner/marlins_2025_use_states.csv
#          output/reproducibility/race_parameters.csv
# Output   output/reproducibility/marlins_case_study_2025.csv
suppressPackageStartupMessages({library(dplyr); library(readr)})

ROOT <- normalizePath(".", mustWork = TRUE)
IN   <- file.path(ROOT, "output", "pinch_runner")
OUT  <- file.path(ROOT, "output", "reproducibility")
need <- function(p) { if (!file.exists(p)) stop("Missing input: ", p); p }

SEASONS <- 400000L        # simulated seasons
MAXRUN  <- 15L
set.seed(7)

P2 <- read_csv(need(file.path(OUT, "race_parameters.csv")),
               show_col_types = FALSE)$safe_probability[[1]]
P3 <- read_csv(need(file.path(OUT, "third_base_parameters.csv")),
               show_col_types = FALSE)$safe_probability[[1]]

run_dist <- read_csv(need(file.path(IN, "base_out_run_distribution_2021_2025.csv")),
                     show_col_types = FALSE)
pmf <- lapply(split(run_dist, run_dist$state),
              function(d) as.numeric(d[1, paste0("p", 0:MAXRUN)]))
cdf <- lapply(pmf, cumsum)
INNING_PMF <- pmf[["0_000"]]        # a half inning that was never played

lines <- read_csv(need(file.path(IN, "marlins_2025_line_scores.csv")), show_col_types = FALSE)
uses  <- read_csv(need(file.path(IN, "marlins_2025_use_states.csv")), show_col_types = FALSE)

games <- lapply(split(lines, lines$game_pk), function(g) {
  maxi <- g$maxi[[1]]
  top <- rep(NA_real_, maxi); bot <- rep(NA_real_, maxi)
  top[g$inning[g$half == "top"]]    <- g$runs[g$half == "top"]
  bot[g$inning[g$half == "bottom"]] <- g$runs[g$half == "bottom"]
  list(home = g$home[[1]], maxi = maxi, top = top, bot = bot)
})

# Value of the finished game to the Marlins: 1 win, 0 loss, 0.5 level after nine
# with no further real data to go on.
resolve <- function(g, use_inning, use_half, h, fill) {
  home <- g$home == "MIA"
  a <- 0; b <- 0
  for (i in seq_len(max(g$maxi, 9L))) {
    for (is_top in c(TRUE, FALSE)) {
      if (!is_top && i >= 9L && b > a) return(if (home) 1 else 0)
      batting <- xor(is_top, home)
      if (batting && i == use_inning && ((use_half == "top") == is_top)) {
        runs <- h
      } else {
        r <- if (is_top) g$top[i] else g$bot[i]
        runs <- if (i > g$maxi || is.na(r)) fill else r
      }
      if (is_top) a <- a + runs else b <- b + runs
      if (i >= 9L && !is_top && a != b) {
        return(if (isTRUE(home) == (b > a)) 1 else 0)
      }
    }
  }
  0.5
}

# The counterfactual draw is held to a result that agrees with the inning that
# actually happened: the same quantile of the base-out run distribution.
conditional <- function(state, lo, hi) {
  cs <- c(0, cdf[[state]])
  q  <- pmax(0, pmin(hi, cs[-1]) - pmax(lo, cs[-length(cs)]))
  q / (hi - lo)
}

GAINS <- c(-1, -0.5, 0, 0.5, 1)
rows <- vector("list", nrow(uses))
gain_dists <- vector("list", nrow(uses))

for (u in seq_len(nrow(uses))) {
  r <- uses[u, ]
  g <- games[[as.character(r$game_pk)]]
  outs <- r$outs; k <- r$actual_rest; pre <- r$pre
  cb <- c(0, cdf[[paste0(outs, "_100")]])
  lo <- cb[k + 1]; hi <- cb[k + 2]

  p_safe <- P2 * P3
  q_safe <- conditional(paste0(outs, "_001"), lo, hi)
  q_out  <- if (outs < 2) conditional(paste0(outs + 1, "_000"), lo, hi)
            else c(1, rep(0, MAXRUN))            # caught with two out ends it
  q <- p_safe * q_safe + (1 - p_safe) * q_out    # runs in the rest of the inning

  gd <- setNames(numeric(length(GAINS)), as.character(GAINS))
  for (f in which(INNING_PMF > 0)) {
    pf <- INNING_PMF[f]; fill <- f - 1
    vb <- resolve(g, r$inning, r$half, pre + k, fill)
    for (j in which(q > 0)) {
      vs <- resolve(g, r$inning, r$half, pre + j - 1, fill)
      gd[as.character(vs - vb)] <- gd[as.character(vs - vb)] + pf * q[j]
    }
  }
  gain_dists[[u]] <- gd
  rows[[u]] <- tibble(game_pk = r$game_pk, inning = r$inning, half = r$half,
                      outs = outs, score_diff = r$score_diff, actual_rest = k,
                      extras = g$maxi > 9,
                      run_gain = sum(q * (0:MAXRUN)) - k,
                      win_gain = sum(gd * GAINS),
                      p_win = gd["1"], p_tie = gd["0.5"],
                      p_lost_tie = gd["-0.5"], p_lost_win = gd["-1"])
}
detail <- bind_rows(rows)
write_csv(detail, file.path(OUT, "marlins_case_study_2025.csv"))

## ---- season totals: exact, then simulated ----------------------------------
grid <- seq(-nrow(uses), nrow(uses), by = 0.5)
season <- numeric(length(grid))                   # convolution, exact
season[match(0, grid)] <- 1
for (gd in gain_dists) {
  nxt <- numeric(length(grid))
  for (m in which(gd > 0)) {
    off <- (GAINS[m] * 2)
    idx <- seq_along(grid) + off
    ok  <- idx >= 1 & idx <= length(grid)
    nxt[idx[ok]] <- nxt[idx[ok]] + gd[m] * season[ok]
  }
  season <- nxt
}
exact_mean <- sum(season * grid)

draws <- matrix(0, nrow = SEASONS, ncol = 1)
for (gd in gain_dists) {
  draws <- draws + sample(GAINS, SEASONS, replace = TRUE, prob = gd)
}

cat(sprintf("\nMIA 2025: %d uses\n", nrow(uses)))
cat(sprintf("  extra runs      %+.2f\n", sum(detail$run_gain)))
cat(sprintf("  net wins        %+.3f   (exact)\n", sum(detail$win_gain)))
cat(sprintf("  net wins        %+.3f   (%s simulated seasons)\n",
            mean(draws), format(SEASONS, big.mark = ",")))
cat(sprintf("  exact season convolution agrees: %+.3f\n", exact_mean))
cat(sprintf("  improved        %.1f%% of seasons\n", 100 * mean(draws > 0)))
cat(sprintf("  loss -> win     %.3f events/season\n", sum(detail$p_win)))
cat(sprintf("  loss -> tie     %.3f events/season\n", sum(detail$p_tie)))
cat(sprintf("  win  -> tie     %.3f events/season\n", sum(detail$p_lost_tie)))
cat(sprintf("  win  -> loss    %.3f events/season\n", sum(detail$p_lost_win)))
ex <- detail |> filter(extras)
cat(sprintf("  uses in games that went to extras: %d holding %.2f of the %.2f\n",
            nrow(ex), sum(ex$win_gain), sum(detail$win_gain)))
cat(sprintf("  written to      %s\n", file.path(OUT, "marlins_case_study_2025.csv")))
