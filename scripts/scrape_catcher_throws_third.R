# Pull tracked catcher throws to THIRD base from the Baseball Savant leaderboard.
# Section 3.9 fits the race-margin scale for a steal of third on this sample.
# The endpoint returns nothing without a browser User-Agent.
#
# Writes data/reference/savant_leaderboards/catcher_throws_third_2021_2025.csv
suppressPackageStartupMessages({library(jsonlite); library(dplyr); library(readr)})

ROOT <- normalizePath(".", mustWork = TRUE)
REF  <- file.path(ROOT, "data", "reference", "savant_leaderboards")
URL  <- paste0("https://baseballsavant.mlb.com/leaderboard/services/",
               "catcher-throwing/%d?season_start=2021&season_end=2025")

# Every catcher who appears in the throws-to-second sample.
ids <- read_csv(file.path(REF, "catcher_throws_tracked_2021_2025.csv"),
                show_col_types = FALSE)$catcher |> unique() |> sort()
message(length(ids), " catchers")

fetch <- function(pid, tries = 3) {
  for (attempt in seq_len(tries)) {
    out <- try({
      con <- url(sprintf(URL, pid), headers = c("User-Agent" = "Mozilla/5.0"))
      on.exit(close(con), add = TRUE)
      fromJSON(paste(readLines(con, warn = FALSE), collapse = ""))$data
    }, silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    Sys.sleep(2 * attempt)
  }
  message("  ", pid, ": giving up")
  NULL
}

rows <- list()
for (i in seq_along(ids)) {
  d <- fetch(ids[i])
  if (!is.null(d) && length(d) && nrow(d)) {
    keep <- d$target_base == "3B" & d$is_game_regular == 1
    if (any(keep)) {
      k <- d[keep, ]
      rows[[length(rows) + 1L]] <- tibble(
        year     = k$year,
        catcher  = k$catcher_id,
        pop_time = k$pop_time,
        distance = k$distance_to_target,
        lead     = k$r_primary_lead,
        sprint   = k$seasonal_sprint_speed,
        is_cs    = k$is_runner_cs,
        is_sb    = k$is_runner_sb,
        tracked  = k$is_throw_tracked,
        pitchout = k$is_pitchout
      )
    }
  }
  if (i %% 20 == 0) message("  ", i, "/", length(ids), " catchers, ",
                            sum(vapply(rows, nrow, integer(1))), " throws")
  Sys.sleep(0.25)
}

throws <- bind_rows(rows) |> arrange(year, catcher)
out <- file.path(REF, "catcher_throws_third_2021_2025.csv")
write_csv(throws, out)
message("\n", nrow(throws), " throws to third base, ",
        sum(!is.na(throws$pop_time)), " of them with a pop time")
message("written to ", out)
