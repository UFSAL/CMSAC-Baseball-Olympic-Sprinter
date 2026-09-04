# Olympic Sprinter as a Pure Pinch Runner

This repository holds the paper and the analysis code for a study of the
Major League Baseball (MLB) 26-man roster. The study asks one question. Can an
Olympic-caliber sprinter give a team more value than the 13th batter?

The paper is `olympian_vs_13th_man.qmd`. It reads its numbers from the files in
`output/`. It does not hold any number in its text.

## What you must install

Install R with these packages: `dplyr`, `tidyr`, `readr`, `arrow`, `fixest`,
`sandwich`, `ggplot2`, `knitr`, and `scales`.

Install Quarto with XeLaTeX. Quarto makes the PDF.

## How to run the analysis

Start in the top directory of the repository. Then give these two commands:

```
Rscript pipeline.R
quarto render olympian_vs_13th_man.qmd --to pdf
```

The first command takes approximately 10 minutes. It writes all of the results
to `output/reproducibility/` and to `output/sensitivity/window_length/`. The
second command makes `olympian_vs_13th_man.pdf`.

## What the pipeline does

`pipeline.R` runs seven R scripts in sequence. Each script needs the results of
the script before it. The pipeline stops if a script fails.

| Step | Script | Result |
|---|---|---|
| 1 | `estimate_race_scale.R` | The race parameters for a steal of second base |
| 2 | `estimate_third_base_scale.R` | The race parameters for a steal of third base |
| 3 | `regenerate_elite_rules.R` | The steal rules for an elite catcher |
| 4 | `final_report_calculations.R` | The main model, and `report_values.csv` |
| 5 | `third_base_value.R` | The value of a steal of third base |
| 6 | `marlins_case_study.R` | The Monte Carlo simulation of the 2025 Miami Marlins |
| 7 | `window_length_sensitivity.R` | The window-length sensitivity results |

Step 4 is the largest step. It measures the value of the 13th batter, it tests
the rest effect, it solves the game-state model, and it applies that model to
the 2021-2025 seasons.

## Test of the results

We deleted every result file. Then we ran `pipeline.R` again. The pipeline made
all 13 files in `output/reproducibility/` again, and it made the three
window-length files again. Each new file was the same as the old file, byte for
byte. All 23 values in `report_values.csv` were the same. The new PDF was the
same as the old PDF, pixel for pixel.

## What this repository does not make

The pipeline starts from prepared input files. These files are in `data/` and in
`output/pinch_runner/`. The full project makes them from Statcast data. This
repository does not hold the Statcast data, because that data is 491 MB.

These input files come with the repository:

- `data/rosters_2021_2025_repaired.parquet` and `data/engine/appearance_panel.parquet`
- `output/thirteenth_man_6game_rolling_2021_2025_repaired.parquet`
- the five `thirteenth_man_*game_rolling.parquet` files in `output/sensitivity/window_length/`
- nine CSV files in `output/pinch_runner/`, which include `re24_2021_2025.csv`,
  `runs_per_win_2021_2025.csv`, and `roster_cost_2021_2025.csv`

Three scripts in `scripts/` are not part of `pipeline.R`. Do not run them here:

- `build_marlins_inputs.R` needs the Statcast data.
- `window_length_sensitivity.sh` needs `engine_07_windows.sql`. It also needs bash and duckdb.
- `scrape_catcher_throws_third.R` gets data from the network. Its result is
  already in `data/reference/savant_leaderboards/`.

## The files in this repository

| Path | Contents |
|---|---|
| `olympian_vs_13th_man.qmd` | The paper |
| `references.bib` | The references |
| `pipeline.R` | The pipeline. It runs the seven steps. |
| `scripts/` | The analysis scripts |
| `data/` | The prepared input data |
| `output/pinch_runner/` | The prepared input tables |
| `output/reproducibility/` | The results of the pipeline |
| `output/sensitivity/window_length/` | The window-length sensitivity results |
