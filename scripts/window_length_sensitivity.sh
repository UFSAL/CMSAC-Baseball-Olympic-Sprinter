#!/usr/bin/env bash
# Window-length sensitivity for the 13th-man selection.
#
# engine_07_windows.sql hardcodes a six-game window in three places: the window
# admissibility bound, the window/game join, and the per-game fWAR divisor. This
# runs that same SQL, unmodified on disk, for L = 4..8 against the repaired
# roster panel the published results use, and writes one rolling parquet per L.
#
#   scripts/window_length_sensitivity.sh [roster.parquet]
#
# L=6 must reproduce output/thirteenth_man_6game_rolling_2021_2025_repaired.parquet
# exactly; scripts/window_length_sensitivity.R checks that.

set -euo pipefail

ROSTERS="${1:-data/rosters_2021_2025_repaired.parquet}"
SQL="scripts/engine_07_windows.sql"
OUTDIR="output/sensitivity/window_length"

for f in "$SQL" "$ROSTERS"; do
  [ -f "$f" ] || { echo "Missing required file: $f" >&2; exit 1; }
done
command -v duckdb >/dev/null || { echo "duckdb is not on PATH." >&2; exit 1; }

mkdir -p "$OUTDIR"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

for L in 4 5 6 7 8; do
  off=$((L - 1))
  ROLLING="$OUTDIR/thirteenth_man_${L}game_rolling.parquet"
  DETAIL="$work/detail_${L}.parquet"
  patched="$work/engine_07_L${L}.sql"

  # Only the window length and the file paths change. The pool rule, the catcher
  # exclusion and the tie-break order are left exactly as the engine defines them.
  sed -e "s|tg.seq + 5 <= mx.max_seq|tg.seq + ${off} <= mx.max_seq|" \
      -e "s|w.window_idx AND w.window_idx + 5|w.window_idx AND w.window_idx + ${off}|" \
      -e "s|/ 6.0 AS fwar_per_game_6g_avg|/ ${L}.0 AS fwar_per_game_6g_avg|" \
      -e "s|'data/rosters_2021_2025.parquet'|'${ROSTERS}'|g" \
      -e "s|'output/thirteenth_man_6game_rolling_2021_2025.parquet'|'${ROLLING}'|g" \
      -e "s|'output/thirteenth_man_game_detail_2021_2025.parquet'|'${DETAIL}'|g" \
      "$SQL" > "$patched"

  # Guard against a silent no-op if any of those literals are ever renamed.
  for lit in "tg.seq + ${off} <= mx.max_seq" \
             "w.window_idx AND w.window_idx + ${off}" \
             "/ ${L}.0 AS fwar_per_game_6g_avg" \
             "'${ROSTERS}'" "'${ROLLING}'"; do
    grep -qF "$lit" "$patched" || {
      echo "Substitution failed for: $lit - check the literals in $SQL." >&2
      exit 1
    }
  done

  echo "=== L=${L} -> ${ROLLING}"
  duckdb < "$patched" >/dev/null
done

echo
echo "Done. Run scripts/window_length_sensitivity.R to summarise."
