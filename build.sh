#!/bin/bash
set -e
cd "$(dirname "$0")"

echo "=== Step 1: Extract git data to JSON ==="
python3 extract_git_data.py

echo ""
echo "=== Step 2: Generate archaeology report (optional, ~10s) ==="
if [ "$1" = "--full" ]; then
    python3 git_archaeology.py --full
elif [ "$1" = "--skip-report" ]; then
    echo "  Skipped (--skip-report)"
else
    python3 git_archaeology.py
fi

echo ""
echo "=== Step 3: Rebuild compiler_movie.html with latest data ==="
python3 rebuild_movie_html.py

echo ""
echo "=== Done ==="
echo "Open compiler_movie.html in a browser to view the animation."
echo ""
echo "Usage:"
echo "  ./build.sh              # extract data + report + rebuild HTML"
echo "  ./build.sh --skip-report  # extract data + rebuild HTML (faster)"
echo "  ./build.sh --full       # extract data + full report + rebuild HTML"
