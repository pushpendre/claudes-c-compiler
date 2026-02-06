#!/bin/bash
# =============================================================================
# Initialize the bare upstream repository
# =============================================================================
# Run this once before launching agents. Creates the shared bare repo
# and seeds it with the initial directory structure.
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
UPSTREAM_DIR="$SCRIPT_DIR/upstream.git"
TEMP_CLONE="$(mktemp -d)"

if [ -d "$UPSTREAM_DIR" ]; then
    echo "upstream.git already exists. Delete it first to re-initialize."
    exit 1
fi

echo "Creating bare repo at $UPSTREAM_DIR..."
git init --bare "$UPSTREAM_DIR"

echo "Seeding initial structure..."
git clone "$UPSTREAM_DIR" "$TEMP_CLONE/code"
cd "$TEMP_CLONE/code"

git config user.name "Claude Opus 4.6"
git config user.email "noreply@anthropic.com"

# Create the coordination directories
mkdir -p current_tasks ideas projects

# Seed with empty .gitkeep files so the directories exist
touch current_tasks/.gitkeep
touch ideas/.gitkeep
touch projects/.gitkeep

git add -A
git commit -m "Initial commit: empty repo structure"
git push origin main 2>/dev/null || git push origin master

# Clean up
rm -rf "$TEMP_CLONE"

echo ""
echo "Upstream repo initialized at: $UPSTREAM_DIR"
echo "Ready to launch agents with: ./launch.sh"
