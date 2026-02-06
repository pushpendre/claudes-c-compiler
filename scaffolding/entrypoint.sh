#!/bin/bash
# =============================================================================
# CCC Agent Entrypoint (runs inside Docker container)
# =============================================================================
# This is the "infinite agent-generation-loop" described in the blog post.
# Each iteration:
#   1. Fresh clone from /upstream
#   2. Clear stale task locks (first agent only)
#   3. Run Claude Code with the agent prompt
#   4. Claude works, commits, pushes to /upstream
#   5. Loop restarts with fresh clone
# =============================================================================

set -e

AGENT_ID="${AGENT_ID:-agent-unknown}"
LOG_DIR="/workspace/agent_logs"
mkdir -p "$LOG_DIR"

echo "[$AGENT_ID] Starting agent loop at $(date -u)"

while true; do
    echo "[$AGENT_ID] === New iteration at $(date -u) ==="

    # Fresh clone from upstream each iteration
    rm -rf /workspace/code
    git clone /upstream /workspace/code 2>/dev/null
    cd /workspace/code

    COMMIT=$(git rev-parse --short=6 HEAD 2>/dev/null || echo "empty")
    LOGFILE="$LOG_DIR/${AGENT_ID}_${COMMIT}_$(date +%s).log"

    # First agent in a new run clears stale task locks.
    # This is a race -- only the first agent to push wins.
    # Others will pull the cleared state on their next rebase.
    if ls current_tasks/*.txt 1>/dev/null 2>&1; then
        LOCK_COUNT=$(ls current_tasks/*.txt 2>/dev/null | wc -l)
        echo "[$AGENT_ID] Found $LOCK_COUNT stale task locks, attempting to clear..."
        git rm current_tasks/*.txt 2>/dev/null || true
        git commit -m "Starting new run; clearing task locks" 2>/dev/null || true
        git push 2>/dev/null || {
            echo "[$AGENT_ID] Another agent already cleared locks, pulling..."
            git pull --rebase 2>/dev/null || true
        }
    fi

    echo "[$AGENT_ID] Running Claude Code session (HEAD: $COMMIT)..."

    # Run Claude Code with the agent prompt
    # --dangerously-skip-permissions: no human approval needed
    # -p: non-interactive mode with prompt from file
    claude --dangerously-skip-permissions \
           -p "$(cat /agent_prompt.md)" \
           --model claude-opus-4-6 \
           &> "$LOGFILE" || true

    echo "[$AGENT_ID] Session ended (exit code: $?), restarting loop..."

    # Brief pause to avoid hammering if something is broken
    sleep 5
done
