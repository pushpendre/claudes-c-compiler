# CCC Agent Orchestration Scaffolding (Reconstructed)

This directory contains a reconstruction of the scaffolding Nicholas Carlini
used to orchestrate 16 parallel Claude Code agents that built Claude's C
Compiler (CCC) -- a 100,000-line Rust-based C compiler capable of building
the Linux kernel.

**This scaffolding is NOT the original.** It was reverse-engineered from:
- The git history (3,980 commits over 13.65 days)
- Commit message patterns (Lock/Unlock protocol, "Starting new run" markers)
- File structures (`ideas/`, `current_tasks/`, `projects/`)
- Nicholas Carlini's blog post describing the architecture

## Architecture

```
Host Machine
  |
  +-- launch.sh                Spawns N Docker containers
  |
  +-- upstream.git/            Bare git repo (shared volume)
  |
  +-- Container 1..N
       |
       +-- entrypoint.sh       Infinite loop: clone -> claude -> push -> repeat
       |
       +-- /workspace/code/    Fresh clone each iteration
       |
       +-- claude              Runs with AGENT_PROMPT.md, --dangerously-skip-permissions
```

### Coordination Protocol

Agents coordinate through **git** using a simple file-based locking protocol:

1. Agent creates `current_tasks/<task>.txt` and pushes ("Lock task: ...")
2. If push fails (conflict), another agent claimed it -- pick something else
3. Agent does work, commits code changes, pushes
4. Agent deletes `current_tasks/<task>.txt` and pushes ("Remove task lock: ...")

No orchestration agent. No message bus. No database. Just git.

### Run Lifecycle

Each "run" is a batch of containers launched together:
1. First agent clears all stale task locks ("Starting new run; clearing task locks")
2. 5-16 agents race to claim tasks within minutes
3. Agents work independently for hours (6-43 hours observed)
4. Human eventually kills containers and restarts (14 restarts observed)

## Files

| File | Purpose |
|------|---------|
| `Dockerfile` | Container image with Rust, cross-compilers, Claude Code |
| `launch.sh` | Spawns N agent containers with shared upstream repo |
| `entrypoint.sh` | Per-container infinite loop (clone -> claude -> push) |
| `AGENT_PROMPT.md` | The prompt given to each Claude session |
| `init_repo.sh` | One-time setup of the bare upstream repo |
| `run_tests.sh` | Stub test runner (real test suites were external) |

## Usage

```bash
# 1. Build the container image
docker build -t ccc-agent .

# 2. Initialize the upstream repo
./init_repo.sh

# 3. Set your API key
export ANTHROPIC_API_KEY="sk-ant-..."

# 4. Launch agents (default 16)
./launch.sh 16

# 5. Watch progress
watch -n 30 'cd upstream.git && git log --oneline -20'
```

## Key Observations from the Git History

- **3,980 commits** in 13.65 days (avg 291 commits/day)
- **14 run restarts** clustering at ~06:00 and ~00:00 UTC
- **~2,006 Lock commits**, **~583 Unlock/Remove commits**
- Agents self-organized: bug fixers, documenters, refactorers, performance tuners
- Stale lock cleanup after 2 hours prevented deadlocks from crashed agents
- Merge conflicts were frequent but Claude handled them
