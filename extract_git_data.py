#!/usr/bin/env python3
"""
Extract comprehensive git data from the claudes-c-compiler repository
for animated visualization. Outputs git_timeline_data.json.

Performance: Uses 2 git commands total, runs in <30 seconds.
"""

import json
import re
import subprocess
import sys
import time
from datetime import datetime

REPO_DIR = "/Users/p/w/tp/claudes-c-compiler"
OUTPUT_FILE = "/Users/p/w/tp/claudes-c-compiler/git_timeline_data.json"
CLASSIFICATION_CACHE = "/Users/p/w/tp/claudes-c-compiler/commit_classifications_cache.json"

# Separator unlikely to appear in commit messages
SEP = "|||FIELD|||"
REC_SEP = "|||RECORD|||"

# Load AI classification cache if available
_classification_cache = {}
try:
    with open(CLASSIFICATION_CACHE) as _f:
        _classification_cache = json.load(_f)
    print(f"  Loaded AI classification cache: {len(_classification_cache)} entries")
except FileNotFoundError:
    pass

def run_git(args, description=""):
    """Run a git command and return stdout."""
    cmd = ["git", "-C", REPO_DIR] + args
    if description:
        print(f"  Running: {description}...")
    start = time.time()
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=120)
    elapsed = time.time() - start
    if result.returncode != 0:
        print(f"  WARNING: git command failed: {' '.join(args[:5])}...")
        print(f"  stderr: {result.stderr[:500]}")
    else:
        print(f"  Done in {elapsed:.1f}s ({len(result.stdout)} bytes)")
    return result.stdout


def classify_feature_area(subject):
    """Classify a commit's feature area. Uses AI cache if available, falls back to regex."""
    # Use AI classification cache if available
    if subject in _classification_cache:
        return _classification_cache[subject]

    s = subject

    # Order matters: more specific patterns first
    # Task management (check early since lock commits may also match other areas)
    if re.search(r'^Lock\b|^Unlock\b|^Take lock\b|^Remove task lock|\btask lock\b|Lock task:', s):
        # But also classify the actual area if it's a lock for a specific task
        # We return task_mgmt for pure lock/unlock messages
        # Check if there's a more specific area in the lock message
        area = _classify_content(s)
        if area != "other":
            return area
        return "task_mgmt"

    return _classify_content(s)


def _classify_content(s):
    """Inner classification based on content keywords."""
    # Preprocessor
    if re.search(r'preprocessor|preprocess|\bmacro\b|#include|#define|\bCPP\b|header search|include path', s, re.IGNORECASE):
        return "preprocessor"

    # Inline asm (before assembler, since "inline as" is specific)
    if re.search(r'inline as|inline_asm|inline asm', s, re.IGNORECASE):
        return "inline_asm"

    # Intrinsics (before backends, since SSE/AVX/NEON are specific)
    if re.search(r'intrinsic|builtin|__builtin|\bSSE\b|\bAVX\b|\bNEON\b|\bmmx\b|mmintrin|xmmintrin|emmintrin|smmintrin|shaintrin|immintrin|bmmintrin|pmmintrin|tmmintrin|nmmintrin|wmmintrin|arm_neon', s, re.IGNORECASE):
        return "intrinsics"

    # Kernel
    if re.search(r'kernel|Linux|defconfig|tinyconfig', s, re.IGNORECASE):
        return "kernel"

    # i686 backend (before x86 to avoid overlap)
    if re.search(r'i686|32-bit x86|32bit x86|i386', s, re.IGNORECASE):
        return "i686_backend"

    # x86 backend
    if re.search(r'x86|x86-64|x86_64|X86|_x86_', s):
        return "x86_backend"

    # ARM backend (also match _arm_ in task lock names where \b doesn't work)
    if re.search(r'\bARM\b|\bAArch64\b|\baarch64\b|\barm\b|_arm_|arm64', s):
        return "arm_backend"

    # RISC-V backend
    if re.search(r'RISC-V|riscv|risc-v|RISCV|_riscv_', s, re.IGNORECASE):
        return "riscv_backend"

    # IR / optimization
    if re.search(r'\bIR\b|Ir[A-Z]|optimization|optimi[sz]|SSA\b|\bDCE\b|\bGVN\b|constant fold|mem2reg|simplif|dead code elim|peephole|register alloc|reg.?alloc|codegen.*optim|phi.*node|basic block', s, re.IGNORECASE):
        return "ir_optimization"

    # Assembler
    if re.search(r'assembler|\basm\b|\.asm\b|\bELF\b|\belf\b|reloc|\.section|\.globl|\.text|GAS |gas |object file|\.o file|DWARF|dwarf|debug info', s, re.IGNORECASE):
        return "assembler"

    # Linker
    if re.search(r'linker|\blink\b|\.so\b|shared lib|dynamic|static lib|archive|\.a file|GOT|PLT|symbol resol', s, re.IGNORECASE):
        return "linker"

    # Type system
    if re.search(r'\btype\b|typedef|\benum\b|\bstruct\b|\bunion\b|\bsizeof\b|\balignof\b|type.*system|type.*check|cast|conversion|bitfield|bit.?field|_Bool|variadic|va_arg|va_list|va_start', s, re.IGNORECASE):
        return "type_system"

    # Parser
    if re.search(r'parser|\bAST\b|parse[rd]?\b|parsing|syntax|lexer|token|declaration|expression|statement', s, re.IGNORECASE):
        return "parser"

    # Scaffold
    if re.search(r'scaffold|initial|Initial commit|empty repo', s, re.IGNORECASE):
        return "scaffold"

    # Documentation
    if re.search(r'README|\.md\b|\bdoc\b|\bdocs\b|comment|documentation|DESIGN|design doc|LICENSE|DISCLAIMER', s, re.IGNORECASE):
        return "documentation"

    # Cleanup
    if re.search(r'cleanup|clean.?up|clippy|dead code|refactor|dedup|deduplic|lint|rustfmt|format|style fix|remove unused|simplify code', s, re.IGNORECASE):
        return "cleanup"

    # Testing
    if re.search(r'\btest\b|suite|testing|test.?case|benchmark', s, re.IGNORECASE):
        return "testing"

    return "other"


def extract_lock_name(subject):
    """Extract the task name from a lock/unlock commit message."""
    # Patterns:
    # "Lock task: <name>"
    # "Lock: <name>"
    # "Unlock task: <name>"
    # "Remove task lock: <name>"
    m = re.match(r'(?:Lock task|Lock|Unlock task|Unlock|Remove task lock):\s*(.+)', subject, re.IGNORECASE)
    if m:
        name = m.group(1).strip()
        # Clean up trailing status markers
        name = re.sub(r'\s*\(completed\)\s*$', '', name, flags=re.IGNORECASE)
        name = re.sub(r'\s*\(done\)\s*$', '', name, flags=re.IGNORECASE)
        name = re.sub(r'\s*completed\s*$', '', name, flags=re.IGNORECASE)
        name = re.sub(r'\s*fixed\s*$', '', name, flags=re.IGNORECASE)
        return name.strip()
    return None


def is_lock_commit(subject):
    """Check if this is a lock commit (task being claimed)."""
    s = subject.strip()
    if re.match(r'^Lock\s*task\s*:', s, re.IGNORECASE):
        return True
    if re.match(r'^Lock\s*:', s, re.IGNORECASE):
        # Make sure it's not "Lock" in the middle of something else
        return True
    return False


def is_unlock_commit(subject):
    """Check if this is an unlock commit (task completed/released)."""
    s = subject.strip()
    if re.match(r'^Unlock\s*task\s*:', s, re.IGNORECASE):
        return True
    if re.match(r'^Unlock\s*:', s, re.IGNORECASE):
        return True
    if re.match(r'^Remove\s+task\s+lock\s*:', s, re.IGNORECASE):
        return True
    return False


def normalize_task_name(name):
    """Normalize a task name for fuzzy matching."""
    if not name:
        return ""
    n = name.lower().strip()
    # Remove underscores, hyphens, extra spaces
    n = re.sub(r'[_\-]+', ' ', n)
    n = re.sub(r'\s+', ' ', n)
    # Remove common suffixes
    n = re.sub(r'\s*\(.*?\)\s*', '', n)
    return n.strip()


def fuzzy_match_task(name, candidates):
    """Try to find the best matching task name from candidates."""
    if not name:
        return None
    norm = normalize_task_name(name)

    # Exact match first
    for c in candidates:
        if normalize_task_name(c) == norm:
            return c

    # Substring match
    for c in candidates:
        cn = normalize_task_name(c)
        if norm in cn or cn in norm:
            return c

    # Word overlap match
    norm_words = set(norm.split())
    best_match = None
    best_score = 0
    for c in candidates:
        cn = normalize_task_name(c)
        c_words = set(cn.split())
        if not c_words or not norm_words:
            continue
        overlap = len(norm_words & c_words)
        score = overlap / max(len(norm_words), len(c_words))
        if score > best_score and score > 0.5:
            best_score = score
            best_match = c

    return best_match


def main():
    print("=" * 60)
    print("Git Data Extraction for claudes-c-compiler visualization")
    print("=" * 60)

    t_start = time.time()

    # ─── Git Command 1: All commit metadata ───────────────────
    print("\n[1/2] Fetching commit metadata...")
    author_filter = "Claude Opus 4.6 <noreply@anthropic.com>"
    fmt = f"%h{SEP}%aI{SEP}%at{SEP}%s{REC_SEP}"
    raw_meta = run_git(
        ["log", "--reverse", f"--author={author_filter}", f"--format={fmt}"],
        "git log --format (metadata)"
    )

    # ─── Git Command 2: Shortstat for insertions/deletions ────
    print("\n[2/2] Fetching shortstat data...")
    raw_stats = run_git(
        ["log", "--reverse", f"--author={author_filter}", "--shortstat", "--format=COMMIT_START %H"],
        "git log --shortstat (insertions/deletions)"
    )

    # ─── Parse metadata ───────────────────────────────────────
    print("\nParsing commit metadata...")
    records = raw_meta.strip().split(REC_SEP)
    records = [r.strip() for r in records if r.strip()]

    commits = []
    for rec in records:
        fields = rec.split(SEP)
        if len(fields) < 4:
            continue
        short_hash = fields[0].strip()
        iso_time = fields[1].strip()
        epoch = int(fields[2].strip())
        subject = fields[3].strip()

        commits.append({
            "hash": short_hash,
            "timestamp": iso_time,
            "epoch": epoch,
            "subject": subject,
            "insertions": 0,
            "deletions": 0,
            "files_changed": 0,
            "is_lock": is_lock_commit(subject),
            "is_unlock": is_unlock_commit(subject),
            "lock_name": extract_lock_name(subject),
            "feature_area": classify_feature_area(subject),
        })

    print(f"  Parsed {len(commits)} commits from metadata")

    # ─── Parse shortstat ──────────────────────────────────────
    print("Parsing shortstat data...")

    # Split by COMMIT_START to get per-commit blocks
    stat_blocks = raw_stats.split("COMMIT_START ")
    stat_blocks = [b.strip() for b in stat_blocks if b.strip()]

    # Build a dict: full_hash -> (files_changed, insertions, deletions)
    stat_by_hash = {}
    for block in stat_blocks:
        lines = block.strip().split('\n')
        if not lines:
            continue
        full_hash = lines[0].strip()
        short_hash = full_hash[:8]

        files_changed = 0
        insertions = 0
        deletions = 0

        for line in lines[1:]:
            line = line.strip()
            if not line:
                continue
            # Parse: "59 files changed, 5657 insertions(+), 11 deletions(-)"
            m_files = re.search(r'(\d+)\s+files?\s+changed', line)
            m_ins = re.search(r'(\d+)\s+insertions?\(\+\)', line)
            m_del = re.search(r'(\d+)\s+deletions?\(-\)', line)
            if m_files:
                files_changed = int(m_files.group(1))
            if m_ins:
                insertions = int(m_ins.group(1))
            if m_del:
                deletions = int(m_del.group(1))

        stat_by_hash[short_hash] = (files_changed, insertions, deletions)

    print(f"  Parsed {len(stat_by_hash)} stat entries")

    # Merge stats into commits
    matched = 0
    for c in commits:
        h = c["hash"]
        if h in stat_by_hash:
            fc, ins, dels = stat_by_hash[h]
            c["files_changed"] = fc
            c["insertions"] = ins
            c["deletions"] = dels
            matched += 1

    print(f"  Merged stats for {matched}/{len(commits)} commits")

    # ─── Build task_locks ─────────────────────────────────────
    print("\nBuilding task lock lifecycle data...")

    # Collect all locks
    lock_events = []  # (epoch, hash, is_lock, task_name, subject)
    for c in commits:
        if c["is_lock"]:
            lock_events.append((c["epoch"], c["hash"], True, c["lock_name"], c["subject"]))
        elif c["is_unlock"]:
            lock_events.append((c["epoch"], c["hash"], False, c["lock_name"], c["subject"]))

    # Build lock records: for each lock, find the corresponding unlock
    active_locks = {}  # normalized_name -> {name, lock_time, lock_commit, feature_area}
    task_locks = []

    # Process in chronological order (commits are already chronological)
    for epoch, commit_hash, is_lock, task_name, subject in lock_events:
        if not task_name:
            continue

        if is_lock:
            norm = normalize_task_name(task_name)
            active_locks[norm] = {
                "name": task_name,
                "lock_time": epoch,
                "unlock_time": None,
                "lock_commit": commit_hash,
                "unlock_commit": None,
                "feature_area": classify_feature_area(subject),
            }
        else:
            # Try to find matching lock
            norm = normalize_task_name(task_name)
            matched_key = None

            if norm in active_locks:
                matched_key = norm
            else:
                # Fuzzy match
                match = fuzzy_match_task(task_name,
                    [active_locks[k]["name"] for k in active_locks])
                if match:
                    matched_key = normalize_task_name(match)

            if matched_key and matched_key in active_locks:
                lock_rec = active_locks.pop(matched_key)
                lock_rec["unlock_time"] = epoch
                lock_rec["unlock_commit"] = commit_hash
                task_locks.append(lock_rec)
            else:
                # Unlock without a matching lock - create a standalone record
                task_locks.append({
                    "name": task_name,
                    "lock_time": None,
                    "unlock_time": epoch,
                    "lock_commit": None,
                    "unlock_commit": commit_hash,
                    "feature_area": classify_feature_area(subject),
                })

    # Add remaining active (never unlocked) locks
    for norm, lock_rec in active_locks.items():
        task_locks.append(lock_rec)

    # Sort by lock_time (or unlock_time if lock_time is None)
    task_locks.sort(key=lambda x: x["lock_time"] or x["unlock_time"] or 0)

    print(f"  Found {len(task_locks)} task lock records")
    print(f"    - Completed (lock+unlock): {sum(1 for t in task_locks if t['lock_time'] and t['unlock_time'])}")
    print(f"    - Still active (lock only): {sum(1 for t in task_locks if t['lock_time'] and not t['unlock_time'])}")
    print(f"    - Orphan unlocks: {sum(1 for t in task_locks if not t['lock_time'] and t['unlock_time'])}")

    # ─── Build growth_snapshots ───────────────────────────────
    print("\nBuilding growth snapshots (every 20 commits)...")

    growth_snapshots = []
    cumulative_lines = 0
    cumulative_files = set()  # We'll approximate with files_changed

    # We need a rough total_files count. We'll track cumulative insertions - deletions
    # as a proxy for "total lines" and use a running count.
    running_ins = 0
    running_del = 0

    for i, c in enumerate(commits):
        running_ins += c["insertions"]
        running_del += c["deletions"]

        if i % 20 == 0 or i == len(commits) - 1:
            growth_snapshots.append({
                "commit_index": i,
                "timestamp": c["epoch"],
                "total_lines": running_ins - running_del,
                "cumulative_insertions": running_ins,
                "cumulative_deletions": running_del,
            })

    print(f"  Generated {len(growth_snapshots)} snapshots")

    # ─── Build metadata ───────────────────────────────────────
    print("\nBuilding metadata...")

    first_epoch = commits[0]["epoch"]
    last_epoch = commits[-1]["epoch"]
    duration_seconds = last_epoch - first_epoch
    duration_hours = duration_seconds / 3600.0

    # Feature area distribution
    area_counts = {}
    for c in commits:
        area = c["feature_area"]
        area_counts[area] = area_counts.get(area, 0) + 1

    metadata = {
        "total_commits": len(commits),
        "first_commit_time": first_epoch,
        "last_commit_time": last_epoch,
        "duration_hours": round(duration_hours, 2),
        "duration_days": round(duration_hours / 24, 2),
        "total_insertions": running_ins,
        "total_deletions": running_del,
        "net_lines": running_ins - running_del,
        "feature_area_distribution": dict(sorted(area_counts.items(), key=lambda x: -x[1])),
    }

    print(f"  Total commits: {metadata['total_commits']}")
    print(f"  Duration: {metadata['duration_hours']} hours ({metadata['duration_days']} days)")
    print(f"  Total insertions: {metadata['total_insertions']:,}")
    print(f"  Total deletions: {metadata['total_deletions']:,}")
    print(f"  Net lines: {metadata['net_lines']:,}")
    print(f"\n  Feature area distribution:")
    for area, count in sorted(area_counts.items(), key=lambda x: -x[1]):
        print(f"    {area:20s}: {count:4d} commits ({100*count/len(commits):.1f}%)")

    # ─── Assemble and write JSON ──────────────────────────────
    print("\nWriting JSON output...")

    output = {
        "metadata": metadata,
        "commits": commits,
        "task_locks": task_locks,
        "growth_snapshots": growth_snapshots,
    }

    with open(OUTPUT_FILE, 'w') as f:
        json.dump(output, f, indent=2)

    import os
    file_size = os.path.getsize(OUTPUT_FILE)
    print(f"  Written to: {OUTPUT_FILE}")
    print(f"  File size: {file_size:,} bytes ({file_size/1024/1024:.2f} MB)")

    elapsed = time.time() - t_start
    print(f"\nTotal time: {elapsed:.1f}s")

    # ─── Print sample ─────────────────────────────────────────
    print("\n" + "=" * 60)
    print("Sample: First 2 commits:")
    print("=" * 60)
    for c in commits[:2]:
        print(json.dumps(c, indent=2))

    print("\n" + "=" * 60)
    print("Sample: First 2 task locks:")
    print("=" * 60)
    for t in task_locks[:2]:
        print(json.dumps(t, indent=2))

    print("\nDone!")


if __name__ == "__main__":
    main()
