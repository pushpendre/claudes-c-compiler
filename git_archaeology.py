#!/usr/bin/env python3
"""
Git Archaeology: Comprehensive analysis of the Claude's C Compiler git history.

Analyzes ~3982 commits from 16 parallel Claude AI agents building a C compiler
in Rust over ~13 days (2026-01-23 to 2026-02-05).

Usage:
    python3 git_archaeology.py          # Fast mode (sampled analysis)
    python3 git_archaeology.py --full   # Full analysis (every commit)
"""

import subprocess
import sys
import re
import json
from collections import Counter, defaultdict
from datetime import datetime, timedelta
from dataclasses import dataclass, field
from typing import List, Dict, Tuple, Optional
import math
import argparse
import textwrap


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def run_git(args: List[str], timeout: int = 120) -> str:
    """Run a git command and return stdout."""
    try:
        result = subprocess.run(
            ["git"] + args,
            capture_output=True, text=True, timeout=timeout
        )
        return result.stdout.strip()
    except subprocess.TimeoutExpired:
        return ""
    except Exception as e:
        return ""


def progress(current: int, total: int, label: str = ""):
    """Print a progress indicator."""
    pct = int(100 * current / total) if total else 0
    bar_len = 30
    filled = int(bar_len * current / total) if total else 0
    bar = "#" * filled + "-" * (bar_len - filled)
    sys.stderr.write(f"\r  [{bar}] {pct:3d}% {label}")
    sys.stderr.flush()
    if current >= total:
        sys.stderr.write("\n")


def parse_datetime(date_str: str) -> Optional[datetime]:
    """Parse a git date string like '2026-01-23 01:04:22 +0000'."""
    try:
        # Strip timezone offset for simplicity -- all commits are +0000
        parts = date_str.strip().rsplit(" ", 1)
        return datetime.strptime(parts[0], "%Y-%m-%d %H:%M:%S")
    except (ValueError, IndexError):
        return None


def fmt_duration(seconds: float) -> str:
    """Format seconds into a human-readable duration."""
    if seconds < 0:
        return f"-{fmt_duration(-seconds)}"
    if seconds < 60:
        return f"{seconds:.0f}s"
    elif seconds < 3600:
        return f"{seconds / 60:.1f}m"
    elif seconds < 86400:
        return f"{seconds / 3600:.1f}h"
    else:
        return f"{seconds / 86400:.1f}d"


# Cache the empty tree hash
_EMPTY_TREE_HASH = None

def get_empty_tree_hash() -> str:
    """Get the empty tree hash for the current git installation."""
    global _EMPTY_TREE_HASH
    if _EMPTY_TREE_HASH is None:
        _EMPTY_TREE_HASH = run_git(["hash-object", "-t", "tree", "/dev/null"])
        if not _EMPTY_TREE_HASH:
            # Fallback: try the well-known SHA-1 empty tree hash
            _EMPTY_TREE_HASH = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
    return _EMPTY_TREE_HASH


def ascii_bar(value: float, max_value: float, width: int = 50) -> str:
    """Create a simple ASCII bar."""
    if max_value <= 0:
        return ""
    filled = int(width * value / max_value)
    filled = max(0, min(width, filled))
    return "=" * filled


def truncate(s: str, length: int = 60) -> str:
    """Truncate a string to a given length."""
    if len(s) <= length:
        return s
    return s[:length - 3] + "..."


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class Commit:
    hash: str
    date: datetime
    subject: str

    @property
    def date_str(self) -> str:
        return self.date.strftime("%Y-%m-%d %H:%M")


# ---------------------------------------------------------------------------
# Data collection
# ---------------------------------------------------------------------------

def get_all_commits() -> List[Commit]:
    """Fetch all commits (hash, date, subject) in chronological order."""
    sys.stderr.write("  Loading commit log...\n")
    raw = run_git(["log", "--format=%H|%ai|%s", "--reverse"], timeout=60)
    commits = []
    for line in raw.splitlines():
        parts = line.split("|", 2)
        if len(parts) < 3:
            continue
        h, date_str, subj = parts
        dt = parse_datetime(date_str)
        if dt is None:
            continue
        commits.append(Commit(hash=h.strip(), date=dt, subject=subj.strip()))
    return commits


def get_diff_stats_between(hash_a: str, hash_b: str) -> Tuple[int, int, int]:
    """Get (files_changed, insertions, deletions) between two commits."""
    raw = run_git(["diff", "--shortstat", hash_a, hash_b], timeout=30)
    files, ins, dels = 0, 0, 0
    if not raw:
        return files, ins, dels
    m = re.search(r"(\d+) file", raw)
    if m:
        files = int(m.group(1))
    m = re.search(r"(\d+) insertion", raw)
    if m:
        ins = int(m.group(1))
    m = re.search(r"(\d+) deletion", raw)
    if m:
        dels = int(m.group(1))
    return files, ins, dels


def get_commit_numstat(commit_hash: str) -> Tuple[int, int, List[str]]:
    """Get (lines_added, lines_removed, [files]) for a single commit."""
    raw = run_git(["diff", "--numstat", f"{commit_hash}^", commit_hash], timeout=30)
    added, removed = 0, 0
    files = []
    for line in raw.splitlines():
        parts = line.split("\t")
        if len(parts) < 3:
            continue
        try:
            a = int(parts[0]) if parts[0] != "-" else 0
            r = int(parts[1]) if parts[1] != "-" else 0
            added += a
            removed += r
            files.append(parts[2])
        except ValueError:
            continue
    return added, removed, files


def get_file_count_at(commit_hash: str) -> int:
    """Count tracked files at a given commit."""
    raw = run_git(["ls-tree", "-r", "--name-only", commit_hash], timeout=30)
    if not raw:
        return 0
    return len(raw.splitlines())


def get_total_lines_at(commit_hash: str) -> int:
    """Count total lines in the repo at a given commit (fast: use diffstat from empty tree)."""
    empty_hash = get_empty_tree_hash()
    raw = run_git(["diff", "--shortstat", empty_hash, commit_hash], timeout=30)
    ins = 0
    m = re.search(r"(\d+) insertion", raw)
    if m:
        ins = int(m.group(1))
    return ins


# ---------------------------------------------------------------------------
# Feature keyword mapping
# ---------------------------------------------------------------------------

FEATURE_KEYWORDS = {
    "preprocessor": [
        r"\bpreprocess", r"\b#include\b", r"\b#define\b", r"\bmacro\b",
        r"\bheader guard", r"\b#if\b", r"\b#ifdef\b", r"\binclude path",
        r"\bcpp\b", r"\btoken past", r"\bstringif", r"\bpragma\b",
        r"\b__has_", r"\b_Pragma\b", r"\binclude.?next",
    ],
    "parser/AST": [
        r"\bparser\b", r"\bAST\b", r"\bparse\b", r"\bsyntax\b",
        r"\bdeclaration\b", r"\bexpression\b", r"\bstatement\b",
        r"\btypedef\b", r"\bgrammar\b", r"\bprecedence\b",
    ],
    "type system": [
        r"\btype\s?(check|system|cast|promot|conver)", r"\bstruct\b",
        r"\bunion\b", r"\benum\b", r"\bbitfield\b", r"\bbit.?field",
        r"\btypedef\b", r"\bsizeof\b", r"\balignof\b", r"\bva_arg\b",
        r"\bva_list\b", r"\bvolatile\b", r"\bqualifier\b",
        r"\blong double\b", r"\b__int128\b", r"\bcomplex\b",
    ],
    "IR/optimization": [
        r"\bIR\b", r"\boptimiz", r"\bpass\b", r"\bSSA\b",
        r"\bconstant fold", r"\bdead code", r"\bsimplif",
        r"\bpeephole\b", r"\binline\b", r"\btailcall\b",
        r"\bdiv.?by.?const", r"\bstrength.?reduc", r"\bregister.?alloc",
    ],
    "x86/x86-64 backend": [
        r"\bx86\b", r"\bx86.64\b", r"\bi686\b", r"\bx64\b",
        r"\bSSE\b", r"\bAVX\b", r"\bXMM\b", r"\bSIMD.*x86",
        r"\bmmx\b", r"\bsse[234]", r"\bssse3", r"\bpclmul",
        r"\baes.?ni\b", r"\bsha\s?intrin",
    ],
    "ARM/AArch64 backend": [
        r"\bARM\b", r"\baarch64\b", r"\barm64\b", r"\bNEON\b",
        r"\bARM64\b", r"\bA64\b",
    ],
    "RISC-V backend": [
        r"\brisc.?v\b", r"\brv32\b", r"\brv64\b",
    ],
    "assembler": [
        r"\bassembl", r"\bGAS\b", r"\b\.text\b", r"\b\.data\b",
        r"\bdirective\b", r"\breloc", r"\bELF\b",
        r"\b\.section\b", r"\b\.org\b", r"\b\.align\b",
        r"\b\.skip\b", r"\b\.set\b",
    ],
    "linker": [
        r"\blinker\b", r"\blink\b", r"\bld\b", r"\bsymbol\s?resolut",
        r"\bGOT\b", r"\bPLT\b", r"\bshared\s?lib", r"\bdynamic",
        r"\barchive\b", r"\b\.a\b\s", r"\b\.so\b",
    ],
    "Linux kernel": [
        r"\bkernel\b", r"\bdefconfig\b", r"\blinux\b", r"\bvmlinux\b",
        r"\bkbuild\b", r"\bspinlock\b", r"\bkconfig\b",
    ],
    "intrinsics/builtins": [
        r"\bintrins", r"\bbuiltin\b", r"\b__builtin", r"\bmmintrin",
        r"\bemmintrin", r"\bxmmintrin", r"\bimmintrin", r"\btmmintrin",
        r"\bnmmintrin", r"\bsmmintrin", r"\bwmmintrin", r"\bpmmintrin",
        r"\bbmi[12]?intrin", r"\bf16cintrin", r"\blzcntintrin",
    ],
    "inline assembly": [
        r"\binline\s?asm", r"\b__asm__\b", r"\basm\b.*constraint",
        r"\basm\s?volatile", r"\boperand.*asm", r"\basm.*operand",
    ],
    "scaffold/infrastructure": [
        r"\bscaffold\b", r"\binitial commit\b", r"\bCI\b", r"\bbuild\b",
        r"\bCargo\b", r"\btest harness\b", r"\binfrastructure\b",
        r"\bCLI\b", r"\bdriver\b",
    ],
    "documentation/README": [
        r"\bREADME\b", r"\bdoc\b", r"\bdocument", r"\bdesign\b",
        r"\bLICENSE\b", r"\bdisclaimer\b",
    ],
    "task management": [
        r"^Lock\s?task:", r"^Unlock\s?task:", r"^Remove\s?task\s?lock:",
        r"\btask\s?lock\b", r"^Lock:", r"^Unlock:",
    ],
}


def classify_commit(subject: str) -> List[str]:
    """Classify a commit into one or more feature areas."""
    areas = []
    for area, patterns in FEATURE_KEYWORDS.items():
        for pat in patterns:
            if re.search(pat, subject, re.IGNORECASE):
                areas.append(area)
                break
    return areas if areas else ["other"]


# ---------------------------------------------------------------------------
# Analysis sections
# ---------------------------------------------------------------------------

def section_header(title: str) -> str:
    width = 78
    return (
        "\n" + "=" * width + "\n"
        + f"  {title}".center(width) + "\n"
        + "=" * width + "\n"
    )


def analyze_timeline_growth(commits: List[Commit], sample_step: int):
    """Section 1: Timeline & Growth."""
    print(section_header("1. TIMELINE & GROWTH"))

    total = len(commits)
    print(f"  Total commits: {total}")
    print(f"  First commit:  {commits[0].date_str}  --  {commits[0].subject}")
    print(f"  Last commit:   {commits[-1].date_str}  --  {commits[-1].subject}")
    span = commits[-1].date - commits[0].date
    print(f"  Time span:     {span.days} days, {span.seconds // 3600} hours")
    print()

    # Sample commits
    indices = list(range(0, total, sample_step))
    if indices[-1] != total - 1:
        indices.append(total - 1)

    samples = []  # (commit, loc, files)
    sys.stderr.write(f"  Sampling {len(indices)} points for growth analysis...\n")

    for i, idx in enumerate(indices):
        progress(i, len(indices), f"commit {idx}/{total}")
        c = commits[idx]
        loc = get_total_lines_at(c.hash)
        nfiles = get_file_count_at(c.hash)
        samples.append((c, loc, nfiles))
    progress(len(indices), len(indices), "done")

    # Also compute cumulative adds/removes between consecutive samples
    cum_added, cum_removed = 0, 0
    sample_cum = [(0, 0)]  # first point: no cumulative changes
    sys.stderr.write(f"  Computing cumulative line changes between {len(indices)} sample points...\n")
    for i in range(1, len(samples)):
        progress(i, len(samples) - 1, f"diff {i}/{len(samples) - 1}")
        _, ins, dels = get_diff_stats_between(samples[i - 1][0].hash, samples[i][0].hash)
        cum_added += ins
        cum_removed += dels
        sample_cum.append((cum_added, cum_removed))
    if len(samples) > 1:
        progress(len(samples) - 1, len(samples) - 1, "done")

    # Print table
    print(f"  {'Date':>16}  {'Commit#':>7}  {'Lines':>8}  {'Files':>6}  {'Cum +':>8}  {'Cum -':>8}  Chart")
    print(f"  {'-' * 16}  {'-' * 7}  {'-' * 8}  {'-' * 6}  {'-' * 8}  {'-' * 8}  {'-' * 32}")

    max_loc = max(s[1] for s in samples) if samples else 1
    if max_loc == 0:
        max_loc = 1
    for i, ((c, loc, nfiles), idx) in enumerate(zip(samples, indices)):
        ca, cr = sample_cum[i]
        bar = ascii_bar(loc, max_loc, 32)
        print(f"  {c.date.strftime('%m-%d %H:%M'):>16}  {idx + 1:>7}  {loc:>8,}  {nfiles:>6}  {'+' + str(ca):>8}  {'-' + str(cr):>8}  |{bar}")

    # Growth summary
    if len(samples) >= 2:
        first_loc = samples[0][1] if samples[0][1] > 0 else 1
        last_loc = samples[-1][1]
        first_files = samples[0][2] if samples[0][2] > 0 else 1
        last_files = samples[-1][2]
        total_added = sample_cum[-1][0]
        total_removed = sample_cum[-1][1]
        print()
        print(f"  Final codebase:    {last_loc:,} lines across {last_files} files")
        if last_loc > 0:
            print(f"  Growth factor:     {last_loc / first_loc:.1f}x lines, {last_files / first_files:.1f}x files")
        print(f"  Total churn:       +{total_added:,} / -{total_removed:,} lines ({total_added + total_removed:,} total)")
        if last_loc > 0 and total_added > 0:
            print(f"  Churn ratio:       {(total_added + total_removed) / last_loc:.1f}x (total changes / final size)")
        if span.total_seconds() > 0:
            loc_per_hour = (last_loc - samples[0][1]) / (span.total_seconds() / 3600)
            commits_per_hour = total / (span.total_seconds() / 3600)
            print(f"  Avg velocity:      {loc_per_hour:,.0f} net lines/hour, {commits_per_hour:.1f} commits/hour")

    # ASCII growth chart
    if len(samples) >= 3:
        print()
        print("  LINES OF CODE GROWTH (ASCII chart):")
        chart_height = 15
        chart_width = min(len(samples), 70)
        # Resample to chart_width points
        step = max(1, len(samples) // chart_width)
        chart_samples = samples[::step]
        if chart_samples[-1] != samples[-1]:
            chart_samples.append(samples[-1])
        locs = [s[1] for s in chart_samples]
        max_chart_loc = max(locs) if locs else 1
        if max_chart_loc == 0:
            max_chart_loc = 1

        for row in range(chart_height, 0, -1):
            threshold = max_chart_loc * row / chart_height
            line = "  "
            if row == chart_height:
                line += f"{max_chart_loc:>7,} |"
            elif row == chart_height // 2:
                line += f"{max_chart_loc // 2:>7,} |"
            elif row == 1:
                line += f"{'0':>7} |"
            else:
                line += "        |"
            for loc in locs:
                if loc >= threshold:
                    line += "#"
                else:
                    line += " "
            print(line)
        # X-axis
        print("        +" + "-" * len(locs))
        if len(chart_samples) >= 2:
            first_date = chart_samples[0][0].date.strftime("%m-%d")
            last_date = chart_samples[-1][0].date.strftime("%m-%d")
            padding = len(locs) - len(first_date) - len(last_date)
            print(f"         {first_date}{' ' * max(1, padding)}{last_date}")


def analyze_feature_evolution(commits: List[Commit]):
    """Section 2: Feature Evolution Phases."""
    print(section_header("2. FEATURE EVOLUTION PHASES"))

    area_commits: Dict[str, List[Commit]] = defaultdict(list)
    for c in commits:
        areas = classify_commit(c.subject)
        for a in areas:
            area_commits[a].append(c)

    # Sort by first commit date
    sorted_areas = sorted(area_commits.items(), key=lambda x: x[1][0].date)

    max_count = max(len(v) for v in area_commits.values()) if area_commits else 1

    print(f"  {'Feature Area':<28} {'Commits':>7}  {'First Active':>16}  {'Last Active':>16}  Bar")
    print(f"  {'-' * 28} {'-' * 7}  {'-' * 16}  {'-' * 16}  {'-' * 30}")

    for area, clist in sorted_areas:
        first = clist[0].date.strftime("%m-%d %H:%M")
        last = clist[-1].date.strftime("%m-%d %H:%M")
        bar = ascii_bar(len(clist), max_count, 30)
        print(f"  {area:<28} {len(clist):>7}  {first:>16}  {last:>16}  |{bar}")

    # Identify phases by grouping into time windows
    print()
    print("  DEVELOPMENT PHASES (by dominant feature area per day):")
    print(f"  {'-' * 70}")

    day_areas: Dict[str, Counter] = defaultdict(Counter)
    for c in commits:
        day = c.date.strftime("%Y-%m-%d")
        areas = classify_commit(c.subject)
        for a in areas:
            day_areas[day][a] += 1

    for day in sorted(day_areas.keys()):
        top = day_areas[day].most_common(4)
        top_str = ", ".join(f"{a}({n})" for a, n in top)
        total_day = sum(day_areas[day].values())
        print(f"  {day}  [{total_day:>4} commits]  {top_str}")


def analyze_task_locks(commits: List[Commit]):
    """Section 3: Task Lock Analysis."""
    print(section_header("3. TASK LOCK ANALYSIS"))

    lock_pattern = re.compile(
        r"^(?:Lock\s*(?:task)?:?\s*)(.*)", re.IGNORECASE
    )
    unlock_pattern = re.compile(
        r"^(?:(?:Unlock|Remove)\s*(?:task)?\s*(?:lock)?:?\s*)(.*)", re.IGNORECASE
    )

    locks = []   # (date, task_name)
    unlocks = [] # (date, task_name)
    all_task_events = []  # (date, "lock"/"unlock", task_name)

    for c in commits:
        subj = c.subject.strip()
        # Check unlock first (more specific patterns)
        m = unlock_pattern.match(subj)
        if m and re.match(r"^(Unlock|Remove)", subj, re.IGNORECASE):
            name = m.group(1).strip().rstrip(".")
            # Clean up trailing status notes
            name = re.sub(r"\s*\((?:completed|done|fixed|no changes needed|complete)\)\s*$", "", name, flags=re.IGNORECASE)
            unlocks.append((c.date, name))
            all_task_events.append((c.date, "unlock", name, c.hash))
            continue

        m = lock_pattern.match(subj)
        if m and re.match(r"^Lock", subj, re.IGNORECASE):
            name = m.group(1).strip().rstrip(".")
            locks.append((c.date, name))
            all_task_events.append((c.date, "lock", name, c.hash))

    print(f"  Total lock events:   {len(locks)}")
    print(f"  Total unlock events: {len(unlocks)}")
    print()

    # Match locks to unlocks to estimate duration
    # Build a simple mapping: for each lock, find the nearest subsequent unlock
    # that has a similar task name (fuzzy match)
    def normalize_task_name(name: str) -> str:
        """Normalize task name for fuzzy matching."""
        name = name.lower().strip()
        name = re.sub(r"[^a-z0-9\s]", " ", name)
        name = re.sub(r"\s+", " ", name).strip()
        return name

    lock_entries = [(d, normalize_task_name(n), n) for d, n in locks]
    unlock_entries = [(d, normalize_task_name(n), n) for d, n in unlocks]

    matched_durations = []
    unmatched_locks = []

    used_unlocks = set()
    for lock_date, lock_norm, lock_name in lock_entries:
        best_match = None
        best_score = 0
        best_idx = -1
        for ui, (unlock_date, unlock_norm, unlock_name) in enumerate(unlock_entries):
            if ui in used_unlocks:
                continue
            if unlock_date < lock_date:
                continue
            # Simple word overlap score
            lock_words = set(lock_norm.split())
            unlock_words = set(unlock_norm.split())
            if not lock_words or not unlock_words:
                continue
            overlap = len(lock_words & unlock_words)
            score = overlap / max(len(lock_words), len(unlock_words))
            if score > best_score:
                best_score = score
                best_match = (unlock_date, unlock_name)
                best_idx = ui
        if best_match and best_score >= 0.3:
            used_unlocks.add(best_idx)
            duration = (best_match[0] - lock_date).total_seconds()
            matched_durations.append((lock_name, duration, lock_date, best_match[0]))
        else:
            unmatched_locks.append((lock_name, lock_date))

    if matched_durations:
        durations_sec = [d for _, d, _, _ in matched_durations]
        avg_dur = sum(durations_sec) / len(durations_sec)
        med_dur = sorted(durations_sec)[len(durations_sec) // 2]
        min_dur = min(durations_sec)
        max_dur = max(durations_sec)
        print(f"  Matched lock/unlock pairs: {len(matched_durations)}")
        print(f"  Average task duration:     {fmt_duration(avg_dur)}")
        print(f"  Median task duration:      {fmt_duration(med_dur)}")
        print(f"  Shortest task:             {fmt_duration(min_dur)}")
        print(f"  Longest task:              {fmt_duration(max_dur)}")
        print()

        # Longest tasks
        longest = sorted(matched_durations, key=lambda x: -x[1])[:10]
        print("  LONGEST TASKS:")
        print(f"  {'Duration':>10}  {'Locked':>16}  {'Unlocked':>16}  Task")
        print(f"  {'-' * 10}  {'-' * 16}  {'-' * 16}  {'-' * 40}")
        for name, dur, ld, ud in longest:
            print(f"  {fmt_duration(dur):>10}  {ld.strftime('%m-%d %H:%M'):>16}  {ud.strftime('%m-%d %H:%M'):>16}  {truncate(name, 50)}")

        # Shortest tasks
        print()
        shortest = sorted(matched_durations, key=lambda x: x[1])[:10]
        print("  SHORTEST TASKS:")
        print(f"  {'Duration':>10}  Task")
        print(f"  {'-' * 10}  {'-' * 50}")
        for name, dur, ld, ud in shortest:
            print(f"  {fmt_duration(dur):>10}  {truncate(name, 60)}")

    print()
    print(f"  Unmatched locks (still open or no matching unlock): {len(unmatched_locks)}")
    if unmatched_locks:
        for name, dt in unmatched_locks[:15]:
            print(f"    {dt.strftime('%m-%d %H:%M')}  {truncate(name, 60)}")
        if len(unmatched_locks) > 15:
            print(f"    ... and {len(unmatched_locks) - 15} more")

    # Concurrent task analysis
    print()
    print("  CONCURRENT TASK ACTIVITY:")
    # For each lock, check how many other tasks are active at that moment
    # A task is "active" between its lock and unlock (or end of project)
    active_intervals = []
    for name, dur, lock_dt, unlock_dt in matched_durations:
        active_intervals.append((lock_dt, unlock_dt))
    # Also add unmatched locks (assume active until end)
    end_date = commits[-1].date if commits else datetime.now()
    for name, dt in unmatched_locks:
        active_intervals.append((dt, end_date))

    if active_intervals:
        # Sample time points and count overlaps
        all_lock_times = sorted(set(
            [iv[0] for iv in active_intervals] + [iv[1] for iv in active_intervals]
        ))
        max_concurrent = 0
        max_concurrent_time = None
        concurrency_samples = []
        for t in all_lock_times:
            count = sum(1 for start, end in active_intervals if start <= t < end)
            concurrency_samples.append((t, count))
            if count > max_concurrent:
                max_concurrent = count
                max_concurrent_time = t

        print(f"  Peak concurrent tasks:     {max_concurrent}", end="")
        if max_concurrent_time:
            print(f"  (at {max_concurrent_time.strftime('%m-%d %H:%M')})")
        else:
            print()

        avg_concurrent = sum(c for _, c in concurrency_samples) / len(concurrency_samples) if concurrency_samples else 0
        print(f"  Average concurrent tasks:  {avg_concurrent:.1f}")

    # Most frequently locked task areas
    print()
    print("  MOST COMMON TASK KEYWORDS:")
    all_lock_words = Counter()
    for _, name in locks:
        words = re.findall(r"[a-zA-Z][a-zA-Z0-9_]+", name.lower())
        for w in words:
            if len(w) > 3 and w not in {"task", "lock", "fix", "the", "for", "and", "from", "with", "that", "this"}:
                all_lock_words[w] += 1
    for word, count in all_lock_words.most_common(20):
        bar = ascii_bar(count, all_lock_words.most_common(1)[0][1] if all_lock_words else 1, 20)
        print(f"    {word:<25} {count:>4}  |{bar}")


def analyze_commit_velocity(commits: List[Commit]):
    """Section 4: Commit Velocity & Patterns."""
    print(section_header("4. COMMIT VELOCITY & PATTERNS"))

    if len(commits) < 2:
        print("  Not enough commits for velocity analysis.")
        return

    total_seconds = (commits[-1].date - commits[0].date).total_seconds()
    total_hours = total_seconds / 3600 if total_seconds > 0 else 1

    print(f"  Overall rate: {len(commits) / total_hours:.1f} commits/hour ({len(commits) / (total_hours / 24):.0f} commits/day)")
    print()

    # Time gaps between consecutive commits (by commit date, sorted)
    # Note: parallel agents mean git log order != chronological order
    sorted_dates = sorted(c.date for c in commits)
    gaps = []
    for i in range(1, len(sorted_dates)):
        gap = (sorted_dates[i] - sorted_dates[i - 1]).total_seconds()
        gaps.append(gap)

    # Also count how many times commits appear out-of-order in git log
    out_of_order = sum(
        1 for i in range(1, len(commits))
        if commits[i].date < commits[i - 1].date
    )

    if gaps:
        avg_gap = sum(gaps) / len(gaps)
        med_gap = sorted(gaps)[len(gaps) // 2]
        min_gap = min(gaps)
        max_gap = max(gaps)
        print(f"  Average gap between commits:  {fmt_duration(avg_gap)}")
        print(f"  Median gap:                   {fmt_duration(med_gap)}")
        print(f"  Shortest gap:                 {fmt_duration(min_gap)}")
        print(f"  Longest gap:                  {fmt_duration(max_gap)}")
        if out_of_order > 0:
            print(f"  Out-of-order commits:         {out_of_order} ({100 * out_of_order / len(commits):.1f}% -- parallel agents)")
        print()

        # Gaps distribution
        brackets = [
            (0, 10, "<10s"),
            (10, 30, "10-30s"),
            (30, 60, "30s-1m"),
            (60, 300, "1-5m"),
            (300, 600, "5-10m"),
            (600, 1800, "10-30m"),
            (1800, 3600, "30m-1h"),
            (3600, 7200, "1-2h"),
            (7200, float("inf"), ">2h"),
        ]
        print("  GAP DISTRIBUTION (between chronologically sorted commits):")
        gap_max_count = 0
        bracket_counts = []
        for lo, hi, label in brackets:
            count = sum(1 for g in gaps if lo <= g < hi)
            bracket_counts.append((label, count))
            gap_max_count = max(gap_max_count, count)

        for label, count in bracket_counts:
            bar = ascii_bar(count, gap_max_count, 30)
            pct = 100 * count / len(gaps) if gaps else 0
            print(f"    {label:>8}  {count:>5} ({pct:5.1f}%)  |{bar}")

    # Commits per hour of day
    print()
    print("  COMMITS BY HOUR OF DAY (UTC):")
    hour_counts = Counter()
    for c in commits:
        hour_counts[c.date.hour] += 1

    max_hour_count = max(hour_counts.values()) if hour_counts else 1
    for h in range(24):
        count = hour_counts.get(h, 0)
        bar = ascii_bar(count, max_hour_count, 40)
        print(f"    {h:02d}:00  {count:>5}  |{bar}")

    # Commits per day
    print()
    print("  COMMITS BY DAY:")
    day_counts = Counter()
    for c in commits:
        day_counts[c.date.strftime("%Y-%m-%d")] += 1

    max_day_count = max(day_counts.values()) if day_counts else 1
    for day in sorted(day_counts.keys()):
        count = day_counts[day]
        bar = ascii_bar(count, max_day_count, 50)
        print(f"    {day}  {count:>5}  |{bar}")

    # Fastest commit streaks
    print()
    print("  FASTEST COMMIT STREAKS (most commits in 10-minute window):")
    window_sec = 600  # 10 minutes
    best_streaks = []
    for i in range(len(commits)):
        j = i
        while j < len(commits) and (commits[j].date - commits[i].date).total_seconds() <= window_sec:
            j += 1
        count = j - i
        if count >= 5:
            best_streaks.append((count, commits[i].date, commits[j - 1].date))

    # Deduplicate overlapping streaks - keep best per 10min window
    best_streaks.sort(key=lambda x: -x[0])
    shown_windows = set()
    displayed = 0
    for count, start, end in best_streaks:
        window_key = start.strftime("%m-%d %H:%M")[:14]  # ~10 min bucket
        if window_key in shown_windows:
            continue
        shown_windows.add(window_key)
        print(f"    {count:>3} commits  {start.strftime('%m-%d %H:%M')} - {end.strftime('%H:%M')}")
        displayed += 1
        if displayed >= 15:
            break


def analyze_lines_changed(commits: List[Commit], sample_step: int):
    """Section 5: Lines Changed Per Commit."""
    print(section_header("5. LINES CHANGED PER COMMIT"))

    # Sample commits for line-change analysis
    indices = list(range(0, len(commits), max(1, sample_step // 2)))
    if not indices:
        print("  No commits to analyze.")
        return

    sys.stderr.write(f"  Analyzing {len(indices)} commits for change sizes...\n")
    commit_sizes = []  # (commit, added, removed)

    for i, idx in enumerate(indices):
        if i % 10 == 0:
            progress(i, len(indices), f"commit {idx}")
        c = commits[idx]
        added, removed, files = get_commit_numstat(c.hash)
        commit_sizes.append((c, added, removed, files))
    progress(len(indices), len(indices), "done")

    if not commit_sizes:
        return

    total_changes = [(c, a + r, a, r, f) for c, a, r, f in commit_sizes if a + r > 0]
    total_changes.sort(key=lambda x: -x[1])

    # Distribution
    all_sizes = [a + r for _, a, r, _ in commit_sizes]
    if all_sizes:
        avg_size = sum(all_sizes) / len(all_sizes)
        med_size = sorted(all_sizes)[len(all_sizes) // 2]
        max_size = max(all_sizes)
        print(f"  Average lines changed per commit:  {avg_size:,.0f}")
        print(f"  Median:                            {med_size:,.0f}")
        print(f"  Maximum:                           {max_size:,.0f}")
        print()

        # Size distribution
        brackets = [
            (0, 5, "0-4"),
            (5, 20, "5-19"),
            (20, 50, "20-49"),
            (50, 100, "50-99"),
            (100, 250, "100-249"),
            (250, 500, "250-499"),
            (500, 1000, "500-999"),
            (1000, 5000, "1K-5K"),
            (5000, float("inf"), "5K+"),
        ]
        print("  SIZE DISTRIBUTION (lines added + removed):")
        bracket_counts = []
        max_bc = 0
        for lo, hi, label in brackets:
            count = sum(1 for s in all_sizes if lo <= s < hi)
            bracket_counts.append((label, count))
            max_bc = max(max_bc, count)

        for label, count in bracket_counts:
            bar = ascii_bar(count, max_bc, 30)
            pct = 100 * count / len(all_sizes) if all_sizes else 0
            print(f"    {label:>8}  {count:>5} ({pct:5.1f}%)  |{bar}")

    # Biggest commits
    print()
    print("  BIGGEST COMMITS (most lines changed):")
    print(f"  {'Lines':>8}  {'+':>6}  {'-':>6}  {'Date':>12}  Subject")
    print(f"  {'-' * 8}  {'-' * 6}  {'-' * 6}  {'-' * 12}  {'-' * 40}")
    for c, total, added, removed, files in total_changes[:15]:
        print(f"  {total:>8,}  {'+' + str(added):>6}  {'-' + str(removed):>6}  {c.date.strftime('%m-%d %H:%M'):>12}  {truncate(c.subject, 50)}")

    # Smallest non-zero commits
    print()
    print("  SMALLEST COMMITS (fewest lines changed, non-zero):")
    smallest = sorted(total_changes, key=lambda x: x[1])[:10]
    for c, total, added, removed, files in smallest:
        print(f"  {total:>8,}  {'+' + str(added):>6}  {'-' + str(removed):>6}  {truncate(c.subject, 55)}")


def analyze_file_hotspots(commits: List[Commit], sample_step: int):
    """Section 6: File Hotspots."""
    print(section_header("6. FILE HOTSPOTS"))

    # Sample commits for file modification analysis
    indices = list(range(0, len(commits), max(1, sample_step // 3)))
    sys.stderr.write(f"  Analyzing {len(indices)} commits for file hotspots...\n")

    file_freq = Counter()
    dir_freq = Counter()

    for i, idx in enumerate(indices):
        if i % 20 == 0:
            progress(i, len(indices), f"commit {idx}")
        c = commits[idx]
        _, _, files = get_commit_numstat(c.hash)
        for f in files:
            file_freq[f] += 1
            parts = f.split("/")
            for depth in range(1, min(4, len(parts))):
                dir_freq["/".join(parts[:depth]) + "/"] += 1
    progress(len(indices), len(indices), "done")

    # Most modified files
    print("  MOST FREQUENTLY MODIFIED FILES:")
    print(f"  {'Modifications':>13}  File")
    print(f"  {'-' * 13}  {'-' * 60}")
    max_file_count = file_freq.most_common(1)[0][1] if file_freq else 1
    for fname, count in file_freq.most_common(25):
        bar = ascii_bar(count, max_file_count, 20)
        print(f"  {count:>13}  {truncate(fname, 55)}  |{bar}")

    # Most active directories
    print()
    print("  MOST ACTIVE DIRECTORIES:")
    print(f"  {'Modifications':>13}  Directory")
    print(f"  {'-' * 13}  {'-' * 50}")
    max_dir_count = dir_freq.most_common(1)[0][1] if dir_freq else 1
    for dname, count in dir_freq.most_common(20):
        bar = ascii_bar(count, max_dir_count, 20)
        print(f"  {count:>13}  {dname:<50}  |{bar}")

    # File type breakdown
    print()
    print("  FILE TYPE BREAKDOWN (by modification count):")
    ext_freq = Counter()
    for fname, count in file_freq.items():
        ext = fname.rsplit(".", 1)[-1] if "." in fname else "(no ext)"
        ext_freq[ext] += count
    max_ext = ext_freq.most_common(1)[0][1] if ext_freq else 1
    for ext, count in ext_freq.most_common(15):
        bar = ascii_bar(count, max_ext, 25)
        print(f"    .{ext:<10}  {count:>6}  |{bar}")


def analyze_parallel_agents(commits: List[Commit]):
    """Section 7: Parallel Agent Activity."""
    print(section_header("7. PARALLEL AGENT ACTIVITY"))

    if len(commits) < 2:
        print("  Not enough commits for parallel analysis.")
        return

    # Detect parallel work: commits within same minute from different task areas
    # Group commits by minute
    minute_buckets: Dict[str, List[Commit]] = defaultdict(list)
    for c in commits:
        key = c.date.strftime("%Y-%m-%d %H:%M")
        minute_buckets[key].append(c)

    # Count minutes with multiple commits
    multi_commit_minutes = {k: v for k, v in minute_buckets.items() if len(v) > 1}
    print(f"  Minutes with >1 commit:  {len(multi_commit_minutes)} / {len(minute_buckets)} total minutes")
    print()

    # For each multi-commit minute, check if they touch different feature areas
    parallel_evidence = []
    for minute_key, clist in sorted(multi_commit_minutes.items()):
        areas_in_minute = set()
        for c in clist:
            areas = classify_commit(c.subject)
            areas_in_minute.update(areas)
        if len(areas_in_minute) > 1:
            parallel_evidence.append((minute_key, len(clist), areas_in_minute, clist))

    print(f"  Minutes with parallel work (different feature areas): {len(parallel_evidence)}")
    print()

    # Estimate concurrent agents using a sliding window approach
    # Look at 2-minute windows and count distinct task subjects
    window_sec = 120  # 2-minute windows
    max_agents = 0
    max_agents_time = None
    agent_estimates = []

    i = 0
    while i < len(commits):
        j = i
        subjects = set()
        while j < len(commits) and (commits[j].date - commits[i].date).total_seconds() <= window_sec:
            # Each unique commit subject = likely different agent
            subjects.add(commits[j].subject)
            j += 1
        count = len(subjects)
        agent_estimates.append((commits[i].date, count))
        if count > max_agents:
            max_agents = count
            max_agents_time = commits[i].date
        i = max(i + 1, i + (j - i) // 2)  # Advance by half window

    print(f"  Estimated peak concurrent agents:  {max_agents}", end="")
    if max_agents_time:
        print(f"  (at {max_agents_time.strftime('%m-%d %H:%M')})")
    else:
        print()

    if agent_estimates:
        avg_agents = sum(c for _, c in agent_estimates) / len(agent_estimates)
        print(f"  Estimated average concurrent agents: {avg_agents:.1f}")

    # Show concurrent agent estimates by hour
    print()
    print("  ESTIMATED CONCURRENT AGENTS BY HOUR:")

    hour_max_agents: Dict[str, int] = defaultdict(int)
    hour_all: Dict[str, List[int]] = defaultdict(list)
    for dt, count in agent_estimates:
        hour_key = dt.strftime("%m-%d %H:00")
        hour_max_agents[hour_key] = max(hour_max_agents[hour_key], count)
        hour_all[hour_key].append(count)

    # Show top hours
    sorted_hours = sorted(hour_max_agents.items(), key=lambda x: -x[1])[:25]
    sorted_hours.sort(key=lambda x: x[0])  # Re-sort chronologically

    overall_max = max(hour_max_agents.values()) if hour_max_agents else 1
    print(f"  {'Hour':>14}  {'Peak':>5}  {'Avg':>5}  Activity")
    print(f"  {'-' * 14}  {'-' * 5}  {'-' * 5}  {'-' * 40}")
    for hour_key, peak in sorted_hours:
        avg = sum(hour_all[hour_key]) / len(hour_all[hour_key]) if hour_all[hour_key] else 0
        bar = ascii_bar(peak, overall_max, 40)
        print(f"  {hour_key:>14}  {peak:>5}  {avg:>5.1f}  |{bar}")

    # Show busiest minutes
    print()
    print("  BUSIEST MINUTES (most commits in a single minute):")
    busiest = sorted(multi_commit_minutes.items(), key=lambda x: -len(x[1]))[:15]
    for minute_key, clist in busiest:
        areas = set()
        for c in clist:
            areas.update(classify_commit(c.subject))
        areas_str = ", ".join(sorted(areas)[:4])
        print(f"    {minute_key}  {len(clist):>3} commits  areas: {truncate(areas_str, 40)}")

    # Show some examples of clear parallel work
    print()
    print("  EXAMPLES OF PARALLEL AGENT WORK (different tasks, same minute):")
    shown = 0
    for minute_key, count, areas, clist in parallel_evidence[:8]:
        print(f"    {minute_key} ({count} commits):")
        for c in clist[:5]:
            print(f"      - {truncate(c.subject, 65)}")
        if len(clist) > 5:
            print(f"      ... and {len(clist) - 5} more")
        shown += 1


# ---------------------------------------------------------------------------
# Summary statistics
# ---------------------------------------------------------------------------

def print_summary(commits: List[Commit]):
    """Print executive summary."""
    print(section_header("EXECUTIVE SUMMARY"))

    total = len(commits)
    span = commits[-1].date - commits[0].date
    hours = span.total_seconds() / 3600

    print(f"  Project:           Claude's C Compiler (Rust)")
    print(f"  Commits:           {total:,}")
    print(f"  Time span:         {span.days} days, {span.seconds // 3600} hours")
    print(f"  Rate:              {total / hours:.1f} commits/hour")
    print(f"  Avg gap:           {hours * 3600 / total:.0f} seconds between commits")
    print()

    # Lock/unlock stats
    lock_count = sum(1 for c in commits if re.match(r"^Lock", c.subject, re.IGNORECASE))
    unlock_count = sum(1 for c in commits if re.match(r"^(Unlock|Remove\s+task\s+lock)", c.subject, re.IGNORECASE))
    work_count = total - lock_count - unlock_count
    print(f"  Lock commits:      {lock_count:,} ({100 * lock_count / total:.1f}%)")
    print(f"  Unlock commits:    {unlock_count:,} ({100 * unlock_count / total:.1f}%)")
    print(f"  Work commits:      {work_count:,} ({100 * work_count / total:.1f}%)")
    print()

    # Feature area summary
    area_counts = Counter()
    for c in commits:
        areas = classify_commit(c.subject)
        for a in areas:
            area_counts[a] += 1
    print("  Top feature areas:")
    for area, count in area_counts.most_common(10):
        pct = 100 * count / total
        print(f"    {area:<28} {count:>5} commits ({pct:5.1f}%)")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Git Archaeology: Analyze the Claude's C Compiler git history"
    )
    parser.add_argument(
        "--full", action="store_true",
        help="Examine every commit (slower, more detailed)"
    )
    parser.add_argument(
        "--fast", action="store_true", default=True,
        help="Sample commits for faster analysis (default)"
    )
    args = parser.parse_args()

    if args.full:
        sample_step = 1
    else:
        sample_step = 50

    banner = r"""
    ===========================================================================
       _____ _ _        _                _                    _
      / ____(_) |      / \   _ __ ___ | |__   __ _  ___  ___ | | ___   __ _ _   _
     | |  __ _| |_    / _ \ | '__/ __|| '_ \ / _` |/ _ \/ _ \| |/ _ \ / _` | | | |
     | | |_ | | __|  / ___ \| | | (__ | | | | (_| |  __/ (_) | | (_) | (_| | |_| |
      \_____|_|\__| /_/   \_\_|  \___||_| |_|\__,_|\___|\___/|_|\___/ \__, |\__, |
                                                                       |___/ |___/
       Claude's C Compiler -- A deep dive into 3982 commits by 16 AI agents
    ===========================================================================
    """
    print(banner)

    mode = "FULL (every commit)" if args.full else f"FAST (sampling every ~{sample_step} commits)"
    print(f"  Analysis mode: {mode}")
    print()

    # Load all commits
    sys.stderr.write("Loading git history...\n")
    commits = get_all_commits()
    if not commits:
        print("ERROR: No commits found. Are you in a git repository?")
        sys.exit(1)

    sys.stderr.write(f"  Found {len(commits)} commits.\n\n")

    # Run analyses
    print_summary(commits)
    analyze_timeline_growth(commits, sample_step)
    analyze_feature_evolution(commits)
    analyze_task_locks(commits)
    analyze_commit_velocity(commits)
    analyze_lines_changed(commits, sample_step)
    analyze_file_hotspots(commits, sample_step)
    analyze_parallel_agents(commits)

    print(section_header("END OF REPORT"))
    print("  Report generated by git_archaeology.py")
    print(f"  Analyzed {len(commits):,} commits in {'full' if args.full else 'fast'} mode")
    print()


if __name__ == "__main__":
    main()
