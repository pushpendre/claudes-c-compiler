#!/usr/bin/env python3
"""Rebuild compiler_movie.html by re-embedding the latest git_timeline_data.json."""

import json
import os

DATA_FILE = os.path.join(os.path.dirname(__file__), "git_timeline_data.json")
HTML_FILE = os.path.join(os.path.dirname(__file__), "compiler_movie.html")

with open(DATA_FILE) as f:
    data = json.load(f)

with open(HTML_FILE) as f:
    html = f.read()

old_start = html.index("const DATA = ") + len("const DATA = ")
old_end = html.index(";\n</script>", old_start)
new_html = html[:old_start] + json.dumps(data) + html[old_end:]

with open(HTML_FILE, "w") as f:
    f.write(new_html)

print(f"Updated {HTML_FILE}: {len(new_html):,} bytes")
print(f"  Commits: {len(data['commits'])}")
print(f"  Task locks: {len(data['task_locks'])}")
print(f"  Snapshots: {len(data['growth_snapshots'])}")
