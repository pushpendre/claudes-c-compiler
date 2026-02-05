Review passes/README.md for accuracy.

Fix minor inaccuracy in DCE pass description: the README says "non-pure Intrinsics"
but the code also treats intrinsics with dest_ptr as side-effecting even when pure.

Started: 2026-02-05
