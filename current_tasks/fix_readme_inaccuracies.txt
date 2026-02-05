Fix inaccuracies in README documentation.

The frontend README omits `is_noreturn` from the FunctionInfo description in
the SemaResult table. The actual struct has 5 fields (return_type, params,
variadic, is_defined, is_noreturn) but the README only lists 4.

Started: 2026-02-05
