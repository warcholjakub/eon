Scenarios for EON epidemic simulation.

Run any scenario with:

`sbt --client "cli/run --config-file scenarios/<file>.conf"`

Notes:
- Scenario 2 (grid) and 3 (scale-free) use `graph.source = file`.
- Scenario 6 is represented by three files: initial infected count 1, 10, and 20.
- Scenario 7 models three dense clusters connected only through one shared hub node; `graph.activation` controls every edge in this generated graph.
- Scenario 7 sweep evaluates infection and recovery probabilities from 0.05 to 1.00 in steps of 0.05 and edge active fractions of 100%, 75%, 50%, and 25%, with 1000 runs per parameter combination.
- Scenario 7 sweep incrementally writes one raw run per row to `sweep_runs.csv` rather than aggregating parameter combinations.
