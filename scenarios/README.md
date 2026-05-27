Scenarios for EON epidemic simulation.

Run any scenario with:

`sbt --client "cli/run --config-file scenarios/<file>.conf"`

Notes:
- Scenario 2 (grid) and 3 (scale-free) use `graph.source = file`.
- Scenario 6 is represented by three files: initial infected count 1, 10, and 20.
- Scenario 7 models three dense clusters connected only through one shared hub node.
- Scenario 7 sweep evaluates infection and recovery probabilities from 0.05 to 1.00 in steps of 0.05, with 1000 runs per pair.
