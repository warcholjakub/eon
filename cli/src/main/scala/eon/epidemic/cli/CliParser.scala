package eon.epidemic.cli

import scopt.OParser

object CliParser:
  def parse(args: Array[String]): Either[String, CliSettings] =
    val builder = OParser.builder[CliOverrides]
    import builder.*

    val parser =
      OParser.sequence(
        programName("epidemic-sim"),
        head("epidemic-sim", "0.1.0"),
        opt[String]("config-file").action((value, state) => state.copy(configFile = Some(value))),
        opt[String]("preset").action((value, state) => state.copy(preset = Some(value))),
        opt[String]("graph-source").action((value, state) => state.copy(graphSource = Some(value))),
        opt[String]("graph-shape").action((value, state) => state.copy(graphShape = Some(value))),
        opt[String]("graph-file").action((value, state) => state.copy(graphFile = Some(value))),
        opt[Int]("explicit-node-count").action((value, state) => state.copy(explicitNodeCount = Some(value))),
        opt[Int]("node-count").action((value, state) => state.copy(nodeCount = Some(value))),
        opt[Double]("edge-probability").action((value, state) => state.copy(edgeProbability = Some(value))),
        opt[Int]("ring-degree").action((value, state) => state.copy(ringDegree = Some(value))),
        opt[Int]("activation-on").action((value, state) => state.copy(activationOnTicks = Some(value))),
        opt[Int]("activation-off").action((value, state) => state.copy(activationOffTicks = Some(value))),
        opt[Int]("activation-phase").action((value, state) => state.copy(activationPhase = Some(value))),
        opt[Double]("infection-probability").action((value, state) => state.copy(infectionProbability = Some(value))),
        opt[Double]("recovery-probability").action((value, state) => state.copy(recoveryProbability = Some(value))),
        opt[Int]("max-ticks").action((value, state) => state.copy(maxTicks = Some(value))),
        opt[Boolean]("stop-when-no-infected").action((value, state) => state.copy(stopWhenNoInfected = Some(value))),
        opt[String]("initial-infected").action((value, state) => state.copy(initialInfected = Some(value))),
        opt[Int]("initial-infected-count").action((value, state) => state.copy(initialInfectedCount = Some(value))),
        opt[Long]("seed").action((value, state) => state.copy(seed = Some(value))),
        opt[Int]("runs").action((value, state) => state.copy(runs = Some(value))),
        opt[String]("output-dir").action((value, state) => state.copy(outputDir = Some(value))),
        opt[Boolean]("visualization").action((value, state) =>
          state.copy(visualizationEnabled = Some(value))
        )
      )

    OParser.parse(parser, args, CliOverrides()) match
      case None => Left("failed to parse CLI arguments; run with --help to see available options")
      case Some(overrides) =>
        val baseSettings =
          overrides.configFile match
            case Some(path) => ConfigFileLoader.load(path)
            case None       => Right(CliSettings.default)

        baseSettings.flatMap(base => ConfigFileLoader.applyOverrides(base, overrides))
