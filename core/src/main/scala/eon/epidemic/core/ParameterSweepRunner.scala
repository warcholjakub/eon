package eon.epidemic.core

object ParameterSweepRunner:
  def run(
      config: SimulationConfig,
      minProbability: Double,
      maxProbability: Double,
      probabilityStep: Double,
      runsPerPair: Int,
      edgeActivations: Vector[EdgeActivation] = Vector.empty,
      persistProgress: ParameterSweepResult => Either[String, Unit] = _ => Right(())
  ): Either[String, ParameterSweepResult] =
    probabilityValues(minProbability, maxProbability, probabilityStep).flatMap: probabilities =>
      val activations =
        if edgeActivations.isEmpty then Vector(configuredActivation(config.graphSpec))
        else edgeActivations
      val combinations =
        for
          infectionProbability <- probabilities
          recoveryProbability <- probabilities
          edgeActivation <- activations
        yield (infectionProbability, recoveryProbability, edgeActivation)

      combinations.foldLeft[Either[String, Vector[ParameterSweepRow]]](Right(Vector.empty)):
        case (rowsEither, (infectionProbability, recoveryProbability, edgeActivation)) =>
          rowsEither.flatMap: rows =>
            val pairConfig =
              config.copy(
                infectionProbability = infectionProbability,
                recoveryProbability = recoveryProbability,
                graphSpec = graphSpecWithActivation(config.graphSpec, edgeActivation)
              )
            BatchRunner.runSummaries(pairConfig, runsPerPair).flatMap: summaries =>
              val updatedRows = rows :+ ParameterSweepRow(infectionProbability, recoveryProbability, edgeActivation, summaries)
              val progress = ParameterSweepResult(runsPerPair, updatedRows, config.trackedNodes)
              persistProgress(progress).map(_ => updatedRows)
      .map(rows => ParameterSweepResult(runsPerPair, rows, config.trackedNodes))

  def runStreaming(
      config: SimulationConfig,
      minProbability: Double,
      maxProbability: Double,
      probabilityStep: Double,
      runsPerCombination: Int,
      edgeActivations: Vector[EdgeActivation] = Vector.empty,
      persistRow: (Int, ParameterSweepRow) => Either[String, Unit]
  ): Either[String, Int] =
    probabilityValues(minProbability, maxProbability, probabilityStep).flatMap: probabilities =>
      val activations =
        if edgeActivations.isEmpty then Vector(configuredActivation(config.graphSpec))
        else edgeActivations
      val combinations =
        for
          infectionProbability <- probabilities
          recoveryProbability <- probabilities
          edgeActivation <- activations
        yield (infectionProbability, recoveryProbability, edgeActivation)

      combinations.foldLeft[Either[String, Int]](Right(0)):
        case (completedEither, (infectionProbability, recoveryProbability, edgeActivation)) =>
          completedEither.flatMap: completed =>
            val pairConfig =
              config.copy(
                infectionProbability = infectionProbability,
                recoveryProbability = recoveryProbability,
                graphSpec = graphSpecWithActivation(config.graphSpec, edgeActivation)
              )
            BatchRunner.runSummaries(pairConfig, runsPerCombination).flatMap: summaries =>
              val row = ParameterSweepRow(infectionProbability, recoveryProbability, edgeActivation, summaries)
              val nextCompleted = completed + 1
              persistRow(nextCompleted, row).map(_ => nextCompleted)

  private def configuredActivation(graphSpec: GraphSpec): EdgeActivation =
    graphSpec match
      case generated: GraphSpec.Generated => generated.edgeActivation
      case fromFile: GraphSpec.FromFile   => fromFile.defaultActivation

  private def graphSpecWithActivation(graphSpec: GraphSpec, activation: EdgeActivation): GraphSpec =
    graphSpec match
      case generated: GraphSpec.Generated => generated.copy(edgeActivation = activation)
      case fromFile: GraphSpec.FromFile   => fromFile.copy(defaultActivation = activation)

  private def probabilityValues(
      minimum: Double,
      maximum: Double,
      step: Double
  ): Either[String, Vector[Double]] =
    if minimum < 0.0 || maximum > 1.0 || minimum > maximum then
      Left("sweep probability range must be within [0.0, 1.0] with minimum <= maximum")
    else if step <= 0.0 then
      Left("sweep probability step must be > 0.0")
    else
      val min = BigDecimal.decimal(minimum)
      val max = BigDecimal.decimal(maximum)
      val increment = BigDecimal.decimal(step)
      val values =
        Iterator
          .iterate(min)(_ + increment)
          .takeWhile(_ <= max)
          .map(_.toDouble)
          .toVector

      Right(values)
