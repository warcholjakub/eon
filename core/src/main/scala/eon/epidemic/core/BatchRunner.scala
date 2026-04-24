package eon.epidemic.core

object BatchRunner:
  def run(config: SimulationConfig, runs: Int): Either[String, BatchResult] =
    if runs <= 0 then Left("runs must be > 0")
    else
      val summariesEither =
        (0 until runs).toVector.foldLeft[Either[String, Vector[SimulationSummary]]](Right(Vector.empty)):
          case (accEither, runIndex) =>
            accEither.flatMap: acc =>
              val runConfig = configForRun(config, runIndex)
              SimulationEngine.run(runConfig).map(result => acc :+ result.summary)

      summariesEither.map: summaries =>
        val aggregate = aggregateMetrics(summaries)
        BatchResult(runs = runs, summaries = summaries, aggregate = aggregate)

  private[core] def configForRun(config: SimulationConfig, runIndex: Int): SimulationConfig =
    val runSeed = config.seed + runIndex.toLong
    val runGraphSpec =
      config.graphSpec match
        case generated: GraphSpec.Generated => generated.copy(seed = runSeed)
        case other                          => other

    config.copy(seed = runSeed, graphSpec = runGraphSpec)

  private def aggregateMetrics(summaries: Vector[SimulationSummary]): AggregateMetrics =
    def average(values: Vector[Int]): Double =
      values.map(_.toDouble).sum / values.size.toDouble

    val totalInfectedValues = summaries.map(_.totalEverInfected)
    val durationValues = summaries.map(_.epidemicDurationTicks)
    val peakValues = summaries.map(_.peakInfected)

    AggregateMetrics(
      minTotalEverInfected = totalInfectedValues.min,
      maxTotalEverInfected = totalInfectedValues.max,
      avgTotalEverInfected = average(totalInfectedValues),
      minDurationTicks = durationValues.min,
      maxDurationTicks = durationValues.max,
      avgDurationTicks = average(durationValues),
      minPeakInfected = peakValues.min,
      maxPeakInfected = peakValues.max,
      avgPeakInfected = average(peakValues)
    )
