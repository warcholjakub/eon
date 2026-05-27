package eon.epidemic.core

object BatchRunner:
  def run(config: SimulationConfig, runs: Int): Either[String, BatchResult] =
    runSummaries(config, runs).map: summaries =>
      val aggregate = aggregateMetrics(summaries, config.trackedNodes)
      BatchResult(runs = runs, summaries = summaries, aggregate = aggregate)

  def runSummaries(config: SimulationConfig, runs: Int): Either[String, Vector[SimulationSummary]] =
    if runs <= 0 then Left("runs must be > 0")
    else
      (0 until runs).toVector.foldLeft[Either[String, Vector[SimulationSummary]]](Right(Vector.empty)):
        case (accEither, runIndex) =>
          accEither.flatMap: acc =>
            val runConfig = configForRun(config, runIndex)
            SimulationEngine.runSummary(runConfig).map(summary => acc :+ summary)

  def aggregate(config: SimulationConfig, runs: Int): Either[String, AggregateMetrics] =
    if runs <= 0 then Left("runs must be > 0")
    else
      (0 until runs).toVector
        .foldLeft[Either[String, Option[MetricsAccumulator]]](Right(None)):
          case (accumulatorEither, runIndex) =>
            accumulatorEither.flatMap: accumulator =>
              val runConfig = configForRun(config, runIndex)
              SimulationEngine.runSummary(runConfig).map: summary =>
                Some(accumulator.fold(MetricsAccumulator.from(summary, config.trackedNodes))(_.append(summary)))
        .map(_.get.toMetrics)

  private[core] def configForRun(config: SimulationConfig, runIndex: Int): SimulationConfig =
    val runSeed = config.seed + runIndex.toLong
    val runGraphSpec =
      config.graphSpec match
        case generated: GraphSpec.Generated => generated.copy(seed = runSeed)
        case other                          => other

    config.copy(seed = runSeed, graphSpec = runGraphSpec)

  private def aggregateMetrics(
      summaries: Vector[SimulationSummary],
      trackedNodes: Set[Int]
  ): AggregateMetrics =
    def average(values: Vector[Int]): Double =
      values.map(_.toDouble).sum / values.size.toDouble

    val totalInfectedValues = summaries.map(_.totalEverInfected)
    val durationValues = summaries.map(_.epidemicDurationTicks)
    val peakValues = summaries.map(_.peakInfected)

    val containmentValues = summaries.flatMap(_.containedToInitialGroups)
    val containmentRate =
      if containmentValues.isEmpty then None
      else Some(containmentValues.count(identity).toDouble / containmentValues.size.toDouble)

    val trackedNodeMetrics =
      trackedNodes.iterator.map: node =>
        val ticks = summaries.flatMap(_.trackedNodeFirstInfection.get(node))
        val infectionRate = ticks.size.toDouble / summaries.size.toDouble
        val avgTicks =
          if ticks.isEmpty then None
          else Some(ticks.map(_.toDouble).sum / ticks.size.toDouble)
        node -> TrackedNodeMetrics(infectionRate, avgTicks)
      .toMap

    AggregateMetrics(
      minTotalEverInfected = totalInfectedValues.min,
      maxTotalEverInfected = totalInfectedValues.max,
      avgTotalEverInfected = average(totalInfectedValues),
      minDurationTicks = durationValues.min,
      maxDurationTicks = durationValues.max,
      avgDurationTicks = average(durationValues),
      minPeakInfected = peakValues.min,
      maxPeakInfected = peakValues.max,
      avgPeakInfected = average(peakValues),
      containmentRate = containmentRate,
      trackedNodes = trackedNodeMetrics
    )

  private final case class TrackedAccumulator(infectedCount: Int, sumTicks: Long):
    def append(maybeTick: Option[Int]): TrackedAccumulator =
      maybeTick match
        case Some(tick) => copy(infectedCount = infectedCount + 1, sumTicks = sumTicks + tick.toLong)
        case None       => this

    def toMetrics(totalRuns: Int): TrackedNodeMetrics =
      TrackedNodeMetrics(
        infectionRate = infectedCount.toDouble / totalRuns.toDouble,
        avgTicksUntilInfected =
          if infectedCount == 0 then None
          else Some(sumTicks.toDouble / infectedCount.toDouble)
      )

  private object TrackedAccumulator:
    val empty: TrackedAccumulator = TrackedAccumulator(0, 0L)

  private final case class MetricsAccumulator(
      count: Int,
      minTotalEverInfected: Int,
      maxTotalEverInfected: Int,
      sumTotalEverInfected: Long,
      minDurationTicks: Int,
      maxDurationTicks: Int,
      sumDurationTicks: Long,
      minPeakInfected: Int,
      maxPeakInfected: Int,
      sumPeakInfected: Long,
      containmentObservations: Int,
      containmentSuccesses: Int,
      tracked: Map[Int, TrackedAccumulator]
  ):
    def append(summary: SimulationSummary): MetricsAccumulator =
      copy(
        count = count + 1,
        minTotalEverInfected = Math.min(minTotalEverInfected, summary.totalEverInfected),
        maxTotalEverInfected = Math.max(maxTotalEverInfected, summary.totalEverInfected),
        sumTotalEverInfected = sumTotalEverInfected + summary.totalEverInfected,
        minDurationTicks = Math.min(minDurationTicks, summary.epidemicDurationTicks),
        maxDurationTicks = Math.max(maxDurationTicks, summary.epidemicDurationTicks),
        sumDurationTicks = sumDurationTicks + summary.epidemicDurationTicks,
        minPeakInfected = Math.min(minPeakInfected, summary.peakInfected),
        maxPeakInfected = Math.max(maxPeakInfected, summary.peakInfected),
        sumPeakInfected = sumPeakInfected + summary.peakInfected,
        containmentObservations = containmentObservations + summary.containedToInitialGroups.map(_ => 1).getOrElse(0),
        containmentSuccesses = containmentSuccesses + summary.containedToInitialGroups.collect { case true => 1 }.getOrElse(0),
        tracked = tracked.map: (node, acc) =>
          node -> acc.append(summary.trackedNodeFirstInfection.get(node))
      )

    def toMetrics: AggregateMetrics =
      AggregateMetrics(
        minTotalEverInfected = minTotalEverInfected,
        maxTotalEverInfected = maxTotalEverInfected,
        avgTotalEverInfected = sumTotalEverInfected.toDouble / count,
        minDurationTicks = minDurationTicks,
        maxDurationTicks = maxDurationTicks,
        avgDurationTicks = sumDurationTicks.toDouble / count,
        minPeakInfected = minPeakInfected,
        maxPeakInfected = maxPeakInfected,
        avgPeakInfected = sumPeakInfected.toDouble / count,
        containmentRate =
          if containmentObservations == 0 then None
          else Some(containmentSuccesses.toDouble / containmentObservations.toDouble),
        trackedNodes = tracked.map((node, acc) => node -> acc.toMetrics(count))
      )

  private object MetricsAccumulator:
    def from(summary: SimulationSummary, trackedNodes: Set[Int]): MetricsAccumulator =
      val initialTracked = trackedNodes.iterator.map(node => node -> TrackedAccumulator.empty.append(summary.trackedNodeFirstInfection.get(node))).toMap
      MetricsAccumulator(
        count = 1,
        minTotalEverInfected = summary.totalEverInfected,
        maxTotalEverInfected = summary.totalEverInfected,
        sumTotalEverInfected = summary.totalEverInfected.toLong,
        minDurationTicks = summary.epidemicDurationTicks,
        maxDurationTicks = summary.epidemicDurationTicks,
        sumDurationTicks = summary.epidemicDurationTicks.toLong,
        minPeakInfected = summary.peakInfected,
        maxPeakInfected = summary.peakInfected,
        sumPeakInfected = summary.peakInfected.toLong,
        containmentObservations = summary.containedToInitialGroups.map(_ => 1).getOrElse(0),
        containmentSuccesses = summary.containedToInitialGroups.collect { case true => 1 }.getOrElse(0),
        tracked = initialTracked
      )
