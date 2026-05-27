package eon.epidemic.core

import scala.annotation.tailrec
import scala.util.Random

object SimulationEngine:
  def run(config: SimulationConfig): Either[String, SimulationResult] =
    GraphBuilder.build(config.graphSpec).flatMap(graph => runWithGraph(config, graph))

  def runSummary(config: SimulationConfig): Either[String, SimulationSummary] =
    GraphBuilder.build(config.graphSpec).flatMap(graph => runSummaryWithGraph(config, graph))

  def runWithGraph(config: SimulationConfig, graph: Graph): Either[String, SimulationResult] =
    runState(config, graph, recordTimeseries = true).map: finalState =>
      SimulationResult(
        summary = summary(finalState, config),
        timeseries = finalState.timeseries,
        tickNodeStates = finalState.tickNodeStates,
        graph = graph,
        layoutHint = layoutHint(config.graphSpec)
      )

  private def runSummaryWithGraph(config: SimulationConfig, graph: Graph): Either[String, SimulationSummary] =
    runState(config, graph, recordTimeseries = false).map(state => summary(state, config))

  private def runState(
      config: SimulationConfig,
      graph: Graph,
      recordTimeseries: Boolean
  ): Either[String, SimState] =
    validateConfig(config, graph).flatMap: _ =>
      val rng = Random(config.seed)
      val allNodes = (0 until graph.nodeCount).toSet
      val initialInfected = config.initialInfected
      val initialRecovered = Set.empty[Int]
      val initialSusceptible = allNodes -- initialInfected
      val initialTimeseries =
        if recordTimeseries then
          Vector(
            TickSnapshot(
              tick = 0,
              susceptible = initialSusceptible.size,
              infected = initialInfected.size,
              recovered = initialRecovered.size
            )
          )
        else Vector.empty

      val initialTrackedFirstInfection: Map[Int, Int] =
        (config.trackedNodes intersect initialInfected).iterator.map(node => node -> 0).toMap

      val initialState = SimState(
        tick = 0,
        susceptible = initialSusceptible,
        infected = initialInfected,
        recovered = initialRecovered,
        everInfected = initialInfected,
        timeseries = initialTimeseries,
        tickNodeStates =
          initialNodeStates(
            collectNodeStates = recordTimeseries && config.collectNodeStates,
            nodeCount = graph.nodeCount,
            susceptible = initialSusceptible,
            infected = initialInfected,
            recovered = initialRecovered
          ),
        peakInfected = initialInfected.size,
        peakTick = 0,
        trackedFirstInfection = initialTrackedFirstInfection
      )

      validatePartition(initialState, graph.nodeCount)
        .flatMap(_ => loop(Right(initialState), config, graph, rng, recordTimeseries))

  private def summary(state: SimState, config: SimulationConfig): SimulationSummary =
    SimulationSummary(
      totalEverInfected = state.everInfected.size,
      epidemicDurationTicks = state.tick,
      peakInfected = state.peakInfected,
      peakTick = state.peakTick,
      finalSusceptible = state.susceptible.size,
      finalInfected = state.infected.size,
      finalRecovered = state.recovered.size,
      trackedNodeFirstInfection = state.trackedFirstInfection,
      containedToInitialGroups = computeContainment(state, config)
    )

  private def computeContainment(state: SimState, config: SimulationConfig): Option[Boolean] =
    // Containment is meaningful only when caller supplied a group partition.
    // A node missing from the map is ignored — its hypothetical group can't
    // be classified as "outside the initial groups".
    if config.nodeGroups.isEmpty then None
    else
      val initialGroups = config.initialInfected.flatMap(config.nodeGroups.get)
      val everInfectedGroups = state.everInfected.flatMap(config.nodeGroups.get)
      Some(everInfectedGroups.subsetOf(initialGroups))

  private final case class SimState(
      tick: Int,
      susceptible: Set[Int],
      infected: Set[Int],
      recovered: Set[Int],
      everInfected: Set[Int],
      timeseries: Vector[TickSnapshot],
      tickNodeStates: Option[Vector[TickNodeStates]],
      peakInfected: Int,
      peakTick: Int,
      trackedFirstInfection: Map[Int, Int]
  )

  @tailrec
  private def loop(
      stateEither: Either[String, SimState],
      config: SimulationConfig,
      graph: Graph,
      rng: Random,
      recordTimeseries: Boolean
  ): Either[String, SimState] =
    stateEither match
      case Left(error) => Left(error)
      case Right(state) =>
        if shouldStop(state, config.stopCondition) then Right(state)
        else loop(step(state, config, graph, rng, recordTimeseries), config, graph, rng, recordTimeseries)

  private def step(
      state: SimState,
      config: SimulationConfig,
      graph: Graph,
      rng: Random,
      recordTimeseries: Boolean
  ): Either[String, SimState] =
    val infectionContacts =
      state.infected.toVector
        .flatMap: infectedNode =>
          graph.neighbors(infectedNode).collect:
            case (neighbor, activation)
                if state.susceptible.contains(neighbor) && activation.isActive(state.tick) =>
              neighbor
        .foldLeft(Map.empty[Int, Int]): (acc, node) =>
          acc.updatedWith(node):
            case Some(contactCount) => Some(contactCount + 1)
            case None               => Some(1)

    val infectionProbabilities =
      infectionContacts.toVector
        .sortBy(_._1)
        .map: (node, contactCount) =>
          node -> infectionProbabilityForContacts(config.infectionProbability, contactCount)

    val newInfected =
      sampleNodesWithProbabilities(
        nodes = infectionProbabilities,
        rng = rng
      )
    val newRecovered =
      sampleNodes(
        nodes = state.infected.toVector.sorted,
        probability = config.recoveryProbability,
        rng = rng
      )

    val (nextInfected, nextRecovered, nextSusceptible) =
      config.diseaseModel match
        case DiseaseModel.SIR =>
          (
            (state.infected -- newRecovered) ++ newInfected,
            state.recovered ++ newRecovered,
            state.susceptible -- newInfected
          )
        case DiseaseModel.SIS =>
          (
            (state.infected -- newRecovered) ++ newInfected,
            Set.empty[Int],
            (state.susceptible ++ newRecovered) -- newInfected
          )
    val nextEverInfected = state.everInfected ++ newInfected
    val nextTick = state.tick + 1

    val nextTrackedFirstInfection =
      newInfected.iterator
        .filter(config.trackedNodes.contains)
        .filterNot(state.trackedFirstInfection.contains)
        .foldLeft(state.trackedFirstInfection)((acc, node) => acc.updated(node, nextTick))

    val nextTimeseries =
      if recordTimeseries then
        state.timeseries :+ TickSnapshot(
          tick = nextTick,
          susceptible = nextSusceptible.size,
          infected = nextInfected.size,
          recovered = nextRecovered.size
        )
      else state.timeseries

    val (nextPeakInfected, nextPeakTick) =
      if nextInfected.size > state.peakInfected then (nextInfected.size, nextTick)
      else (state.peakInfected, state.peakTick)

    val nextTickNodeStates =
      state.tickNodeStates.map: states =>
        states :+ TickNodeStates(
          tick = nextTick,
          nodeStates =
            buildNodeStates(
              nodeCount = graph.nodeCount,
              susceptible = nextSusceptible,
              infected = nextInfected,
              recovered = nextRecovered
            )
        )

    val nextState = state.copy(
      tick = nextTick,
      susceptible = nextSusceptible,
      infected = nextInfected,
      recovered = nextRecovered,
      everInfected = nextEverInfected,
      timeseries = nextTimeseries,
      tickNodeStates = nextTickNodeStates,
      peakInfected = nextPeakInfected,
      peakTick = nextPeakTick,
      trackedFirstInfection = nextTrackedFirstInfection
    )

    validatePartition(nextState, graph.nodeCount).map(_ => nextState)

  private def shouldStop(state: SimState, stopCondition: StopCondition): Boolean =
    state.tick >= stopCondition.maxTicks ||
      (stopCondition.stopWhenNoInfected && state.infected.isEmpty)

  private def validateConfig(config: SimulationConfig, graph: Graph): Either[String, Unit] =
    if config.infectionProbability < 0.0 || config.infectionProbability > 1.0 then
      Left("infectionProbability must be in [0.0, 1.0]")
    else if config.recoveryProbability < 0.0 || config.recoveryProbability > 1.0 then
      Left("recoveryProbability must be in [0.0, 1.0]")
    else if config.initialInfected.exists(node => node < 0 || node >= graph.nodeCount) then
      Left("initialInfected contains nodes outside graph")
    else Right(())

  private def buildNodeStates(
      nodeCount: Int,
      susceptible: Set[Int],
      infected: Set[Int],
      recovered: Set[Int]
  ): Vector[NodeHealth] =
    (0 until nodeCount).toVector.map: node =>
      if infected.contains(node) then NodeHealth.Infected
      else if recovered.contains(node) then NodeHealth.Recovered
      else NodeHealth.Susceptible

  private def initialNodeStates(
      collectNodeStates: Boolean,
      nodeCount: Int,
      susceptible: Set[Int],
      infected: Set[Int],
      recovered: Set[Int]
  ): Option[Vector[TickNodeStates]] =
    if collectNodeStates then
      Some(
        Vector(
          TickNodeStates(
            tick = 0,
            nodeStates =
              buildNodeStates(
                nodeCount = nodeCount,
                susceptible = susceptible,
                infected = infected,
                recovered = recovered
              )
          )
        )
      )
    else None

  private def sampleNodes(nodes: Vector[Int], probability: Double, rng: Random): Set[Int] =
    nodes.foldLeft(Set.empty[Int]): (acc, node) =>
      if rng.nextDouble() <= probability then acc + node
      else acc

  private def sampleNodesWithProbabilities(nodes: Vector[(Int, Double)], rng: Random): Set[Int] =
    nodes.foldLeft(Set.empty[Int]):
      case (acc, (node, probability)) =>
        if rng.nextDouble() <= probability then acc + node
        else acc

  private[core] def infectionProbabilityForContacts(baseProbability: Double, contactCount: Int): Double =
    if contactCount <= 0 then 0.0
    else 1.0 - Math.pow(1.0 - baseProbability, contactCount.toDouble)

  private def validatePartition(state: SimState, nodeCount: Int): Either[String, Unit] =
    val allNodes = (0 until nodeCount).toSet
    val union = state.susceptible union state.infected union state.recovered
    val infectedRecoveredOverlap = state.infected intersect state.recovered
    val infectedSusceptibleOverlap = state.infected intersect state.susceptible
    val recoveredSusceptibleOverlap = state.recovered intersect state.susceptible

    if infectedRecoveredOverlap.nonEmpty then Left(s"invalid state partition at tick ${state.tick}: infected/recovered overlap")
    else if infectedSusceptibleOverlap.nonEmpty then Left(s"invalid state partition at tick ${state.tick}: infected/susceptible overlap")
    else if recoveredSusceptibleOverlap.nonEmpty then Left(s"invalid state partition at tick ${state.tick}: recovered/susceptible overlap")
    else if union != allNodes then Left(s"invalid state partition at tick ${state.tick}: node coverage mismatch")
    else Right(())

  private def layoutHint(spec: GraphSpec): Option[String] =
    spec match
      case generated: GraphSpec.Generated if generated.shape == GraphShape.ClusteredVpn =>
        Some("clustered-vpn")
      case generated: GraphSpec.Generated if generated.shape == GraphShape.ThreeClustersHub =>
        Some("three-clusters-hub")
      case _ => None
