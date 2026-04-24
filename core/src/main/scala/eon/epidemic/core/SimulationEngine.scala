package eon.epidemic.core

import scala.annotation.tailrec
import scala.util.Random

object SimulationEngine:
  def run(config: SimulationConfig): Either[String, SimulationResult] =
    GraphBuilder.build(config.graphSpec).flatMap(graph => runWithGraph(config, graph))

  def runWithGraph(config: SimulationConfig, graph: Graph): Either[String, SimulationResult] =
    validateConfig(config, graph).flatMap: _ =>
      val rng = Random(config.seed)
      val allNodes = (0 until graph.nodeCount).toSet
      val initialInfected = config.initialInfected
      val initialRecovered = Set.empty[Int]
      val initialSusceptible = allNodes -- initialInfected
      val initialSnapshot = TickSnapshot(
        tick = 0,
        susceptible = initialSusceptible.size,
        infected = initialInfected.size,
        recovered = initialRecovered.size
      )

      val initialState = SimState(
        tick = 0,
        susceptible = initialSusceptible,
        infected = initialInfected,
        recovered = initialRecovered,
        everInfected = initialInfected,
        timeseries = Vector(initialSnapshot),
        tickNodeStates =
          initialNodeStates(
            collectNodeStates = config.collectNodeStates,
            nodeCount = graph.nodeCount,
            susceptible = initialSusceptible,
            infected = initialInfected,
            recovered = initialRecovered
          ),
        peakInfected = initialInfected.size,
        peakTick = 0
      )

      validatePartition(initialState, graph.nodeCount)
        .flatMap(_ => loop(Right(initialState), config, graph, rng))
        .map: finalState =>
          val summary = SimulationSummary(
            totalEverInfected = finalState.everInfected.size,
            epidemicDurationTicks = finalState.tick,
            peakInfected = finalState.peakInfected,
            peakTick = finalState.peakTick,
            finalSusceptible = finalState.susceptible.size,
            finalInfected = finalState.infected.size,
            finalRecovered = finalState.recovered.size
          )

          SimulationResult(
            summary = summary,
            timeseries = finalState.timeseries,
            tickNodeStates = finalState.tickNodeStates,
            graph = graph
          )

  private final case class SimState(
      tick: Int,
      susceptible: Set[Int],
      infected: Set[Int],
      recovered: Set[Int],
      everInfected: Set[Int],
      timeseries: Vector[TickSnapshot],
      tickNodeStates: Option[Vector[TickNodeStates]],
      peakInfected: Int,
      peakTick: Int
  )

  @tailrec
  private def loop(
      stateEither: Either[String, SimState],
      config: SimulationConfig,
      graph: Graph,
      rng: Random
  ): Either[String, SimState] =
    stateEither match
      case Left(error) => Left(error)
      case Right(state) =>
        if shouldStop(state, config.stopCondition) then Right(state)
        else loop(step(state, config, graph, rng), config, graph, rng)

  private def step(
      state: SimState,
      config: SimulationConfig,
      graph: Graph,
      rng: Random
  ): Either[String, SimState] =
    val infectionCandidates =
      state.infected.toVector
        .flatMap: infectedNode =>
          graph.neighbors(infectedNode).collect:
            case (neighbor, activation)
                if state.susceptible.contains(neighbor) && activation.isActive(state.tick) =>
              neighbor
        .toSet

    val newInfected =
      sampleNodes(
        nodes = infectionCandidates.toVector.sorted,
        probability = config.infectionProbability,
        rng = rng
      )
    val newRecovered =
      sampleNodes(
        nodes = state.infected.toVector.sorted,
        probability = config.recoveryProbability,
        rng = rng
      )

    val nextInfected = (state.infected -- newRecovered) ++ newInfected
    val nextRecovered = state.recovered ++ newRecovered
    val nextSusceptible = state.susceptible -- newInfected
    val nextEverInfected = state.everInfected ++ newInfected
    val nextTick = state.tick + 1

    val nextSnapshot = TickSnapshot(
      tick = nextTick,
      susceptible = nextSusceptible.size,
      infected = nextInfected.size,
      recovered = nextRecovered.size
    )

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
      timeseries = state.timeseries :+ nextSnapshot,
      tickNodeStates = nextTickNodeStates,
      peakInfected = nextPeakInfected,
      peakTick = nextPeakTick
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
