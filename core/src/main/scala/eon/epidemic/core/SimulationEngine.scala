package eon.epidemic.core

import scala.annotation.tailrec
import scala.util.Random

object SimulationEngine:
  def run(config: SimulationConfig): Either[String, SimulationResult] =
    GraphBuilder.build(config.graphSpec).flatMap(graph => runWithGraph(config, graph))

  def runWithGraph(config: SimulationConfig, graph: Graph): Either[String, SimulationResult] =
    validateConfig(config, graph).map: _ =>
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

      val finalState = loop(initialState, config, graph, rng)
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
      state: SimState,
      config: SimulationConfig,
      graph: Graph,
      rng: Random
  ): SimState =
    if shouldStop(state, config.stopCondition) then state
    else loop(step(state, config, graph, rng), config, graph, rng)

  private def step(
      state: SimState,
      config: SimulationConfig,
      graph: Graph,
      rng: Random
  ): SimState =
    val infectionCandidates =
      state.infected.toVector
        .flatMap: infectedNode =>
          graph.neighbors(infectedNode).collect:
            case (neighbor, activation)
                if state.susceptible.contains(neighbor) && activation.isActive(state.tick) =>
              neighbor
        .toSet

    val newInfected = infectionCandidates.filter(_ => rng.nextDouble() <= config.infectionProbability)
    val newRecovered = state.infected.filter(_ => rng.nextDouble() <= config.recoveryProbability)

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

    state.copy(
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
      else if susceptible.contains(node) then NodeHealth.Susceptible
      else
        throw IllegalStateException(
          s"Node $node is not present in susceptible, infected, or recovered sets"
        )

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
