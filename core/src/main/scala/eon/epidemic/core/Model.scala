package eon.epidemic.core

enum GraphShape:
  case ErdosRenyi
  case Ring

final case class EdgeActivation(onTicks: Int, offTicks: Int, phase: Int = 0):
  require(onTicks >= 0, "onTicks must be >= 0")
  require(offTicks >= 0, "offTicks must be >= 0")
  require(onTicks + offTicks > 0, "onTicks + offTicks must be > 0")

  def isActive(tick: Int): Boolean =
    if onTicks == 0 then false
    else
      val cycle = onTicks + offTicks
      val position = Math.floorMod(tick + phase, cycle)
      position < onTicks

final case class Edge(a: Int, b: Int, activation: EdgeActivation):
  require(a >= 0, "edge endpoint a must be >= 0")
  require(b >= 0, "edge endpoint b must be >= 0")
  require(a != b, "self-loops are not supported")

  def normalized: Edge =
    if a < b then this else Edge(b, a, activation)

  def endpoints: (Int, Int) =
    val sorted = normalized
    (sorted.a, sorted.b)

final case class Graph(
    nodeCount: Int,
    edges: Vector[Edge],
    adjacency: Map[Int, Vector[(Int, EdgeActivation)]]
):
  require(nodeCount > 0, "nodeCount must be > 0")

  def neighbors(node: Int): Vector[(Int, EdgeActivation)] =
    adjacency.getOrElse(node, Vector.empty)

object Graph:
  def fromEdges(nodeCount: Int, rawEdges: Vector[Edge]): Either[String, Graph] =
    if nodeCount <= 0 then Left("nodeCount must be > 0")
    else if rawEdges.exists(edge => edge.a >= nodeCount || edge.b >= nodeCount) then
      Left("edge endpoint exceeds nodeCount")
    else
      val normalized = rawEdges.map(_.normalized)
      val grouped = normalized.groupBy(_.endpoints)
      val duplicateEndpoints = grouped.collect:
        case (endpoints, entries) if entries.size > 1 => endpoints

      if duplicateEndpoints.nonEmpty then
        Left(s"duplicate edges detected for endpoints: ${duplicateEndpoints.mkString(", ")}")
      else
        val adjacency = normalized.foldLeft(Map.empty[Int, Vector[(Int, EdgeActivation)]]):
          (acc, edge) =>
            val fromA = acc.updated(edge.a, acc.getOrElse(edge.a, Vector.empty) :+ (edge.b, edge.activation))
            fromA.updated(edge.b, fromA.getOrElse(edge.b, Vector.empty) :+ (edge.a, edge.activation))

        Right(Graph(nodeCount = nodeCount, edges = normalized, adjacency = adjacency))

sealed trait GraphSpec

object GraphSpec:
  final case class Generated(
      shape: GraphShape,
      nodeCount: Int,
      edgeActivation: EdgeActivation,
      erdosProbability: Double,
      ringDegree: Int,
      seed: Long
  ) extends GraphSpec

  final case class FromFile(
      path: String,
      defaultActivation: EdgeActivation,
      explicitNodeCount: Option[Int]
  ) extends GraphSpec

final case class StopCondition(stopWhenNoInfected: Boolean, maxTicks: Int):
  require(maxTicks >= 0, "maxTicks must be >= 0")

final case class SimulationConfig(
    infectionProbability: Double,
    recoveryProbability: Double,
    initialInfected: Set[Int],
    stopCondition: StopCondition,
    seed: Long,
    graphSpec: GraphSpec,
    collectNodeStates: Boolean
)

final case class TickSnapshot(
    tick: Int,
    susceptible: Int,
    infected: Int,
    recovered: Int
)

enum NodeHealth:
  case Susceptible
  case Infected
  case Recovered

final case class TickNodeStates(
    tick: Int,
    nodeStates: Vector[NodeHealth]
)

final case class SimulationSummary(
    totalEverInfected: Int,
    epidemicDurationTicks: Int,
    peakInfected: Int,
    peakTick: Int,
    finalSusceptible: Int,
    finalInfected: Int,
    finalRecovered: Int
)

final case class SimulationResult(
    summary: SimulationSummary,
    timeseries: Vector[TickSnapshot],
    tickNodeStates: Option[Vector[TickNodeStates]],
    graph: Graph
)

final case class AggregateMetrics(
    minTotalEverInfected: Int,
    maxTotalEverInfected: Int,
    avgTotalEverInfected: Double,
    minDurationTicks: Int,
    maxDurationTicks: Int,
    avgDurationTicks: Double,
    minPeakInfected: Int,
    maxPeakInfected: Int,
    avgPeakInfected: Double
)

final case class BatchResult(
    runs: Int,
    summaries: Vector[SimulationSummary],
    aggregate: AggregateMetrics
)
