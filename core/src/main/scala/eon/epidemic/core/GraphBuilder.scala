package eon.epidemic.core

import scala.io.Source
import scala.util.Random
import scala.util.Using

object GraphBuilder:
  def build(spec: GraphSpec): Either[String, Graph] =
    spec match
      case generated: GraphSpec.Generated => buildGenerated(generated)
      case fromFile: GraphSpec.FromFile   => buildFromFile(fromFile)

  private def buildGenerated(spec: GraphSpec.Generated): Either[String, Graph] =
    if spec.nodeCount <= 0 then Left("nodeCount must be > 0")
    else
      spec.shape match
        case GraphShape.ErdosRenyi => buildErdosRenyi(spec)
        case GraphShape.Ring       => buildRing(spec)
        case GraphShape.ClusteredVpn =>
          buildClusteredVpn(spec)
        case GraphShape.ThreeClustersHub =>
          buildThreeClustersHub(spec)

  private def buildErdosRenyi(spec: GraphSpec.Generated): Either[String, Graph] =
    if spec.erdosProbability < 0.0 || spec.erdosProbability > 1.0 then
      Left("erdosProbability must be in [0.0, 1.0]")
    else
      val rng = Random(spec.seed)
      val edges =
        (0 until spec.nodeCount).toVector.flatMap: a =>
          ((a + 1) until spec.nodeCount).toVector.collect:
            case b if rng.nextDouble() <= spec.erdosProbability =>
              Edge(a, b, randomizeActivation(spec.edgeActivation, rng))

      Graph.fromEdges(spec.nodeCount, edges)

  private def buildRing(spec: GraphSpec.Generated): Either[String, Graph] =
    if spec.ringDegree <= 0 || spec.ringDegree >= spec.nodeCount then
      Left("ringDegree must be > 0 and < nodeCount")
    else if spec.ringDegree % 2 != 0 then
      Left("ringDegree must be even")
    else
      val rng = Random(spec.seed)
      val halfDegree = spec.ringDegree / 2
      val edges =
        (0 until spec.nodeCount).toVector.flatMap: node =>
          (1 to halfDegree).toVector.map: offset =>
            val target = (node + offset) % spec.nodeCount
            Edge(node, target, randomizeActivation(spec.edgeActivation, rng)).normalized

      Graph.fromEdges(spec.nodeCount, edges.distinct)

  private def buildClusteredVpn(spec: GraphSpec.Generated): Either[String, Graph] =
    if spec.erdosProbability < 0.0 || spec.erdosProbability > 1.0 then
      Left("erdosProbability must be in [0.0, 1.0]")
    else if spec.nodeCount < 4 then
      Left("nodeCount must be >= 4 for clustered-vpn graph")
    else
      val leftCluster = (0 until (spec.nodeCount / 2)).toVector
      val rightCluster = ((spec.nodeCount / 2) until spec.nodeCount).toVector
      val maxVpnLinks = Math.min(leftCluster.size, rightCluster.size)
      val vpnLinks = Math.max(1, spec.ringDegree)

      if vpnLinks > maxVpnLinks then
        Left(s"ringDegree (vpn links) must be <= $maxVpnLinks for clustered-vpn graph")
      else
        val rng = Random(spec.seed)

        def clusterEdges(nodes: Vector[Int]): Vector[Edge] =
          nodes.indices.toVector.flatMap: i =>
            ((i + 1) until nodes.size).toVector.collect:
              case j if rng.nextDouble() <= spec.erdosProbability =>
                Edge(nodes(i), nodes(j), randomizeActivation(spec.edgeActivation, rng))

        val shuffledLeft = rng.shuffle(leftCluster)
        val shuffledRight = rng.shuffle(rightCluster)
        val vpnEdges =
          (0 until vpnLinks).toVector.map: idx =>
            Edge(shuffledLeft(idx), shuffledRight(idx), randomizeActivation(spec.edgeActivation, rng))

        val edges = (clusterEdges(leftCluster) ++ clusterEdges(rightCluster) ++ vpnEdges).map(_.normalized)
        Graph.fromEdges(spec.nodeCount, edges.distinct)

  private def buildThreeClustersHub(spec: GraphSpec.Generated): Either[String, Graph] =
    if spec.nodeCount < 4 || (spec.nodeCount - 1) % 3 != 0 then
      Left("nodeCount must be 1 plus a positive multiple of 3 for three-clusters-hub graph")
    else
      val rng = Random(spec.seed)
      val clusterSize = (spec.nodeCount - 1) / 3
      val clusters =
        (0 until 3).toVector.map: index =>
          val firstNode = 1 + index * clusterSize
          (firstNode until (firstNode + clusterSize)).toVector

      val clusterActivation = EdgeActivation(onTicks = 1, offTicks = 2)
      val clusterEdges =
        clusters.flatMap: nodes =>
          nodes.indices.toVector.flatMap: leftIndex =>
            ((leftIndex + 1) until nodes.size).toVector.map: rightIndex =>
              Edge(nodes(leftIndex), nodes(rightIndex), randomizeActivation(clusterActivation, rng))

      val hubEdges =
        clusters.map: nodes =>
          Edge(0, nodes.head, randomizeActivation(spec.edgeActivation, rng))

      Graph.fromEdges(spec.nodeCount, clusterEdges ++ hubEdges)

  private def buildFromFile(spec: GraphSpec.FromFile): Either[String, Graph] =
    val loaded =
      Using(Source.fromFile(spec.path)): source =>
        source.getLines().zipWithIndex.toVector

    loaded.toEither.left.map: error =>
      s"failed to read graph file '${spec.path}': ${error.toString}"
    .flatMap: lines =>
      val parsed = lines.foldLeft[Either[String, Vector[Edge]]](Right(Vector.empty)):
        case (accEither, (line, index)) =>
          accEither.flatMap: acc =>
            parseEdgeLine(line, index + 1, spec.defaultActivation).map:
              case Some(edge) => acc :+ edge
              case None       => acc

      parsed.flatMap: edges =>
        val inferredNodeCount =
          if edges.isEmpty then 0
          else edges.flatMap(edge => Vector(edge.a, edge.b)).max + 1

        val nodeCount = spec.explicitNodeCount.getOrElse(inferredNodeCount)
        Graph.fromEdges(nodeCount, edges)

  private def parseEdgeLine(
      line: String,
      lineNumber: Int,
      defaultActivation: EdgeActivation
  ): Either[String, Option[Edge]] =
    val trimmed = line.trim
    if trimmed.isEmpty || trimmed.startsWith("#") then Right(None)
    else
      val columns = trimmed.split(',').map(_.trim).toVector
      columns match
        case Vector(a, b) =>
          for
            source <- parseInt(a, s"invalid source node at line $lineNumber")
            target <- parseInt(b, s"invalid target node at line $lineNumber")
            edge <- buildEdge(source, target, defaultActivation, lineNumber)
          yield Some(edge)

        case Vector(a, b, onTicks, offTicks, phase) =>
          for
            source <- parseInt(a, s"invalid source node at line $lineNumber")
            target <- parseInt(b, s"invalid target node at line $lineNumber")
            on <- parseInt(onTicks, s"invalid onTicks at line $lineNumber")
            off <- parseInt(offTicks, s"invalid offTicks at line $lineNumber")
            parsedPhase <- parseInt(phase, s"invalid phase at line $lineNumber")
            activation <- buildActivation(on, off, parsedPhase, lineNumber)
            edge <- buildEdge(source, target, activation, lineNumber)
          yield Some(edge)

        case _ => Left(s"invalid edge format at line $lineNumber")

  private def parseInt(raw: String, error: String): Either[String, Int] =
    raw.toIntOption.toRight(error)

  private def buildActivation(
      onTicks: Int,
      offTicks: Int,
      phase: Int,
      lineNumber: Int
  ): Either[String, EdgeActivation] =
    scala.util.Try(EdgeActivation(onTicks, offTicks, phase)).toEither.left.map: error =>
      s"invalid edge activation at line $lineNumber: ${error.getMessage}"

  private def buildEdge(
      source: Int,
      target: Int,
      activation: EdgeActivation,
      lineNumber: Int
  ): Either[String, Edge] =
    scala.util.Try(Edge(source, target, activation)).toEither.left.map: error =>
      s"invalid edge at line $lineNumber: ${error.getMessage}"

  private def randomizeActivation(base: EdgeActivation, rng: Random): EdgeActivation =
    val cycle = base.onTicks + base.offTicks
    if cycle <= 1 then base
    else base.copy(phase = Math.floorMod(base.phase + rng.nextInt(cycle), cycle))
