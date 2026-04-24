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

  private def buildFromFile(spec: GraphSpec.FromFile): Either[String, Graph] =
    val loaded =
      Using(Source.fromFile(spec.path)): source =>
        source.getLines().zipWithIndex.toVector

    loaded.toEither.left.map(_.getMessage).flatMap: lines =>
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
          yield Some(Edge(source, target, defaultActivation))

        case Vector(a, b, onTicks, offTicks, phase) =>
          for
            source <- parseInt(a, s"invalid source node at line $lineNumber")
            target <- parseInt(b, s"invalid target node at line $lineNumber")
            on <- parseInt(onTicks, s"invalid onTicks at line $lineNumber")
            off <- parseInt(offTicks, s"invalid offTicks at line $lineNumber")
            parsedPhase <- parseInt(phase, s"invalid phase at line $lineNumber")
          yield Some(Edge(source, target, EdgeActivation(on, off, parsedPhase)))

        case _ => Left(s"invalid edge format at line $lineNumber")

  private def parseInt(raw: String, error: String): Either[String, Int] =
    raw.toIntOption.toRight(error)

  private def randomizeActivation(base: EdgeActivation, rng: Random): EdgeActivation =
    val cycle = base.onTicks + base.offTicks
    if cycle <= 1 then base
    else base.copy(phase = Math.floorMod(base.phase + rng.nextInt(cycle), cycle))
