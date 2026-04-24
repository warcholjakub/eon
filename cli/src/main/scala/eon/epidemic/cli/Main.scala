package eon.epidemic.cli

import eon.epidemic.core.BatchRunner
import eon.epidemic.core.EdgeActivation
import eon.epidemic.core.GraphSpec
import eon.epidemic.core.SimulationConfig
import eon.epidemic.core.SimulationEngine
import eon.epidemic.core.StopCondition

import scala.util.Random

object Main:
  def main(args: Array[String]): Unit =
    val execution =
      for
        settings <- CliParser.parse(args)
        config <- toSimulationConfig(settings)
        _ <- execute(config, settings)
      yield ()

    execution match
      case Right(_) => ()
      case Left(error) =>
        Console.err.println(error)
        System.exit(1)

  private def execute(config: SimulationConfig, settings: CliSettings): Either[String, Unit] =
    if settings.runs <= 1 then
      SimulationEngine
        .run(config)
        .flatMap(result =>
          OutputWriter.writeSingle(
            outputDir = settings.outputDir,
            result = result,
            writeVisualization = settings.visualizationEnabled
          )
        )
    else
      BatchRunner.run(config, settings.runs).flatMap(result => OutputWriter.writeBatch(settings.outputDir, result))

  private def toSimulationConfig(settings: CliSettings): Either[String, SimulationConfig] =
    val graphSpecEither =
      settings.graphSource.toLowerCase match
        case "generated" =>
          Right(
            GraphSpec.Generated(
              shape = settings.graphShape,
              nodeCount = settings.nodeCount,
              edgeActivation = settings.activation,
              erdosProbability = settings.edgeProbability,
              ringDegree = settings.ringDegree,
              seed = settings.seed
            )
          )
        case "file" =>
          settings.graphFile match
            case Some(path) =>
              Right(
                GraphSpec.FromFile(
                  path = path,
                  defaultActivation = settings.activation,
                  explicitNodeCount = settings.explicitNodeCount
                )
              )
            case None => Left("graph-file is required when graph-source=file")
        case other => Left(s"unsupported graph-source: $other")

    graphSpecEither.flatMap: graphSpec =>
      initialInfected(settings).map: infected =>
        SimulationConfig(
          infectionProbability = settings.infectionProbability,
          recoveryProbability = settings.recoveryProbability,
          initialInfected = infected,
          diseaseModel = settings.diseaseModel,
          stopCondition = StopCondition(
            stopWhenNoInfected = settings.stopWhenNoInfected,
            maxTicks = settings.maxTicks
          ),
          seed = settings.seed,
          graphSpec = graphSpec,
          collectNodeStates = settings.visualizationEnabled && settings.runs <= 1
        )

  private def initialInfected(settings: CliSettings): Either[String, Set[Int]] =
    settings.initialInfectedCount match
      case Some(count) =>
        if count <= 0 then Left("initial-infected-count must be > 0")
        else if settings.graphSource.equalsIgnoreCase("file") then
          settings.explicitNodeCount match
            case Some(nodeCount) => pickRandomInfected(nodeCount, count, settings.seed)
            case None => Left("explicit-node-count is required with initial-infected-count for file graph")
        else pickRandomInfected(settings.nodeCount, count, settings.seed)

      case None =>
        val parsed =
          settings.initialInfected
            .split(',')
            .map(_.trim)
            .filter(_.nonEmpty)
            .toVector
            .map(_.toIntOption)

        if parsed.exists(_.isEmpty) then Left("initial-infected contains non-integer value")
        else
          val values = parsed.flatten.toSet
          if values.isEmpty then Left("initial-infected must not be empty")
          else Right(values)

  private def pickRandomInfected(nodeCount: Int, count: Int, seed: Long): Either[String, Set[Int]] =
    if nodeCount <= 0 then Left("nodeCount must be > 0")
    else if count > nodeCount then Left("initial-infected-count cannot exceed node count")
    else
      val rng = Random(seed + 999)
      val nodes = Array.tabulate(nodeCount)(identity)
      for i <- 0 until count do
        val j = i + rng.nextInt(nodeCount - i)
        val tmp = nodes(i)
        nodes(i) = nodes(j)
        nodes(j) = tmp

      Right(nodes.take(count).toSet)
