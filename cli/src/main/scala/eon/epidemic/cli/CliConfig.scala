package eon.epidemic.cli

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueType
import eon.epidemic.core.EdgeActivation
import eon.epidemic.core.GraphShape

import java.io.File
import java.io.FileInputStream
import java.util.Properties
import scala.jdk.CollectionConverters.*
import scala.util.Try

final case class CliSettings(
    preset: Option[String],
    graphSource: String,
    graphShape: GraphShape,
    graphFile: Option[String],
    explicitNodeCount: Option[Int],
    nodeCount: Int,
    edgeProbability: Double,
    ringDegree: Int,
    activation: EdgeActivation,
    infectionProbability: Double,
    recoveryProbability: Double,
    maxTicks: Int,
    stopWhenNoInfected: Boolean,
    initialInfected: String,
    initialInfectedCount: Option[Int],
    seed: Long,
    runs: Int,
    outputDir: String
)

object CliSettings:
  val default: CliSettings =
    CliSettings(
      preset = None,
      graphSource = "generated",
      graphShape = GraphShape.ErdosRenyi,
      graphFile = None,
      explicitNodeCount = None,
      nodeCount = 200,
      edgeProbability = 0.02,
      ringDegree = 4,
      activation = EdgeActivation(onTicks = 1, offTicks = 0, phase = 0),
      infectionProbability = 0.08,
      recoveryProbability = 0.05,
      maxTicks = 400,
      stopWhenNoInfected = true,
      initialInfected = "0",
      initialInfectedCount = None,
      seed = 42L,
      runs = 1,
      outputDir = "out"
    )

object ScenarioPresets:
  private val presets: Map[String, CliSettings => CliSettings] =
    Map(
      "high-spread" -> (settings =>
        settings.copy(
          infectionProbability = 0.25,
          recoveryProbability = 0.03,
          edgeProbability = 0.04,
          activation = settings.activation.copy(onTicks = 5, offTicks = 1),
          maxTicks = 600,
          runs = 30
        )
      ),
      "slow-recovery" -> (settings =>
        settings.copy(
          infectionProbability = 0.1,
          recoveryProbability = 0.01,
          edgeProbability = 0.02,
          activation = settings.activation.copy(onTicks = 4, offTicks = 1),
          maxTicks = 800,
          runs = 30
        )
      ),
      "balanced" -> (settings =>
        settings.copy(
          infectionProbability = 0.08,
          recoveryProbability = 0.05,
          edgeProbability = 0.02,
          activation = settings.activation.copy(onTicks = 3, offTicks = 2),
          maxTicks = 500,
          runs = 20
        )
      )
    )

  def available: Vector[String] = presets.keys.toVector.sorted

  def applyPreset(base: CliSettings, name: String): Either[String, CliSettings] =
    presets.get(name.toLowerCase) match
      case Some(transform) => Right(transform(base).copy(preset = Some(name.toLowerCase)))
      case None => Left(s"unsupported preset: $name (available: ${available.mkString(", ")})")

final case class CliOverrides(
    configFile: Option[String] = None,
    preset: Option[String] = None,
    graphSource: Option[String] = None,
    graphShape: Option[String] = None,
    graphFile: Option[String] = None,
    explicitNodeCount: Option[Int] = None,
    nodeCount: Option[Int] = None,
    edgeProbability: Option[Double] = None,
    ringDegree: Option[Int] = None,
    activationOnTicks: Option[Int] = None,
    activationOffTicks: Option[Int] = None,
    activationPhase: Option[Int] = None,
    infectionProbability: Option[Double] = None,
    recoveryProbability: Option[Double] = None,
    maxTicks: Option[Int] = None,
    stopWhenNoInfected: Option[Boolean] = None,
    initialInfected: Option[String] = None,
    initialInfectedCount: Option[Int] = None,
    seed: Option[Long] = None,
    runs: Option[Int] = None,
    outputDir: Option[String] = None
)

object ConfigFileLoader:
  def load(path: String): Either[String, CliSettings] =
    val file = File(path)
    if !file.exists() then Left(s"config file does not exist: $path")
    else
      val parsedOverrides =
        if path.endsWith(".properties") then loadProperties(path)
        else loadHocon(path)

      parsedOverrides.flatMap(overrides => applyOverrides(CliSettings.default, overrides))

  def applyOverrides(base: CliSettings, overrides: CliOverrides): Either[String, CliSettings] =
    val baseWithPresetEither =
      overrides.preset match
        case Some(name) => ScenarioPresets.applyPreset(base, name)
        case None       => Right(base)

    baseWithPresetEither.flatMap: baseWithPreset =>
      val effectivePreset = overrides.preset.map(_.toLowerCase).orElse(baseWithPreset.preset)
      parseGraphShape(overrides.graphShape.getOrElse(baseWithPreset.graphShape.toString)).flatMap: shape =>
        val activationEither =
          buildActivation(
            onTicks = overrides.activationOnTicks.getOrElse(baseWithPreset.activation.onTicks),
            offTicks = overrides.activationOffTicks.getOrElse(baseWithPreset.activation.offTicks),
            phase = overrides.activationPhase.getOrElse(baseWithPreset.activation.phase)
          )

        activationEither.map: activation =>
          baseWithPreset.copy(
            preset = effectivePreset,
            graphSource = overrides.graphSource.getOrElse(baseWithPreset.graphSource),
            graphShape = shape,
            graphFile = overrides.graphFile.orElse(baseWithPreset.graphFile),
            explicitNodeCount = overrides.explicitNodeCount.orElse(baseWithPreset.explicitNodeCount),
            nodeCount = overrides.nodeCount.getOrElse(baseWithPreset.nodeCount),
            edgeProbability = overrides.edgeProbability.getOrElse(baseWithPreset.edgeProbability),
            ringDegree = overrides.ringDegree.getOrElse(baseWithPreset.ringDegree),
            activation = activation,
            infectionProbability = overrides.infectionProbability.getOrElse(baseWithPreset.infectionProbability),
            recoveryProbability = overrides.recoveryProbability.getOrElse(baseWithPreset.recoveryProbability),
            maxTicks = overrides.maxTicks.getOrElse(baseWithPreset.maxTicks),
            stopWhenNoInfected = overrides.stopWhenNoInfected.getOrElse(baseWithPreset.stopWhenNoInfected),
            initialInfected = overrides.initialInfected.getOrElse(baseWithPreset.initialInfected),
            initialInfectedCount = overrides.initialInfectedCount.orElse(baseWithPreset.initialInfectedCount),
            seed = overrides.seed.getOrElse(baseWithPreset.seed),
            runs = overrides.runs.getOrElse(baseWithPreset.runs),
            outputDir = overrides.outputDir.getOrElse(baseWithPreset.outputDir)
          )

  private def buildActivation(onTicks: Int, offTicks: Int, phase: Int): Either[String, EdgeActivation] =
    Try(EdgeActivation(onTicks = onTicks, offTicks = offTicks, phase = phase)).toEither.left.map:
      error => s"invalid edge activation: ${error.getMessage}"

  private def loadProperties(path: String): Either[String, CliOverrides] =
    val props = Properties()
    val loaded =
      try
        val input = FileInputStream(path)
        try props.load(input)
        finally input.close()
        Right(())
      catch
        case error: Exception => Left(s"failed to load config file: ${error.getMessage}")

    loaded.map: _ =>
      val map = props.asScala.toMap
      CliOverrides(
        preset = map.get("preset"),
        graphSource = map.get("graphSource"),
        graphShape = map.get("graphShape"),
        graphFile = map.get("graphFile"),
        explicitNodeCount = map.get("explicitNodeCount").flatMap(_.toIntOption),
        nodeCount = map.get("nodeCount").flatMap(_.toIntOption),
        edgeProbability = map.get("edgeProbability").flatMap(_.toDoubleOption),
        ringDegree = map.get("ringDegree").flatMap(_.toIntOption),
        activationOnTicks = map.get("activationOnTicks").flatMap(_.toIntOption),
        activationOffTicks = map.get("activationOffTicks").flatMap(_.toIntOption),
        activationPhase = map.get("activationPhase").flatMap(_.toIntOption),
        infectionProbability = map.get("infectionProbability").flatMap(_.toDoubleOption),
        recoveryProbability = map.get("recoveryProbability").flatMap(_.toDoubleOption),
        maxTicks = map.get("maxTicks").flatMap(_.toIntOption),
        stopWhenNoInfected = map.get("stopWhenNoInfected").flatMap(_.toBooleanOption),
        initialInfected = map.get("initialInfected"),
        initialInfectedCount = map.get("initialInfectedCount").flatMap(_.toIntOption),
        seed = map.get("seed").flatMap(_.toLongOption),
        runs = map.get("runs").flatMap(_.toIntOption),
        outputDir = map.get("outputDir")
      )

  private def loadHocon(path: String): Either[String, CliOverrides] =
    Try(ConfigFactory.parseFile(File(path)).resolve()).toEither.left.map: error =>
      s"failed to parse HOCON config: ${error.getMessage}"
    .map: config =>
      CliOverrides(
        preset = getString(config, "preset"),
        graphSource = getString(config, "graph.source", "graphSource"),
        graphShape = getString(config, "graph.shape", "graphShape"),
        graphFile = getString(config, "graph.file", "graphFile"),
        explicitNodeCount = getInt(config, "graph.explicit-node-count", "graph.explicitNodeCount"),
        nodeCount = getInt(config, "graph.node-count", "graph.nodeCount", "nodeCount"),
        edgeProbability = getDouble(config, "graph.edge-probability", "graph.edgeProbability", "edgeProbability"),
        ringDegree = getInt(config, "graph.ring-degree", "graph.ringDegree", "ringDegree"),
        activationOnTicks = getInt(config, "graph.activation.on-ticks", "graph.activation.onTicks", "activationOnTicks"),
        activationOffTicks = getInt(config, "graph.activation.off-ticks", "graph.activation.offTicks", "activationOffTicks"),
        activationPhase = getInt(config, "graph.activation.phase", "activationPhase"),
        infectionProbability = getDouble(config, "simulation.infection-probability", "simulation.infectionProbability", "infectionProbability"),
        recoveryProbability = getDouble(config, "simulation.recovery-probability", "simulation.recoveryProbability", "recoveryProbability"),
        maxTicks = getInt(config, "simulation.max-ticks", "simulation.maxTicks", "maxTicks"),
        stopWhenNoInfected = getBoolean(config, "simulation.stop-when-no-infected", "simulation.stopWhenNoInfected", "stopWhenNoInfected"),
        initialInfected = getInitialInfected(config),
        initialInfectedCount = getInt(config, "simulation.initial-infected-count", "simulation.initialInfectedCount", "initialInfectedCount"),
        seed = getLong(config, "simulation.seed", "seed"),
        runs = getInt(config, "simulation.runs", "runs"),
        outputDir = getString(config, "output.dir", "outputDir")
      )

  private def getString(config: Config, paths: String*): Option[String] =
    paths.find(config.hasPath).map(config.getString)

  private def getInt(config: Config, paths: String*): Option[Int] =
    paths.find(config.hasPath).flatMap(path => Try(config.getInt(path)).toOption)

  private def getLong(config: Config, paths: String*): Option[Long] =
    paths.find(config.hasPath).flatMap(path => Try(config.getLong(path)).toOption)

  private def getDouble(config: Config, paths: String*): Option[Double] =
    paths.find(config.hasPath).flatMap(path => Try(config.getDouble(path)).toOption)

  private def getBoolean(config: Config, paths: String*): Option[Boolean] =
    paths.find(config.hasPath).flatMap(path => Try(config.getBoolean(path)).toOption)

  private def getInitialInfected(config: Config): Option[String] =
    val candidates = Vector("simulation.initial-infected", "simulation.initialInfected", "initialInfected")
    candidates.find(config.hasPath).flatMap: path =>
      val valueType = config.getValue(path).valueType()
      if valueType == ConfigValueType.LIST then
        val values = config.getIntList(path).asScala.toVector
        if values.isEmpty then None else Some(values.mkString(","))
      else getString(config, path)

  private def parseGraphShape(raw: String): Either[String, GraphShape] =
    raw.toLowerCase match
      case "erdos" | "erdosrenyi" | "erdos-renyi" => Right(GraphShape.ErdosRenyi)
      case "ring"                                   => Right(GraphShape.Ring)
      case other                                     => Left(s"unsupported graph shape: $other")
