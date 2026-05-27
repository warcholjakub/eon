package eon.epidemic.cli

import munit.FunSuite

import java.nio.file.Files

class CliParserSuite extends FunSuite:
  test("loads settings from hocon file"):
    val file = Files.createTempFile("epidemic", ".conf")
    Files.writeString(
      file,
      """graph {
        |  source = generated
        |  shape = ring
        |  node-count = 50
        |  ring-degree = 6
        |}
        |simulation {
        |  infection-probability = 0.3
        |  recovery-probability = 0.2
        |  max-ticks = 100
        |  initial-infected = [0, 1]
        |  runs = 3
        |}
        |""".stripMargin
    )

    val parsed = CliParser.parse(Array("--config-file", file.toString)).toOption.get

    assertEquals(parsed.graphSource, "generated")
    assertEquals(parsed.nodeCount, 50)
    assertEquals(parsed.ringDegree, 6)
    assertEquals(parsed.initialInfected, "0,1")
    assertEquals(parsed.runs, 3)

  test("visualization flag can be disabled from hocon"):
    val file = Files.createTempFile("epidemic-visualization", ".conf")
    Files.writeString(
      file,
      """output {
        |  visualization = false
        |}
        |""".stripMargin
    )

    val parsed = CliParser.parse(Array("--config-file", file.toString)).toOption.get
    assertEquals(parsed.visualizationEnabled, false)

  test("loads parameter sweep settings from hocon"):
    val file = Files.createTempFile("epidemic-sweep", ".conf")
    Files.writeString(
      file,
      """sweep {
        |  enabled = true
        |  min-probability = 0.05
        |  max-probability = 0.50
        |  probability-step = 0.05
        |  activations = [
        |    { on-ticks = 1, off-ticks = 0 }
        |    { on-ticks = 1, off-ticks = 3 }
        |  ]
        |}
        |""".stripMargin
    )

    val parsed = CliParser.parse(Array("--config-file", file.toString)).toOption.get
    assert(parsed.sweepEnabled)
    assertEquals(parsed.sweepMinProbability, 0.05)
    assertEquals(parsed.sweepMaxProbability, 0.5)
    assertEquals(parsed.sweepProbabilityStep, 0.05)
    assertEquals(parsed.sweepEdgeActivations.map(_.onTicks), Vector(1, 1))
    assertEquals(parsed.sweepEdgeActivations.map(_.offTicks), Vector(0, 3))

  test("malformed initial-infected list in hocon returns clear error"):
    val file = Files.createTempFile("epidemic-initial-infected", ".conf")
    Files.writeString(
      file,
      """simulation {
        |  initial-infected = [0, "oops", 2]
        |}
        |""".stripMargin
    )

    val result = CliParser.parse(Array("--config-file", file.toString))
    assert(result.isLeft)
    assert(result.swap.toOption.get.contains("invalid simulation.initial-infected list"))

  test("config preset is not reapplied during cli merge"):
    val file = Files.createTempFile("epidemic-preset", ".conf")
    Files.writeString(
      file,
      """preset = "balanced"
        |
        |simulation {
        |  runs = 1
        |}
        |""".stripMargin
    )

    val parsed = CliParser.parse(Array("--config-file", file.toString)).toOption.get
    assertEquals(parsed.runs, 1)

  test("preset is applied and cli overrides keep precedence"):
    val parsed =
      CliParser
        .parse(Array("--preset", "high-spread", "--recovery-probability", "0.15"))
        .toOption
        .get

    assertEquals(parsed.preset, Some("high-spread"))
    assertEquals(parsed.infectionProbability, 0.25)
    assertEquals(parsed.recoveryProbability, 0.15)

  test("loads legacy properties file for compatibility"):
    val file = Files.createTempFile("epidemic", ".properties")
    Files.writeString(
      file,
      """graphSource=generated
        |graphShape=ring
        |nodeCount=40
        |ringDegree=4
        |infectionProbability=0.2
        |recoveryProbability=0.1
        |maxTicks=80
        |initialInfected=0,1
        |runs=2
        |""".stripMargin
    )

    val parsed = CliParser.parse(Array("--config-file", file.toString)).toOption.get

    assertEquals(parsed.nodeCount, 40)
    assertEquals(parsed.runs, 2)

  test("rejects runs <= 0 from hocon config"):
    val file = Files.createTempFile("epidemic-runs-invalid", ".conf")
    Files.writeString(
      file,
      """simulation {
        |  runs = 0
        |}
        |""".stripMargin
    )

    val result = CliParser.parse(Array("--config-file", file.toString))
    assertEquals(result, Left("runs must be >= 1"))

  test("rejects runs <= 0 from cli arguments"):
    val result = CliParser.parse(Array("--runs", "0"))
    assertEquals(result, Left("runs must be >= 1"))

  test("supports switching disease model to sis"):
    val result = CliParser.parse(Array("--model", "sis")).toOption.get
    assertEquals(result.diseaseModel, eon.epidemic.core.DiseaseModel.SIS)
