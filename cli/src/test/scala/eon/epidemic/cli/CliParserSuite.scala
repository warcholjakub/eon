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
