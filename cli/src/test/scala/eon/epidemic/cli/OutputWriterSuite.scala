package eon.epidemic.cli

import eon.epidemic.core.Edge
import eon.epidemic.core.EdgeActivation
import eon.epidemic.core.Graph
import eon.epidemic.core.SimulationResult
import eon.epidemic.core.SimulationSummary
import eon.epidemic.core.TickNodeStates
import eon.epidemic.core.TickSnapshot
import munit.FunSuite

import java.nio.file.Files

class OutputWriterSuite extends FunSuite:
  test("writeSingle returns Left when visualization requested but node states are missing"):
    val outputDir = Files.createTempDirectory("eon-output-writer-missing-states")
    val result = sampleResult(tickNodeStates = None)

    val write = OutputWriter.writeSingle(outputDir.toString, result, writeVisualization = true)

    assertEquals(write, Left("visualization requested but node states are missing"))
    assert(!Files.exists(outputDir.resolve("visualization.html")))

  test("writeSingle succeeds without visualization when node states are missing"):
    val outputDir = Files.createTempDirectory("eon-output-writer-no-viz")
    val result = sampleResult(tickNodeStates = None)

    val write = OutputWriter.writeSingle(outputDir.toString, result, writeVisualization = false)

    assertEquals(write, Right(()))
    assert(Files.exists(outputDir.resolve("summary.json")))
    assert(Files.exists(outputDir.resolve("timeseries.csv")))
    assert(!Files.exists(outputDir.resolve("visualization.html")))

  test("writeSingle writes visualization when node states are present"):
    val outputDir = Files.createTempDirectory("eon-output-writer-with-viz")
    val result = sampleResult(
      tickNodeStates = Some(
        Vector(
          TickNodeStates(0, Vector(eon.epidemic.core.NodeHealth.Infected, eon.epidemic.core.NodeHealth.Susceptible)),
          TickNodeStates(1, Vector(eon.epidemic.core.NodeHealth.Recovered, eon.epidemic.core.NodeHealth.Susceptible))
        )
      )
    )

    val write = OutputWriter.writeSingle(outputDir.toString, result, writeVisualization = true)

    assertEquals(write, Right(()))
    assert(Files.exists(outputDir.resolve("visualization.html")))

  private def sampleResult(tickNodeStates: Option[Vector[TickNodeStates]]): SimulationResult =
    val graph = Graph.fromEdges(2, Vector(Edge(0, 1, EdgeActivation(1, 0)))).toOption.get
    SimulationResult(
      summary = SimulationSummary(
        totalEverInfected = 1,
        epidemicDurationTicks = 1,
        peakInfected = 1,
        peakTick = 0,
        finalSusceptible = 1,
        finalInfected = 0,
        finalRecovered = 1
      ),
      timeseries =
        Vector(
          TickSnapshot(tick = 0, susceptible = 1, infected = 1, recovered = 0),
          TickSnapshot(tick = 1, susceptible = 1, infected = 0, recovered = 1)
        ),
      tickNodeStates = tickNodeStates,
      graph = graph
    )
