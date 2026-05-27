package eon.epidemic.cli

import eon.epidemic.core.AggregateMetrics
import eon.epidemic.core.Edge
import eon.epidemic.core.EdgeActivation
import eon.epidemic.core.Graph
import eon.epidemic.core.ParameterSweepResult
import eon.epidemic.core.ParameterSweepRow
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

  test("writeSweep writes one CSV row per parameter pair"):
    val outputDir = Files.createTempDirectory("eon-output-writer-sweep")
    val aggregate = AggregateMetrics(1, 2, 1.5, 1, 3, 2.0, 1, 2, 1.5)
    val result =
      ParameterSweepResult(
        runsPerPair = 1000,
        rows = Vector(
          ParameterSweepRow(0.05, 0.05, aggregate),
          ParameterSweepRow(0.05, 0.1, aggregate)
        )
      )

    val write = OutputWriter.writeSweep(outputDir.toString, result)
    val output = Files.readString(outputDir.resolve("sweep_summary.csv"))

    assertEquals(write, Right(()))
    assert(output.contains("infectionProbability,recoveryProbability,runs"))
    assert(output.contains("0.05,0.1,1000"))

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
      graph = graph,
      layoutHint = None
    )
