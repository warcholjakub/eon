package eon.epidemic.cli

import eon.epidemic.core.BatchResult
import eon.epidemic.core.NodeHealth
import eon.epidemic.core.SimulationResult
import eon.epidemic.core.SimulationSummary
import eon.epidemic.core.TickNodeStates
import eon.epidemic.core.TickSnapshot

import java.nio.file.Files
import java.nio.file.Path

object OutputWriter:
  def writeSingle(
      outputDir: String,
      result: SimulationResult,
      writeVisualization: Boolean
  ): Either[String, Unit] =
    if writeVisualization && result.tickNodeStates.isEmpty then
      Left("visualization requested but node states are missing")
    else
      withOutputDir(outputDir): dir =>
        val summaryPath = dir.resolve("summary.json")
        val timeseriesPath = dir.resolve("timeseries.csv")
        val visualizationPath = dir.resolve("visualization.html")
        Files.writeString(summaryPath, renderSummaryJson(result.summary))
        Files.writeString(timeseriesPath, renderTimeseriesCsv(result.timeseries))
        if writeVisualization then
          val html = renderVisualizationHtml(result, result.tickNodeStates.getOrElse(Vector.empty))
          Files.writeString(visualizationPath, html)

  def writeBatch(outputDir: String, result: BatchResult): Either[String, Unit] =
    withOutputDir(outputDir): dir =>
      val summaryPath = dir.resolve("batch_summary.json")
      val runsPath = dir.resolve("batch_runs.csv")
      Files.writeString(summaryPath, renderBatchSummaryJson(result))
      Files.writeString(runsPath, renderBatchRunsCsv(result.summaries))

  private def withOutputDir(outputDir: String)(write: Path => Unit): Either[String, Unit] =
    try
      val dir = Path.of(outputDir)
      Files.createDirectories(dir)
      write(dir)
      Right(())
    catch
      case error: Exception => Left(s"failed to write output: ${error.getMessage}")

  private def renderSummaryJson(summary: SimulationSummary): String =
    s"""{
       |  "totalEverInfected": ${summary.totalEverInfected},
       |  "epidemicDurationTicks": ${summary.epidemicDurationTicks},
       |  "peakInfected": ${summary.peakInfected},
       |  "peakTick": ${summary.peakTick},
       |  "finalSusceptible": ${summary.finalSusceptible},
       |  "finalInfected": ${summary.finalInfected},
       |  "finalRecovered": ${summary.finalRecovered}
       |}
       |""".stripMargin

  private def renderTimeseriesCsv(series: Vector[TickSnapshot]): String =
    val header = "tick,susceptible,infected,recovered"
    val rows = series.map(snapshot =>
      s"${snapshot.tick},${snapshot.susceptible},${snapshot.infected},${snapshot.recovered}"
    )
    (header +: rows).mkString("\n") + "\n"

  private def renderBatchSummaryJson(result: BatchResult): String =
    val aggregate = result.aggregate
    s"""{
       |  "runs": ${result.runs},
       |  "aggregate": {
       |    "minTotalEverInfected": ${aggregate.minTotalEverInfected},
       |    "maxTotalEverInfected": ${aggregate.maxTotalEverInfected},
       |    "avgTotalEverInfected": ${aggregate.avgTotalEverInfected},
       |    "minDurationTicks": ${aggregate.minDurationTicks},
       |    "maxDurationTicks": ${aggregate.maxDurationTicks},
       |    "avgDurationTicks": ${aggregate.avgDurationTicks},
       |    "minPeakInfected": ${aggregate.minPeakInfected},
       |    "maxPeakInfected": ${aggregate.maxPeakInfected},
       |    "avgPeakInfected": ${aggregate.avgPeakInfected}
       |  }
       |}
       |""".stripMargin

  private def renderBatchRunsCsv(summaries: Vector[SimulationSummary]): String =
    val header =
      "run,totalEverInfected,epidemicDurationTicks,peakInfected,peakTick,finalSusceptible,finalInfected,finalRecovered"
    val rows = summaries.zipWithIndex.map: (summary, index) =>
      s"${index + 1},${summary.totalEverInfected},${summary.epidemicDurationTicks},${summary.peakInfected},${summary.peakTick},${summary.finalSusceptible},${summary.finalInfected},${summary.finalRecovered}"

    (header +: rows).mkString("\n") + "\n"

  private def renderVisualizationHtml(
      result: SimulationResult,
      nodeStates: Vector[TickNodeStates]
  ): String =
    val seriesJson =
      result.timeseries
        .map(snapshot =>
          s"{\"tick\":${snapshot.tick},\"susceptible\":${snapshot.susceptible},\"infected\":${snapshot.infected},\"recovered\":${snapshot.recovered}}"
        )
        .mkString("[", ",", "]")

    val edgesJson =
      result.graph.edges
        .map(edge =>
          s"{\"a\":${edge.a},\"b\":${edge.b},\"on\":${edge.activation.onTicks},\"off\":${edge.activation.offTicks},\"phase\":${edge.activation.phase}}"
        )
        .mkString("[", ",", "]")

    val statesJson =
      nodeStates
        .map(encodeTickStates)
        .map(encoded => s"\"$encoded\"")
        .mkString("[", ",", "]")

    s"""<!doctype html>
       |<html lang="en">
       |<head>
       |  <meta charset="utf-8" />
       |  <meta name="viewport" content="width=device-width, initial-scale=1" />
       |  <title>Epidemic Simulation Visualization</title>
       |  <style>
       |    :root {
       |      --bg: #f8f5ec;
       |      --panel: #fffef7;
       |      --ink: #1f1c18;
       |      --line: #d8cfbd;
       |      --sus: #1f7a8c;
       |      --inf: #d62828;
       |      --rec: #588157;
       |    }
       |    body {
       |      margin: 0;
       |      font-family: "Avenir Next", "Trebuchet MS", sans-serif;
       |      color: var(--ink);
       |      background: radial-gradient(circle at 10% 10%, #fffdf5 0%, var(--bg) 45%, #e8dfcf 100%);
       |    }
       |    .wrap {
       |      max-width: 1200px;
       |      margin: 0 auto;
       |      padding: 20px;
       |      display: grid;
       |      grid-template-columns: 1fr;
       |      gap: 16px;
       |    }
       |    .panel {
       |      background: var(--panel);
       |      border: 1px solid var(--line);
       |      border-radius: 12px;
       |      box-shadow: 0 10px 30px rgba(41, 35, 24, 0.08);
       |      padding: 16px;
       |    }
       |    .controls {
       |      display: grid;
       |      grid-template-columns: auto auto 1fr auto;
       |      gap: 12px;
       |      align-items: center;
       |    }
       |    button {
       |      border: 1px solid var(--line);
       |      border-radius: 8px;
       |      background: #fff;
       |      padding: 8px 12px;
       |      cursor: pointer;
       |      font-weight: 600;
       |    }
       |    canvas {
       |      width: 100%;
       |      height: auto;
       |      border: 1px solid var(--line);
       |      border-radius: 10px;
       |      background: #fff;
       |    }
       |    .legend {
       |      display: flex;
       |      gap: 14px;
       |      flex-wrap: wrap;
       |      margin-top: 8px;
       |    }
       |    .dot {
       |      width: 10px;
       |      height: 10px;
       |      border-radius: 50%;
       |      display: inline-block;
       |      margin-right: 6px;
       |    }
       |  </style>
       |</head>
       |<body>
       |  <div class="wrap">
       |    <div class="panel controls">
       |      <button id="play">Play</button>
       |      <button id="pause">Pause</button>
       |      <input id="tickRange" type="range" min="0" max="${result.timeseries.size - 1}" value="0" />
       |      <div id="tickLabel">tick: 0</div>
       |    </div>
       |
       |    <div class="panel">
       |      <canvas id="graph" width="1100" height="640"></canvas>
       |      <div class="legend">
       |        <span><span class="dot" style="background: var(--sus)"></span>Susceptible</span>
       |        <span><span class="dot" style="background: var(--inf)"></span>Infected</span>
       |        <span><span class="dot" style="background: var(--rec)"></span>Recovered</span>
       |        <span style="margin-left: 10px">Edge opacity follows on/off activity for selected tick</span>
       |      </div>
       |    </div>
       |
       |    <div class="panel">
       |      <canvas id="sir" width="1100" height="280"></canvas>
       |    </div>
       |  </div>
       |
       |  <script>
       |    const data = {
       |      nodeCount: ${result.graph.nodeCount},
       |      edges: $edgesJson,
       |      series: $seriesJson,
       |      states: $statesJson
       |    };
       |
       |    const graphCanvas = document.getElementById("graph");
       |    const graphCtx = graphCanvas.getContext("2d");
       |    const sirCanvas = document.getElementById("sir");
       |    const sirCtx = sirCanvas.getContext("2d");
       |    const tickRange = document.getElementById("tickRange");
       |    const tickLabel = document.getElementById("tickLabel");
       |    const playButton = document.getElementById("play");
       |    const pauseButton = document.getElementById("pause");
       |
       |    const palette = { S: "#1f7a8c", I: "#d62828", R: "#588157" };
       |
       |    const positions = Array.from({ length: data.nodeCount }, (_, idx) => {
       |      const angle = (Math.PI * 2 * idx) / Math.max(1, data.nodeCount);
       |      const radius = Math.min(graphCanvas.width, graphCanvas.height) * 0.38;
       |      return {
       |        x: graphCanvas.width / 2 + Math.cos(angle) * radius,
       |        y: graphCanvas.height / 2 + Math.sin(angle) * radius
       |      };
       |    });
       |
       |    const isEdgeActive = (edge, tick) => {
       |      if (edge.on === 0) return false;
       |      const cycle = edge.on + edge.off;
       |      const position = ((tick + edge.phase) % cycle + cycle) % cycle;
       |      return position < edge.on;
       |    };
       |
       |    const renderGraph = tick => {
       |      graphCtx.clearRect(0, 0, graphCanvas.width, graphCanvas.height);
       |      graphCtx.lineWidth = 1;
       |
       |      for (const edge of data.edges) {
       |        const active = isEdgeActive(edge, tick);
       |        graphCtx.strokeStyle = active ? "rgba(43,43,43,0.28)" : "rgba(43,43,43,0.08)";
       |        const from = positions[edge.a];
       |        const to = positions[edge.b];
       |        graphCtx.beginPath();
       |        graphCtx.moveTo(from.x, from.y);
       |        graphCtx.lineTo(to.x, to.y);
       |        graphCtx.stroke();
       |      }
       |
       |      const statusLine = data.states[tick] || "";
       |      for (let node = 0; node < positions.length; node += 1) {
       |        const status = statusLine.charAt(node) || "S";
       |        const p = positions[node];
       |        graphCtx.fillStyle = palette[status] || palette.S;
       |        graphCtx.beginPath();
       |        graphCtx.arc(p.x, p.y, 4.5, 0, Math.PI * 2);
       |        graphCtx.fill();
       |      }
       |    };
       |
       |    const renderSirChart = tick => {
       |      sirCtx.clearRect(0, 0, sirCanvas.width, sirCanvas.height);
       |      const w = sirCanvas.width;
       |      const h = sirCanvas.height;
       |      const pad = 24;
       |      const maxY = Math.max(1, ...data.series.map(s => Math.max(s.susceptible, s.infected, s.recovered)));
       |      const maxX = Math.max(1, data.series.length - 1);
       |
       |      const drawLine = (key, color) => {
       |        sirCtx.strokeStyle = color;
       |        sirCtx.lineWidth = 2;
       |        sirCtx.beginPath();
       |        data.series.forEach((point, i) => {
       |          const x = pad + (i / maxX) * (w - pad * 2);
       |          const y = h - pad - (point[key] / maxY) * (h - pad * 2);
       |          if (i === 0) sirCtx.moveTo(x, y);
       |          else sirCtx.lineTo(x, y);
       |        });
       |        sirCtx.stroke();
       |      };
       |
       |      sirCtx.strokeStyle = "rgba(0,0,0,0.2)";
       |      sirCtx.strokeRect(pad, pad, w - pad * 2, h - pad * 2);
       |
       |      drawLine("susceptible", palette.S);
       |      drawLine("infected", palette.I);
       |      drawLine("recovered", palette.R);
       |
       |      const markerX = pad + (tick / maxX) * (w - pad * 2);
       |      sirCtx.strokeStyle = "rgba(0,0,0,0.35)";
       |      sirCtx.beginPath();
       |      sirCtx.moveTo(markerX, pad);
       |      sirCtx.lineTo(markerX, h - pad);
       |      sirCtx.stroke();
       |    };
       |
       |    let currentTick = 0;
       |    let timer = null;
       |
       |    const render = () => {
       |      tickLabel.textContent = `tick: ${'$'}{currentTick}`;
       |      tickRange.value = currentTick;
       |      renderGraph(currentTick);
       |      renderSirChart(currentTick);
       |    };
       |
       |    tickRange.addEventListener("input", event => {
       |      currentTick = Number(event.target.value);
       |      render();
       |    });
       |
       |    playButton.addEventListener("click", () => {
       |      if (timer !== null) return;
       |      timer = setInterval(() => {
       |        currentTick = (currentTick + 1) % data.series.length;
       |        render();
       |      }, 140);
       |    });
       |
       |    pauseButton.addEventListener("click", () => {
       |      if (timer !== null) {
       |        clearInterval(timer);
       |        timer = null;
       |      }
       |    });
       |
       |    render();
       |  </script>
       |</body>
       |</html>
       |""".stripMargin

  private def encodeTickStates(tickStates: TickNodeStates): String =
    tickStates.nodeStates.map(nodeStateToChar).mkString

  private def nodeStateToChar(status: NodeHealth): Char =
    status match
      case NodeHealth.Susceptible => 'S'
      case NodeHealth.Infected    => 'I'
      case NodeHealth.Recovered   => 'R'
