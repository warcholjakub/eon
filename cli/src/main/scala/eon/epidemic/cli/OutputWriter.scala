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
       |    svg.viewport {
       |      width: 100%;
       |      height: auto;
       |      border: 1px solid var(--line);
       |      border-radius: 10px;
       |      background: #fff;
       |      display: block;
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
       |      <svg id="graph" class="viewport" viewBox="0 0 1100 640" aria-label="Graph visualization"></svg>
       |      <div class="legend">
       |        <span><span class="dot" style="background: var(--sus)"></span>Susceptible</span>
       |        <span><span class="dot" style="background: var(--inf)"></span>Infected</span>
       |        <span><span class="dot" style="background: var(--rec)"></span>Recovered</span>
       |        <span style="margin-left: 10px">Edge opacity follows on/off activity for selected tick</span>
       |      </div>
       |    </div>
       |
       |    <div class="panel">
       |      <svg id="sir" class="viewport" viewBox="0 0 1100 280" aria-label="SIR chart"></svg>
       |    </div>
       |  </div>
       |
       |  <script>
       |    const data = {
       |      nodeCount: ${result.graph.nodeCount},
       |      edges: $edgesJson,
       |      series: $seriesJson,
       |      states: $statesJson,
       |      layoutHint: "${result.layoutHint.getOrElse("")}"
       |    };
       |
       |    const graphWidth = 1100;
       |    const graphHeight = 640;
       |    const chartWidth = 1100;
       |    const chartHeight = 280;
       |
       |    const ns = "http://www.w3.org/2000/svg";
       |    const graphSvg = document.getElementById("graph");
       |    const sirSvg = document.getElementById("sir");
       |    const tickRange = document.getElementById("tickRange");
       |    const tickLabel = document.getElementById("tickLabel");
       |    const playButton = document.getElementById("play");
       |    const pauseButton = document.getElementById("pause");
       |
       |    const palette = { S: "#1f7a8c", I: "#d62828", R: "#588157" };
       |
       |    const make = (name, attrs) => {
       |      const el = document.createElementNS(ns, name);
       |      for (const [key, value] of Object.entries(attrs)) {
       |        el.setAttribute(key, String(value));
       |      }
       |      return el;
       |    };
       |
       |    const clear = svg => {
       |      while (svg.firstChild) {
       |        svg.removeChild(svg.firstChild);
       |      }
       |    };
       |
       |    const positions = (() => {
       |      if (data.layoutHint === "clustered-vpn") {
       |        const mid = Math.floor(data.nodeCount / 2);
       |        const leftNodes = mid;
       |        const rightNodes = data.nodeCount - mid;
       |        const radius = Math.min(graphWidth, graphHeight) * 0.22;
       |        const leftCenterX = graphWidth * 0.3;
       |        const rightCenterX = graphWidth * 0.7;
       |        const centerY = graphHeight * 0.52;
       |
       |        const cluster = (count, centerX) =>
       |          Array.from({ length: count }, (_, idx) => {
       |            const angle = (Math.PI * 2 * idx) / Math.max(1, count);
       |            return {
       |              x: centerX + Math.cos(angle) * radius,
       |              y: centerY + Math.sin(angle) * radius
       |            };
       |          });
       |
       |        return [...cluster(leftNodes, leftCenterX), ...cluster(rightNodes, rightCenterX)];
       |      }
       |
       |      return Array.from({ length: data.nodeCount }, (_, idx) => {
       |        const angle = (Math.PI * 2 * idx) / Math.max(1, data.nodeCount);
       |        const radius = Math.min(graphWidth, graphHeight) * 0.38;
       |        return {
       |          x: graphWidth / 2 + Math.cos(angle) * radius,
       |          y: graphHeight / 2 + Math.sin(angle) * radius
       |        };
       |      });
       |    })();
       |
       |    const isEdgeActive = (edge, tick) => {
       |      if (edge.on === 0) return false;
       |      const cycle = edge.on + edge.off;
       |      const position = ((tick + edge.phase) % cycle + cycle) % cycle;
       |      return position < edge.on;
       |    };
       |
       |    const renderGraph = tick => {
       |      clear(graphSvg);
       |
       |      const edgeLayer = make("g", { "stroke-width": 1, fill: "none" });
       |      for (const edge of data.edges) {
       |        const active = isEdgeActive(edge, tick);
       |        const from = positions[edge.a];
       |        const to = positions[edge.b];
       |        edgeLayer.appendChild(
       |          make("line", {
       |            x1: from.x,
       |            y1: from.y,
       |            x2: to.x,
       |            y2: to.y,
       |            stroke: active ? "rgba(43,43,43,0.28)" : "rgba(43,43,43,0.08)"
       |          })
       |        );
       |      }
       |      graphSvg.appendChild(edgeLayer);
       |
       |      const nodeLayer = make("g", {});
       |      const statusLine = data.states[tick] || "";
       |      for (let node = 0; node < positions.length; node += 1) {
       |        const status = statusLine.charAt(node) || "S";
       |        const p = positions[node];
       |        nodeLayer.appendChild(
       |          make("circle", {
       |            cx: p.x,
       |            cy: p.y,
       |            r: 4.5,
       |            fill: palette[status] || palette.S
       |          })
       |        );
       |      }
       |      graphSvg.appendChild(nodeLayer);
       |    };
       |
       |    const buildPath = (key, maxX, maxY, pad) => {
       |      return data.series
       |        .map((point, i) => {
       |          const x = pad + (i / maxX) * (chartWidth - pad * 2);
       |          const y = chartHeight - pad - (point[key] / maxY) * (chartHeight - pad * 2);
       |          return (i === 0 ? "M" : "L") + x.toFixed(2) + " " + y.toFixed(2);
       |        })
       |        .join(" ");
       |    };
       |
       |    const renderSirChart = tick => {
       |      clear(sirSvg);
       |      const pad = 24;
       |      const maxY = Math.max(1, ...data.series.map(s => Math.max(s.susceptible, s.infected, s.recovered)));
       |      const maxX = Math.max(1, data.series.length - 1);
       |
       |      sirSvg.appendChild(
       |        make("rect", {
       |          x: pad,
       |          y: pad,
       |          width: chartWidth - pad * 2,
       |          height: chartHeight - pad * 2,
       |          fill: "none",
       |          stroke: "rgba(0,0,0,0.2)"
       |        })
       |      );
       |
       |      sirSvg.appendChild(
       |        make("path", {
       |          d: buildPath("susceptible", maxX, maxY, pad),
       |          fill: "none",
       |          stroke: palette.S,
       |          "stroke-width": 2
       |        })
       |      );
       |      sirSvg.appendChild(
       |        make("path", {
       |          d: buildPath("infected", maxX, maxY, pad),
       |          fill: "none",
       |          stroke: palette.I,
       |          "stroke-width": 2
       |        })
       |      );
       |      sirSvg.appendChild(
       |        make("path", {
       |          d: buildPath("recovered", maxX, maxY, pad),
       |          fill: "none",
       |          stroke: palette.R,
       |          "stroke-width": 2
       |        })
       |      );
       |
       |      const markerX = pad + (tick / maxX) * (chartWidth - pad * 2);
       |      sirSvg.appendChild(
       |        make("line", {
       |          x1: markerX,
       |          y1: pad,
       |          x2: markerX,
       |          y2: chartHeight - pad,
       |          stroke: "rgba(0,0,0,0.35)",
       |          "stroke-width": 1
       |        })
       |      );
       |    };
       |
       |    let currentTick = 0;
       |    let timer = null;
       |
       |    const render = () => {
       |      tickLabel.textContent = "tick: " + currentTick;
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
