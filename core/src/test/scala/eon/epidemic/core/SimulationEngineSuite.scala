package eon.epidemic.core

import munit.FunSuite

class SimulationEngineSuite extends FunSuite:
  test("EdgeActivation follows on/off cycle"):
    val activation = EdgeActivation(onTicks = 2, offTicks = 1, phase = 0)

    assert(activation.isActive(0))
    assert(activation.isActive(1))
    assert(!activation.isActive(2))
    assert(activation.isActive(3))

  test("deterministic spread with probability 1 on active edge"):
    val graph = Graph.fromEdges(
      nodeCount = 2,
      rawEdges = Vector(Edge(0, 1, EdgeActivation(onTicks = 1, offTicks = 0)))
    ).toOption.get

    val config = SimulationConfig(
      infectionProbability = 1.0,
      recoveryProbability = 0.0,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 3),
      seed = 123,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.Ring,
        nodeCount = 2,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 1,
        seed = 123
      ),
      collectNodeStates = false
    )

    val result = SimulationEngine.runWithGraph(config, graph).toOption.get
    val summary = result.summary

    assertEquals(summary.totalEverInfected, 2)
    assertEquals(summary.peakInfected, 2)
    assertEquals(summary.peakTick, 1)

  test("simulation stops at max ticks when infection persists"):
    val graph = Graph.fromEdges(nodeCount = 1, rawEdges = Vector.empty).toOption.get

    val config = SimulationConfig(
      infectionProbability = 0.0,
      recoveryProbability = 0.0,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 5),
      seed = 1,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 1,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 0,
        seed = 1
      ),
      collectNodeStates = false
    )

    val result = SimulationEngine.runWithGraph(config, graph).toOption.get
    assertEquals(result.summary.epidemicDurationTicks, 5)

  test("batch run returns expected number of summaries"):
    val config = SimulationConfig(
      infectionProbability = 0.1,
      recoveryProbability = 0.2,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 10),
      seed = 42,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 20,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.1,
        ringDegree = 2,
        seed = 42
      ),
      collectNodeStates = false
    )

    val result = BatchRunner.run(config, runs = 4).toOption.get
    assertEquals(result.runs, 4)
    assertEquals(result.summaries.size, 4)

  test("parameter sweep evaluates each infection and recovery combination"):
    val config = SimulationConfig(
      infectionProbability = 0.1,
      recoveryProbability = 0.1,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 2),
      seed = 42,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 2,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 0,
        seed = 42
      ),
      collectNodeStates = false
    )

    val result =
      ParameterSweepRunner
        .run(
          config,
          minProbability = 0.05,
          maxProbability = 0.10,
          probabilityStep = 0.05,
          runsPerPair = 2,
          edgeActivations = Vector(EdgeActivation(1, 0), EdgeActivation(1, 1))
        )
        .toOption
        .get

    assertEquals(result.runsPerPair, 2)
    assertEquals(result.rows.size, 8)
    assert(result.rows.forall(_.summaries.size == 2))
    assertEquals(
      result.rows.map(row => (row.infectionProbability, row.recoveryProbability)).toSet,
      Set((0.05, 0.05), (0.05, 0.1), (0.1, 0.05), (0.1, 0.1))
    )
    assertEquals(result.rows.map(_.edgeActivation).toSet, Set(EdgeActivation(1, 0), EdgeActivation(1, 1)))

    val completed =
      ParameterSweepRunner
        .runStreaming(
          config,
          minProbability = 0.05,
          maxProbability = 0.10,
          probabilityStep = 0.05,
          runsPerCombination = 2,
          edgeActivations = Vector(EdgeActivation(1, 0), EdgeActivation(1, 1)),
          persistRow = (_, _) => Right(())
        )
        .toOption
        .get

    assertEquals(completed, 8)

  test("node state collection is optional"):
    val graph = Graph.fromEdges(nodeCount = 2, rawEdges = Vector.empty).toOption.get
    val baseConfig = SimulationConfig(
      infectionProbability = 0.0,
      recoveryProbability = 1.0,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 3),
      seed = 1,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 2,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 0,
        seed = 1
      ),
      collectNodeStates = false
    )

    val withoutStates = SimulationEngine.runWithGraph(baseConfig, graph).toOption.get
    assertEquals(withoutStates.tickNodeStates, None)

    val withStates =
      SimulationEngine.runWithGraph(baseConfig.copy(collectNodeStates = true), graph).toOption.get
    assert(withStates.tickNodeStates.exists(_.nonEmpty))

  test("same seed yields identical simulation result"):
    val graph = Graph.fromEdges(
      nodeCount = 6,
      rawEdges = Vector(
        Edge(0, 1, EdgeActivation(1, 0)),
        Edge(1, 2, EdgeActivation(1, 0)),
        Edge(2, 3, EdgeActivation(1, 0)),
        Edge(3, 4, EdgeActivation(1, 0)),
        Edge(4, 5, EdgeActivation(1, 0))
      )
    ).toOption.get

    val config = SimulationConfig(
      infectionProbability = 0.45,
      recoveryProbability = 0.25,
      initialInfected = Set(0, 2),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 30),
      seed = 777,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 6,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 2,
        seed = 777
      ),
      collectNodeStates = true
    )

    val first = SimulationEngine.runWithGraph(config, graph).toOption.get
    val second = SimulationEngine.runWithGraph(config, graph).toOption.get

    assertEquals(first.summary, second.summary)
    assertEquals(first.timeseries, second.timeseries)
    assertEquals(first.tickNodeStates, second.tickNodeStates)

  test("BatchRunner configForRun updates simulation and generated graph seeds"):
    val base = SimulationConfig(
      infectionProbability = 0.2,
      recoveryProbability = 0.1,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 20),
      seed = 100,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 10,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.2,
        ringDegree = 2,
        seed = 7
      ),
      collectNodeStates = false
    )

    val run = BatchRunner.configForRun(base, runIndex = 3)

    assertEquals(run.seed, 103L)
    val generated = run.graphSpec.asInstanceOf[GraphSpec.Generated]
    assertEquals(generated.seed, 103L)

  test("BatchRunner configForRun keeps file graph spec while updating simulation seed"):
    val base = SimulationConfig(
      infectionProbability = 0.2,
      recoveryProbability = 0.1,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 20),
      seed = 50,
      graphSpec = GraphSpec.FromFile(
        path = "data/graph.csv",
        defaultActivation = EdgeActivation(1, 0),
        explicitNodeCount = Some(5)
      ),
      collectNodeStates = false
    )

    val run = BatchRunner.configForRun(base, runIndex = 2)

    assertEquals(run.seed, 52L)
    assertEquals(run.graphSpec, base.graphSpec)

  test("trackedNodes records first infection tick"):
    // Linear chain 0-1-2. Infection starts at 0, β=1, γ=0.
    // Node 1 gets infected at tick 1, node 2 at tick 2.
    val graph = Graph.fromEdges(
      nodeCount = 3,
      rawEdges = Vector(
        Edge(0, 1, EdgeActivation(onTicks = 1, offTicks = 0)),
        Edge(1, 2, EdgeActivation(onTicks = 1, offTicks = 0))
      )
    ).toOption.get

    val config = SimulationConfig(
      infectionProbability = 1.0,
      recoveryProbability = 0.0,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = false, maxTicks = 5),
      seed = 1,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 3,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 0,
        seed = 1
      ),
      collectNodeStates = false,
      trackedNodes = Set(1, 2)
    )

    val summary = SimulationEngine.runWithGraph(config, graph).toOption.get.summary
    assertEquals(summary.trackedNodeFirstInfection.get(1), Some(1))
    assertEquals(summary.trackedNodeFirstInfection.get(2), Some(2))

  test("containedToInitialGroups is true when infection stays in starting cluster"):
    // 4-node graph: cluster A = {0,1}, cluster B = {2,3}. No edges between clusters.
    // Infection starts at 0, β=1. Should never reach cluster B.
    val graph = Graph.fromEdges(
      nodeCount = 4,
      rawEdges = Vector(Edge(0, 1, EdgeActivation(onTicks = 1, offTicks = 0)))
    ).toOption.get

    val config = SimulationConfig(
      infectionProbability = 1.0,
      recoveryProbability = 0.0,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = false, maxTicks = 5),
      seed = 1,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 4,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 0,
        seed = 1
      ),
      collectNodeStates = false,
      nodeGroups = Map(0 -> 1, 1 -> 1, 2 -> 2, 3 -> 2)
    )

    val summary = SimulationEngine.runWithGraph(config, graph).toOption.get.summary
    assertEquals(summary.containedToInitialGroups, Some(true))

  test("containedToInitialGroups is false when infection crosses cluster boundary"):
    // 3-node graph: cluster A = {0}, cluster B = {1,2}. Edge 0-1 bridges clusters.
    val graph = Graph.fromEdges(
      nodeCount = 3,
      rawEdges = Vector(
        Edge(0, 1, EdgeActivation(onTicks = 1, offTicks = 0)),
        Edge(1, 2, EdgeActivation(onTicks = 1, offTicks = 0))
      )
    ).toOption.get

    val config = SimulationConfig(
      infectionProbability = 1.0,
      recoveryProbability = 0.0,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = false, maxTicks = 5),
      seed = 1,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 3,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 0,
        seed = 1
      ),
      collectNodeStates = false,
      nodeGroups = Map(0 -> 1, 1 -> 2, 2 -> 2)
    )

    val summary = SimulationEngine.runWithGraph(config, graph).toOption.get.summary
    assertEquals(summary.containedToInitialGroups, Some(false))

  test("containedToInitialGroups is None when nodeGroups is empty"):
    val graph = Graph.fromEdges(nodeCount = 2, rawEdges = Vector.empty).toOption.get

    val config = SimulationConfig(
      infectionProbability = 0.0,
      recoveryProbability = 1.0,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIR,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 3),
      seed = 1,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 2,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 0,
        seed = 1
      ),
      collectNodeStates = false
    )

    val summary = SimulationEngine.runWithGraph(config, graph).toOption.get.summary
    assertEquals(summary.containedToInitialGroups, None)

  test("SIS model moves recovered nodes back to susceptible"):
    val graph = Graph.fromEdges(nodeCount = 1, rawEdges = Vector.empty).toOption.get

    val config = SimulationConfig(
      infectionProbability = 0.0,
      recoveryProbability = 1.0,
      initialInfected = Set(0),
      diseaseModel = DiseaseModel.SIS,
      stopCondition = StopCondition(stopWhenNoInfected = true, maxTicks = 3),
      seed = 10,
      graphSpec = GraphSpec.Generated(
        shape = GraphShape.ErdosRenyi,
        nodeCount = 1,
        edgeActivation = EdgeActivation(1, 0),
        erdosProbability = 0.0,
        ringDegree = 0,
        seed = 10
      ),
      collectNodeStates = true
    )

    val result = SimulationEngine.runWithGraph(config, graph).toOption.get
    assertEquals(result.summary.finalSusceptible, 1)
    assertEquals(result.summary.finalRecovered, 0)
