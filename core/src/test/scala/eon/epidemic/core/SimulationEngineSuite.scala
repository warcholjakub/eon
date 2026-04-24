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

  test("node state collection is optional"):
    val graph = Graph.fromEdges(nodeCount = 2, rawEdges = Vector.empty).toOption.get
    val baseConfig = SimulationConfig(
      infectionProbability = 0.0,
      recoveryProbability = 1.0,
      initialInfected = Set(0),
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
