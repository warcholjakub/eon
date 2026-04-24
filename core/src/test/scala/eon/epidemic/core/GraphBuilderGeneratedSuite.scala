package eon.epidemic.core

import munit.FunSuite

class GraphBuilderGeneratedSuite extends FunSuite:
  test("clustered-vpn builds two clusters connected by configured vpn links"):
    val spec = GraphSpec.Generated(
      shape = GraphShape.ClusteredVpn,
      nodeCount = 10,
      edgeActivation = EdgeActivation(1, 0),
      erdosProbability = 0.0,
      ringDegree = 2,
      seed = 11
    )

    val graph = GraphBuilder.build(spec).toOption.get
    val mid = spec.nodeCount / 2

    val interClusterEdges = graph.edges.filter: edge =>
      (edge.a < mid && edge.b >= mid) || (edge.a >= mid && edge.b < mid)

    assertEquals(interClusterEdges.size, 2)

  test("clustered-vpn rejects invalid vpn link count"):
    val spec = GraphSpec.Generated(
      shape = GraphShape.ClusteredVpn,
      nodeCount = 8,
      edgeActivation = EdgeActivation(1, 0),
      erdosProbability = 0.1,
      ringDegree = 5,
      seed = 1
    )

    val error = GraphBuilder.build(spec).swap.toOption.get
    assert(error.contains("ringDegree (vpn links)"))

  test("clustered-vpn requires minimum node count"):
    val spec = GraphSpec.Generated(
      shape = GraphShape.ClusteredVpn,
      nodeCount = 3,
      edgeActivation = EdgeActivation(1, 0),
      erdosProbability = 0.1,
      ringDegree = 1,
      seed = 1
    )

    val error = GraphBuilder.build(spec).swap.toOption.get
    assertEquals(error, "nodeCount must be >= 4 for clustered-vpn graph")
