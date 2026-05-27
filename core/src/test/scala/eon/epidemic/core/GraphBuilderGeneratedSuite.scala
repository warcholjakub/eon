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

  test("three-clusters-hub builds three complete clusters connected only through hub"):
    val spec = GraphSpec.Generated(
      shape = GraphShape.ThreeClustersHub,
      nodeCount = 25,
      edgeActivation = EdgeActivation(1, 0),
      erdosProbability = 0.0,
      ringDegree = 0,
      seed = 42
    )

    val graph = GraphBuilder.build(spec).toOption.get
    val clusters = Vector((1 to 8).toSet, (9 to 16).toSet, (17 to 24).toSet)
    val hubEdges = graph.edges.filter(edge => edge.a == 0 || edge.b == 0)
    val clusterEdges = graph.edges.filter(edge => edge.a != 0 && edge.b != 0)
    val edgesBetweenClusters = graph.edges.filter: edge =>
      edge.a != 0 && edge.b != 0 && !clusters.exists(cluster => cluster.contains(edge.a) && cluster.contains(edge.b))

    assertEquals(hubEdges.map(_.endpoints).toSet, Set((0, 1), (0, 9), (0, 17)))
    assert(clusterEdges.forall(edge => edge.activation.onTicks == 1 && edge.activation.offTicks == 2))
    assert(hubEdges.forall(_.activation == EdgeActivation(1, 0)))
    assertEquals(edgesBetweenClusters, Vector.empty)
    assertEquals(graph.edges.size, 3 * 28 + 3)

  test("three-clusters-hub requires three equally sized clusters"):
    val spec = GraphSpec.Generated(
      shape = GraphShape.ThreeClustersHub,
      nodeCount = 24,
      edgeActivation = EdgeActivation(1, 0),
      erdosProbability = 0.0,
      ringDegree = 0,
      seed = 42
    )

    val error = GraphBuilder.build(spec).swap.toOption.get
    assert(error.contains("positive multiple of 3"))
