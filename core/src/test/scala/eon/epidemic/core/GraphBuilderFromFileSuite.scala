package eon.epidemic.core

import munit.FunSuite

import java.nio.file.Files
import java.nio.file.Path

class GraphBuilderFromFileSuite extends FunSuite:
  private val defaultActivation = EdgeActivation(onTicks = 3, offTicks = 2, phase = 1)

  test("from-file parses 2-column edges and independently randomizes activation phase"):
    val path = writeTemp(
      """# comment
        |
        |0,1
        |2,3
        |4,5
        |6,7
        |""".stripMargin
    )

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None, seed = 42)
    val graph = GraphBuilder.build(spec).toOption.get
    val repeated = GraphBuilder.build(spec).toOption.get

    assertEquals(graph.nodeCount, 8)
    assertEquals(graph.edges.size, 4)
    assertEquals(graph.edges.map(_.endpoints).toSet, Set((0, 1), (2, 3), (4, 5), (6, 7)))
    assert(graph.edges.forall(edge => edge.activation.onTicks == 3 && edge.activation.offTicks == 2))
    assert(graph.edges.map(_.activation.phase).distinct.size > 1)
    assertEquals(graph, repeated)

  test("from-file randomizes phase of 5-column activation while retaining its frequency"):
    val path = writeTemp("0,1,5,2,3\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None, seed = 42)
    val graph = GraphBuilder.build(spec).toOption.get
    val edge = graph.edges.head

    assertEquals(graph.nodeCount, 2)
    assertEquals(edge.activation.onTicks, 5)
    assertEquals(edge.activation.offTicks, 2)
    assert(edge.activation.phase >= 0 && edge.activation.phase < 7)

  test("from-file uses seed to vary activation phases between runs"):
    val path = writeTemp((0 until 8).map(node => s"$node,${node + 1}").mkString("\n") + "\n")
    val first = GraphBuilder.build(GraphSpec.FromFile(path.toString, defaultActivation, None, seed = 42)).toOption.get
    val second = GraphBuilder.build(GraphSpec.FromFile(path.toString, defaultActivation, None, seed = 43)).toOption.get

    assertNotEquals(first.edges.map(_.activation.phase), second.edges.map(_.activation.phase))

  test("from-file uses explicit node count when provided"):
    val path = writeTemp("0,1\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = Some(10))
    val graph = GraphBuilder.build(spec).toOption.get

    assertEquals(graph.nodeCount, 10)
    assertEquals(graph.edges.map(_.endpoints).toSet, Set((0, 1)))

  test("from-file empty file fails without explicit node count"):
    val path = writeTemp("\n# only comments\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assertEquals(error, "nodeCount must be > 0")

  test("from-file empty file succeeds with explicit node count"):
    val path = writeTemp("\n# only comments\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = Some(3))
    val graph = GraphBuilder.build(spec).toOption.get

    assertEquals(graph.nodeCount, 3)
    assertEquals(graph.edges, Vector.empty)

  test("from-file rejects invalid column count"):
    val path = writeTemp("0,1,2\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assertEquals(error, "invalid edge format at line 1")

  test("from-file rejects invalid source node"):
    val path = writeTemp("x,1\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assertEquals(error, "invalid source node at line 1")

  test("from-file rejects invalid target node"):
    val path = writeTemp("0,y\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assertEquals(error, "invalid target node at line 1")

  test("from-file rejects invalid onTicks"):
    val path = writeTemp("0,1,oops,1,0\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assertEquals(error, "invalid onTicks at line 1")

  test("from-file rejects invalid offTicks"):
    val path = writeTemp("0,1,1,oops,0\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assertEquals(error, "invalid offTicks at line 1")

  test("from-file rejects invalid phase"):
    val path = writeTemp("0,1,1,2,oops\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assertEquals(error, "invalid phase at line 1")

  test("from-file rejects invalid edge activation instead of throwing"):
    val path = writeTemp("0,1,0,0,0\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assert(error.startsWith("invalid edge activation at line 1:"))

  test("from-file rejects negative endpoint instead of throwing"):
    val path = writeTemp("-1,2\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assert(error.startsWith("invalid edge at line 1:"))

  test("from-file rejects self-loop instead of throwing"):
    val path = writeTemp("3,3\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val error = GraphBuilder.build(spec).swap.toOption.get

    assert(error.startsWith("invalid edge at line 1:"))

  private def writeTemp(content: String): Path =
    val file = Files.createTempFile("graph-builder-from-file", ".csv")
    Files.writeString(file, content)
    file
