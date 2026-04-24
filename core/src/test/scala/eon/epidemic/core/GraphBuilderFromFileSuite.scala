package eon.epidemic.core

import munit.FunSuite

import java.nio.file.Files
import java.nio.file.Path

class GraphBuilderFromFileSuite extends FunSuite:
  private val defaultActivation = EdgeActivation(onTicks = 3, offTicks = 2, phase = 1)

  test("from-file parses 2-column edges and ignores comments/blank lines"):
    val path = writeTemp(
      """# comment
        |
        |0,1
        |2,3
        |""".stripMargin
    )

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val graph = GraphBuilder.build(spec).toOption.get

    assertEquals(graph.nodeCount, 4)
    assertEquals(graph.edges.size, 2)
    assertEquals(graph.edges.map(_.endpoints).toSet, Set((0, 1), (2, 3)))
    assert(graph.edges.forall(_.activation == defaultActivation))

  test("from-file parses 5-column edges with explicit activation"):
    val path = writeTemp("0,1,5,2,3\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)
    val graph = GraphBuilder.build(spec).toOption.get
    val edge = graph.edges.head

    assertEquals(graph.nodeCount, 2)
    assertEquals(edge.activation, EdgeActivation(onTicks = 5, offTicks = 2, phase = 3))

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

  test("from-file currently throws when activation on+off is zero"):
    val path = writeTemp("0,1,0,0,0\n")

    val spec = GraphSpec.FromFile(path.toString, defaultActivation, explicitNodeCount = None)

    intercept[IllegalArgumentException]:
      GraphBuilder.build(spec)

  private def writeTemp(content: String): Path =
    val file = Files.createTempFile("graph-builder-from-file", ".csv")
    Files.writeString(file, content)
    file
