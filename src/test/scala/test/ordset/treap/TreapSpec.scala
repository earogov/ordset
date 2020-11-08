package test.ordset.treap

import ordset.domain.{Domain, DomainOps}
import ordset.instances
import ordset.treap.eval.NodeVisitStack
import ordset.treap.reduce.{CallTrace, ContextExtract, TreeMerge, TreeSlice}
import ordset.treap.traverse.{DepthFirst, KeySearch}
import ordset.treap.{Reduce, TraverseVisit, Treap}
import org.scalatest.funspec.AnyFunSpec

class TreapSpec extends AnyFunSpec {

  import instances.Int._

  type Dom = Domain[Int]

  implicit val ops: DomainOps[Int, Dom] = DomainOps.defaultDomainOps

  //   9  -                               A
  //   8  -                      ↙          ↘
  //   7  -             B                     ↘
  //   6  -       ↙           ↘                 C
  //   5  -    ↙                    E             ↘
  //   4  - D                      ↙                ↘
  //   3  -    ↘                 ↙                    G
  //   2  -       F            ↙
  //   1  -                   H
  //        |-----|-----|-----|-----|-----|-----|-----|
  //        1     2     3     4     5     6     7     8

  it("should reduce tree") {

    val leafF = Treap.Leaf[Int, Dom, String](2, 2, "F")
    val leafH = Treap.Leaf[Int, Dom, String](4, 1, "H")
    val nodeD = Treap.NodeWithRightOnly[Int, Dom, String](leafF, 1, 4, "D")
    val nodeE = Treap.NodeWithLeftOnly[Int, Dom, String](leafH, 5, 5, "E")
    val nodeB = Treap.NodeWithLeftRight[Int, Dom, String](nodeD, nodeE, 3, 7, "B")
    val leafG = Treap.Leaf[Int, Dom, String](8, 3, "G")
    val nodeC = Treap.NodeWithRightOnly[Int, Dom, String](leafG, 7, 6, "C")
    val nodeA = Treap.NodeWithLeftRight[Int, Dom, String](nodeB, nodeC, 6, 9, "A")

    println("")
    println("DepthFirst traverse with NodeVisitStack context")

    Reduce.before(
      nodeA,
      new NodeVisitStack.Context[Int, Dom, String](TraverseVisit.None, Nil),
      ()
    )(
      DepthFirst.nonEmpty(
        DepthFirst.leftFirstNavigate,
        NodeVisitStack.of(nodeA)
      ),
      CallTrace.toConsole
    )

//    Reduce.before(
//      nodeA,
//      new NodeIntervalStack.Context[Int, Dom, String](ops.interval.universal, TraverseVisit.None, Nil),
//      ()
//    )(
//      DepthFirst.withEmpty(
//        DepthFirst.leftFirstNavigate,
//        NodeIntervalStack.of(nodeA)
//      ),
//      CallTrace.toConsole[Int, Dom, String, NodeIntervalStack.Context[Int, Dom, String]](
//        Treap.treapShow,
//        NodeIntervalStack.contextShow()
//      )
//    )

    println("")
    println("KeySearch.down traverse with NodeVisitStack context")

//    Reduce.before(
//      treap,
//      new NodeVisitStack.Context[Int, Dom](TraverseVisit.None, Nil),
//      ()
//    )(
//      KeySearch.evalContext(4, NodeVisitStack()),
//      CallTrace.toConsole
//    )

//    Reduce.before(
//      nodeA,
//      new NodeIntervalStack.Context[Int, Dom, String](ops.interval.universal, TraverseVisit.None, Nil),
//      ()
//    )(
//      KeySearch.down(4, NodeIntervalStack()),
//      CallTrace.toConsole[Int, Dom, String, NodeIntervalStack.Context[Int, Dom, String]](
//        Treap.treapShow,
//        NodeIntervalStack.contextShow()
//      )
//    )

    println("")
    println("KeySearch.nextKey traverse with NodeVisitStack context")

    var contextExtract =
      ContextExtract.reduceBefore(
        nodeA,
        NodeVisitStack.contextOps[Int, Dom, String].getEmptyContext
      )(
        DepthFirst.nonEmpty(DepthFirst.leftOnlyNavigate, NodeVisitStack.of(nodeA))
      )

    val toConsole = CallTrace.toConsole[Int, Dom, String, NodeVisitStack.Context[Int, Dom, String]]
    var context = contextExtract.context
    var tree = contextExtract.tree

    toConsole(tree, context, ())
    for (i <- 1 to 7) {
      val currentExtract = ContextExtract.reduceAfter(tree, context)(KeySearch.nextKey(NodeVisitStack()))
      context = currentExtract.context
      tree = currentExtract.tree
      toConsole(tree, context, ())
    }
    for (i <- 1 to 7) {
      val currentExtract = ContextExtract.reduceAfter(tree, context)(KeySearch.prevKey(NodeVisitStack()))
      context = currentExtract.context
      tree = currentExtract.tree
      toConsole(tree, context, ())
    }

    println("")
    println("KeySearch.up traverse + TreeSlice reduce")

    val slice = TreeSlice.reduce[Int, Dom, String](nodeA, 4, TreeSlice.Mutable.Output.initial)

    println(slice.leftTree)

    val mergedTree = TreeMerge.reduce(slice.leftTree, slice.rightTree)

    println(mergedTree)
  }
}