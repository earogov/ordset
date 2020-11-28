package test.ordset.treap

import ordset.domain.{Domain, DomainOps}
import ordset.instances
import ordset.tree.core.eval.TreeVisitStack
import ordset.tree.core.reduce.{CallTrace, ContextExtract}
import ordset.tree.treap.reduce.{SplitOutput, TreeMerge, TreeSplit}
import ordset.tree.treap.traverse.{NodeDepthFirst, NodeSearch}
import ordset.tree.treap.Treap
import ordset.tree.core.{BinaryTreeVisit, Reduce}
import ordset.tree.treap.eval.NodeVisitContext
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

    val leafF = Treap.Leaf[Int, String](2, 2, "F")
    val leafH = Treap.Leaf[Int, String](4, 1, "H")
    val nodeD = Treap.NodeWithRightOnly[Int, String](leafF, 1, 4, "D")
    val nodeE = Treap.NodeWithLeftOnly[Int, String](leafH, 5, 5, "E")
    val nodeB = Treap.NodeWithLeftRight[Int, String](nodeD, nodeE, 3, 7, "B")
    val leafG = Treap.Leaf[Int, String](8, 3, "G")
    val nodeC = Treap.NodeWithRightOnly[Int, String](leafG, 7, 6, "C")
    val nodeA = Treap.NodeWithLeftRight[Int, String](nodeB, nodeC, 6, 9, "A")

    println("")
    println("DepthFirst traverse with NodeVisitStack context")

    Reduce.before[Int, String, Treap.Node, NodeVisitContext[Int, String], Unit](
      nodeA,
      new TreeVisitStack.Context(BinaryTreeVisit.None, Nil),
      ()
    )(
      NodeDepthFirst.standard(
        NodeDepthFirst.leftFirstNavigate,
        TreeVisitStack.function()
      ),
      CallTrace.toConsole
    )

//    Reduce.before(
//      nodeA,
//      new NodeIntervalStack.Context[Int, String](ops.interval.universal, TraverseVisit.None, Nil),
//      ()
//    )(
//      DepthFirst.withEmpty(
//        DepthFirst.leftFirstNavigate,
//        NodeIntervalStack.of(nodeA)
//      ),
//      CallTrace.toConsole[Int, String, NodeIntervalStack.Context[Int, String]](
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
//      new NodeIntervalStack.Context[Int, String](ops.interval.universal, TraverseVisit.None, Nil),
//      ()
//    )(
//      KeySearch.down(4, NodeIntervalStack()),
//      CallTrace.toConsole[Int, String, NodeIntervalStack.Context[Int, String]](
//        Treap.treapShow,
//        NodeIntervalStack.contextShow()
//      )
//    )

    println("")
    println("KeySearch.nextKey traverse with NodeVisitStack context")

    var contextExtract =
      ContextExtract.reduceBefore[Int, String, Treap.Node, NodeVisitContext[Int, String]](
        nodeA,
        TreeVisitStack.contextOps.getEmptyContext
      )(
        NodeDepthFirst.standard(NodeDepthFirst.leftOnlyNavigate, TreeVisitStack.function())
      )

    val toConsole = CallTrace.toConsole[Int, String, Treap.Node, NodeVisitContext[Int, String]]
    var context = contextExtract.context
    var tree = contextExtract.tree

    toConsole(tree, context, ())
    for (i <- 1 to 7) {
      val currentExtract = ContextExtract.reduceAfter(
        tree,
        context
      )(
        NodeSearch.nextKey(TreeVisitStack.function())
      )
      context = currentExtract.context
      tree = currentExtract.tree
      toConsole(tree, context, ())
    }

    println("")
    println("KeySearch.prevKey traverse with NodeVisitStack context")

    toConsole(tree, context, ())
    for (i <- 1 to 7) {
      val currentExtract = ContextExtract.reduceAfter(
        tree,
        context
      )(
        NodeSearch.prevKey(TreeVisitStack.function())
      )
      context = currentExtract.context
      tree = currentExtract.tree
      toConsole(tree, context, ())
    }

    println("")
    println("TreeSlice")

    val split = TreeSplit.reduceNode[Int, String](
      nodeA,
      4,
      SplitOutput.Mutable.Output.initial
    )

    println(split)

    println("")
    println("MergeSlice")

    val mergedTree = TreeMerge.reduceTreap[Int, String](
      split.leftTree,
      split.rightTree
    )

    println(mergedTree)
  }
}