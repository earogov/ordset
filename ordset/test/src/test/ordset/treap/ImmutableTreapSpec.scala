package test.ordset.treap

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.instances
import ordset.tree.core.eval.TreeVisitStack
import ordset.tree.core.fold.{CallTrace, ContextExtract}
import ordset.tree.treap.immutable.transform.TreeSplit
import ordset.tree.treap.immutable.traverse.{NodeAside, NodeDepthFirst}
import ordset.tree.core.{BinaryTreeVisit, Fold}
import ordset.tree.treap.Treap
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.NodeVisitContext
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

// TODO implement unit tests for treap.
@RunWith(classOf[JUnitRunner])
class ImmutableTreapSpec extends AnyFunSpec {

  import ordset.core.instances.int._

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

  it("should fold tree") {

    val leafF = ImmutableTreap.Leaf[Int, String](2, 2, "F")
    val leafH = ImmutableTreap.Leaf[Int, String](4, 1, "H")
    val nodeD = ImmutableTreap.NodeWithRightOnly[Int, String](leafF, 1, 4, "D")
    val nodeE = ImmutableTreap.NodeWithLeftOnly[Int, String](leafH, 5, 5, "E")
    val nodeB = ImmutableTreap.NodeWithLeftRight[Int, String](nodeD, nodeE, 3, 7, "B")
    val leafG = ImmutableTreap.Leaf[Int, String](8, 3, "G")
    val nodeC = ImmutableTreap.NodeWithRightOnly[Int, String](leafG, 7, 6, "C")
    val nodeA = ImmutableTreap.NodeWithLeftRight[Int, String](nodeB, nodeC, 6, 9, "A")

//    println("")
//    println("DepthFirst traverse with NodeVisitStack context")
//
//    Fold.before[Int, String, ImmutableTreap.Node, NodeVisitContext[Int, String], Unit](
//      nodeA,
//      new TreeVisitStack.Context(BinaryTreeVisit.None, Nil),
//      ()
//    )(
//      NodeDepthFirst.standard(
//        NodeDepthFirst.leftFirstNavigation,
//        TreeVisitStack.function
//      ),
//      CallTrace.toConsole
//    )
//
//    println("")
//    println("KeySearch.nextKey traverse with NodeVisitStack context")
//
//    var contextExtract =
//      ContextExtract.foldBefore[Int, String, ImmutableTreap.Node, NodeVisitContext[Int, String]](
//        nodeA,
//        TreeVisitStack.contextOps.getEmptyContext
//      )(
//        NodeDepthFirst.standard(NodeDepthFirst.leftOnlyNavigation, TreeVisitStack.function)
//      )
//
//    val toConsole = CallTrace.toConsole[Int, String, ImmutableTreap.Node, NodeVisitContext[Int, String]]
//    var context = contextExtract.context
//    var tree = contextExtract.tree
//
//    toConsole(tree, context, ())
//    for (i <- 1 to 7) {
//      val currentExtract = ContextExtract.foldAfter(
//        tree,
//        context
//      )(
//        NodeSearch.nextKey(TreeVisitStack.function)
//      )
//      context = currentExtract.context
//      tree = currentExtract.tree
//      toConsole(tree, context, ())
//    }
//
//    println("")
//    println("KeySearch.prevKey traverse with NodeVisitStack context")
//
//    toConsole(tree, context, ())
//    for (i <- 1 to 7) {
//      val currentExtract = ContextExtract.foldAfter(
//        tree,
//        context
//      )(
//        NodeSearch.prevKey(TreeVisitStack.function)
//      )
//      context = currentExtract.context
//      tree = currentExtract.tree
//      toConsole(tree, context, ())
//    }
//
//    println("")
//    println("TreeSlice")
//
//    val split = TreeSplit.foldNode[Int, Int, String](
//      nodeA,
//      4,
//      SplitOutput.Mutable.Output.initial
//    )
//
//    println(split)
//
//    println("")
//    println("MergeSlice")
//
//    val mergedTree = TreeMerge.foldTreap[Int, Int, String](
//      split.leftTree,
//      split.rightTree
//    )(
//      Treap.nodePriorityOrder
//    )
//
//    println(mergedTree)
  }
}