package test.ordset.treap

import ordset.domain.{Domain, DomainOps}
import ordset.instances
import ordset.treap.eval.{NodeIntervalStack, NodeVisitStack}
import ordset.treap.reduce.CallTrace
import ordset.treap.traverse.{DepthFirst, KeySearch}
import ordset.treap.{Reduce, TraverseVisit, Treap}
import org.scalatest.funspec.AnyFunSpec

class TreapSpec extends AnyFunSpec {

  import instances.Int._
  import NodeVisitStack.stackShow
  import NodeIntervalStack.stackShow

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

//    Reduce(
//      nodeA,
//      new NodeVisitStack.Context[Int, Dom, String](TraverseVisit.None, Nil),
//      ()
//    )(
//      DepthFirst.nonEmpty(
//        DepthFirst.leftFirstNavigate,
//        NodeVisitStack.of(nodeA)
//      ),
//      CallTrace.toConsole
//    )

    Reduce(
      nodeA,
      new NodeIntervalStack.Context[Int, Dom, String](ops.interval.universal, TraverseVisit.None, Nil),
      ()
    )(
      DepthFirst.withEmpty(
        DepthFirst.leftFirstNavigate,
        NodeIntervalStack.of(nodeA)
      ),
      CallTrace.toConsole[Int, Dom, String, NodeIntervalStack.Context[Int, Dom, String]]((NodeIntervalStack.contextShow()))
    )

    println("")
    println("KeySearch traverse with NodeVisitStack context")

//    Reduce(
//      treap,
//      new NodeVisitStack.Context[Int, Dom](TraverseVisit.None, Nil),
//      ()
//    )(
//      KeySearch.evalContext(4, NodeVisitStack()),
//      CallTrace.toConsole
//    )

    println("")
    println("KeySearch traverse with NodeIntervalStack context")

    Reduce(
      nodeA,
      new NodeIntervalStack.Context[Int, Dom, String](ops.interval.universal, TraverseVisit.None, Nil),
      ()
    )(
      KeySearch.evalContext(4, NodeIntervalStack()),
      CallTrace.toConsole[Int, Dom, String, NodeIntervalStack.Context[Int, Dom, String]](NodeIntervalStack.contextShow())
    )
  }
}