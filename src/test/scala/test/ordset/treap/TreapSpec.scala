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

  //   9  -                               1
  //   8  -                      ↙          ↘
  //   7  -             2                     ↘
  //   6  -       ↙           ↘                 3
  //   5  -    ↙                    5             ↘
  //   4  - 4                      ↙                ↘
  //   3  -    ↘                 ↙                    6
  //   2  -       7            ↙
  //   1  -                   8
  //        |-----|-----|-----|-----|-----|-----|-----|
  //        1     2     3     4     5     6     7     8

  it("should reduce tree") {

    val leaf7 = Treap.Leaf[Int, Dom](2, 2)
    val leaf8 = Treap.Leaf[Int, Dom](4, 1)
    val node4 = Treap.WithRightOnly[Int, Dom](leaf7, 1, 4)
    val node5 = Treap.WithLeftOnly[Int, Dom](leaf8, 5, 5)
    val node2 = Treap.WithLeftRight[Int, Dom](node4, node5, 3, 7)
    val leaf6 = Treap.Leaf[Int, Dom](8, 3)
    val node3 = Treap.WithRightOnly[Int, Dom](leaf6, 7, 6)
    val treap = Treap.WithLeftRight[Int, Dom](node2, node3, 6, 9)

    println("")
    println("DepthFirst traverse with NodeVisitStack context")

    Reduce(
      treap,
      new NodeVisitStack.Context[Int, Dom](TraverseVisit.None, Nil),
      ()
    )(
      DepthFirst.nonEmpty(
        DepthFirst.leftFirstNavigate,
        NodeVisitStack.of(treap)
      ),
      CallTrace.toConsole
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
      treap,
      new NodeIntervalStack.Context[Int, Dom](ops.interval.universal, TraverseVisit.None, Nil),
      ()
    )(
      KeySearch.evalContext(4, NodeIntervalStack()),
      CallTrace.toConsole[Int, Dom, NodeIntervalStack.Context[Int, Dom]](NodeIntervalStack.contextShow())
    )
  }
}