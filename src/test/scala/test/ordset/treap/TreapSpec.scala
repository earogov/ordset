package test.ordset.treap

import ordset.Order
import org.scalatest.funspec.AnyFunSpec
import ordset.treap.{Treap, reduce, traverse}

class TreapSpec extends AnyFunSpec {

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

    type Ord = ordset.Order[Int]
    implicit val Ord: Order[Int] = ordset.OrderWithDir.intAscOrderWithDir

    val leaf7 = Treap.Leaf[Int, Ord](2, 2)
    val leaf8 = Treap.Leaf[Int, Ord](4, 1)
    val node4 = Treap.WithRightOnly[Int, Ord](leaf7, 1, 4)
    val node5 = Treap.WithLeftOnly[Int, Ord](leaf8, 5, 5)
    val node2 = Treap.WithLeftRight[Int, Ord](node4, node5, 3, 7)
    val leaf6 = Treap.Leaf[Int, Ord](8, 3)
    val node3 = Treap.WithRightOnly[Int, Ord](leaf6, 7, 6)
    val treap = Treap.WithLeftRight[Int, Ord](node2, node3, 6, 9)

    //treap.reduce(reduce.CallTrace.toConsole, traverse.DepthFirst.makeLeftFirst, ())
    treap.reduce(reduce.CallTrace.toConsole, traverse.KeySearch(0), ())
  }
}