package ordset.test.treap.examples

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.instances
import ordset.tree.core.eval.TreeVisitStack
import ordset.tree.core.fold.{CallTrace, ContextExtract}
import ordset.tree.core.{BinaryTreeVisit, Fold}
import ordset.tree.treap.Treap
import ordset.tree.treap.immutable.{ImmutableTreap, NodeVisitContext}
import ordset.tree.treap.immutable.transform.TreeSplit
import ordset.tree.treap.immutable.traverse.{NodeAside, NodeDepthFirst}
import org.junit.runner.RunWith

object ImmutableTreapExample {

  import ordset.core.instances.int.*

  type Dom = Domain[Int]

  private val ops: DomainOps[Int, Dom] = DomainOps.defaultDomainOps

  @main
  def immutableTreapExampleMain() = {

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

    val leafF = ImmutableTreap.Leaf[Int, String](2, 2, "F")
    val leafH = ImmutableTreap.Leaf[Int, String](4, 1, "H")
    val nodeD = ImmutableTreap.NodeWithRightOnly[Int, String](leafF, 1, 4, "D")
    val nodeE = ImmutableTreap.NodeWithLeftOnly[Int, String](leafH, 5, 5, "E")
    val nodeB = ImmutableTreap.NodeWithLeftRight[Int, String](nodeD, nodeE, 3, 7, "B")
    val leafG = ImmutableTreap.Leaf[Int, String](8, 3, "G")
    val nodeC = ImmutableTreap.NodeWithRightOnly[Int, String](leafG, 7, 6, "C")
    val nodeA = ImmutableTreap.NodeWithLeftRight[Int, String](nodeB, nodeC, 6, 9, "A")

    // TODO add some operations
  }
}