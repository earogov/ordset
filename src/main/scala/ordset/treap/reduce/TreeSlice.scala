package ordset.treap.reduce

import ordset.domain.Domain
import ordset.treap.{Reduce, Treap}

/**
 * [[TreeSlice.function]] implements slice operation for treap. Output is a pair of left and right trees.
 * If node key ≤ slice key then node is included in left tree, else in right.
 * Both immutable and mutable output tuples are available.
 *
 * {{{
 *                           initial tree
 *
 *    9  -                               A
 *    8  -                      ↙          ↘
 *    7  -             B                     ↘
 *    6  -       ↙           ↘                 C
 *    5  -    ↙                    E             ↘
 *    4  - D                      ↙                ↘
 *    3  -    ↘                 ↙                    G
 *    2  -       F            ↙
 *    1  -                   H
 *         |-----|-----|-----|-----|-----|-----|-----|
 *         1     2     3     4     5     6     7     8
 *
 *                        slice at key 4
 *                              V
 *
 *               output.leftTree      output.rightTree
 *
 *    9  -                               A
 *    8  -                             /   ↘
 *    7  -             B              /       ↘
 *    6  -       ↙      \            /          C
 *    5  -    ↙          \          E             ↘
 *    4  - D              \                         ↘
 *    3  -    ↘            \                         G
 *    2  -       F          \
 *    1  -                   H
 *         |-----|-----|-----|-----|-----|-----|-----|
 *         1     2     3     4     5     6     7     8
 * }}}
 * =Usage=
 *
 * 1. Move down from the root to the specified key and save context - stack of visited nodes.
 *    In example above we start from node A and stop at node H and context is a list of nodes E, B, A.
 * {{{
 * val contextExtract = ContextExtract.reduceAfter(
 *    nodeA,
 *    NodeVisitStack.contextOps[Int, Domain[Int], String].emptyContext()
 * )(
 *    KeySearch.down(4, NodeVisitStack.of(nodeA))
 * )
 * }}}
 * 2. Move upward starting from node H (with the received context) and apply [[TreeSlice.function]] as a reduce
 *    function to build left and right subtrees.
 * {{{
 * val slice = Reduce.before(
 *    contextExtract.tree,
 *    contextExtract.context,
 *    TreeSlice.Immutable.Output.initial[Int, Dom, String]
 * )(
 *    KeySearch.up(_ => false, NodeVisitStack.of(contextExtract.tree)),
 *    TreeSlice.function[Int, Dom, String, NodeVisitStack.Context[Int, Dom, String]](4)
 * )
 * println("Left tree: " + slice.leftTree)
 * println("Right tree: " + slice.rightTree)
 * }}}
 */
object TreeSlice {

  trait Output[E, D <: Domain[E], W] {

    def leftTree: Treap[E, D, W]

    def rightTree: Treap[E, D, W]

    def withLeftTree(tree: Treap[E, D, W]): Output[E, D, W]

    def withLeftParent(parent: Treap.Node[E, D, W]): Output[E, D, W]

    def withRightTree(tree: Treap[E, D, W]): Output[E, D, W]

    def withRightParent(parent: Treap.Node[E, D, W]): Output[E, D, W]
  }

  object Immutable {

    case class Output[E, D <: Domain[E], W](
      override val leftTree: Treap[E, D, W],
      override val rightTree: Treap[E, D, W]
    ) extends TreeSlice.Output[E, D, W] {

      override def withLeftTree(tree: Treap[E, D, W]): Output[E, D, W] = Output(tree, rightTree)

      override def withLeftParent(parent: Treap.Node[E, D, W]): Output[E, D, W] =
        leftTree match {
          case leftTree: Treap.Node[E, D, W] => Output(parent.withRight(leftTree), rightTree)
          case _ => Output(parent.withoutRight(), rightTree)
        }

      override def withRightTree(tree: Treap[E, D, W]): Output[E, D, W] = Output(leftTree, tree)

      override def withRightParent(parent: Treap.Node[E, D, W]): Output[E, D, W] =
        rightTree match {
          case rightTree: Treap.Node[E, D, W] => Output(leftTree, parent.withLeft(rightTree))
          case _ => Output(leftTree, parent.withoutLeft())
        }
    }

    object Output {

      def initial[E, D <: Domain[E], W]: TreeSlice.Output[E, D, W] = Initial.asInstanceOf[TreeSlice.Output[E, D, W]]

      private lazy val Initial: Output[Any, Domain[Any], Any] = Output(Treap.Empty(), Treap.Empty())
    }
  }

  object Mutable {

    class Output[E, D <: Domain[E], W](
      private var leftTreeVar: Treap[E, D, W],
      private var rightTreeVar: Treap[E, D, W]
    ) extends TreeSlice.Output[E, D, W] {

      override def leftTree: Treap[E, D, W] = leftTreeVar

      override def rightTree: Treap[E, D, W] = rightTreeVar

      override def withLeftTree(tree: Treap[E, D, W]): Output[E, D, W] = {
        this.leftTreeVar = tree
        this
      }

      override def withLeftParent(parent: Treap.Node[E, D, W]): Output[E, D, W] = {
        leftTreeVar match {
          case leftTree: Treap.Node[E, D, W] => this.leftTreeVar = parent.withRight(leftTree)
          case _ => this.leftTreeVar = parent.withoutRight()
        }
        this
      }

      override def withRightTree(tree: Treap[E, D, W]): Output[E, D, W] = {
        this.rightTreeVar = tree
        this
      }

      override def withRightParent(parent: Treap.Node[E, D, W]): Output[E, D, W] = {
        rightTreeVar match {
          case rightTree: Treap.Node[E, D, W] => this.rightTreeVar = parent.withLeft(rightTree)
          case _ => this.rightTreeVar = parent.withoutLeft()
        }
        this
      }
    }

    object Output {

      def initial[E, D <: Domain[E], W]: TreeSlice.Output[E, D, W] = new Output[E, D, W](Treap.Empty(), Treap.Empty())
    }
  }

  def function[E, D <: Domain[E], W, C](
    key: E
  )(
    implicit domain: Domain[E]
  ): Reduce.Func[E, D, W, C, Output[E, D, W]] =
    (tree, _, output) => tree match {
      case tree: Treap.Node[E, D, W] =>
        if (domain.elementOrd.compare(tree.key, key) <= 0) output.withLeftParent(tree)
        else output.withRightParent(tree)
      case _ => output
    }
}
