package ordset.treap.reduce

import ordset.domain.Domain
import ordset.treap.eval.NodeVisitStack
import ordset.treap.traverse.KeySearch
import ordset.treap.{Reduce, Treap}

/**
 * [[TreeSplit.splitFunc]] implements split operation for treap. Output is a pair of left and right trees.
 * If node key ≤ split key then node is included in left tree, else in right.
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
 *                        split at key 4
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
 * 1. Move down from the root to the specified key and save context with stack of visited nodes.
 *    In example above we start from node A and stop at node H. Context will contain list of nodes E, B, A.
 *
 * {{{
 * val contextExtract = ContextExtract.reduceAfter(
 *    nodeA,
 *    NodeVisitStack.contextOps[Int, Domain[Int], String].emptyContext()
 * )(
 *    KeySearch.down(4, NodeVisitStack.of(nodeA))
 * )
 * }}}
 *
 * 2. Move upward starting from node H (with the received context) and apply [[TreeSplit.splitFunc]] as a reduce
 *    function to build left and right subtrees.
 *
 * {{{
 * val split = Reduce.before(
 *    contextExtract.tree,
 *    contextExtract.context,
 *    TreeSplit.Immutable.Output.initial[Int, Domain[Int], String]
 * )(
 *    KeySearch.up(_ => false, NodeVisitStack.of(contextExtract.tree)),
 *    TreeSplit.splitFunc[Int, Domain[Int], String, NodeVisitStack.Context[Int, Domain[Int], String]](4)
 * )
 * println("Left tree: " + split.leftTree)
 * println("Right tree: " + split.rightTree)
 * }}}
 */
object TreeSplit {

  trait Output[E, D <: Domain[E], W] {

    def leftTree: Treap[E, D, W]

    def rightTree: Treap[E, D, W]

    def withLeftTree(tree: Treap[E, D, W]): Output[E, D, W]

    def withRightTree(tree: Treap[E, D, W]): Output[E, D, W]

    def withLeftParent(parent: Treap.Node[E, D, W]): Output[E, D, W] = withLeftTree(parent.withRightTree(leftTree))

    def withRightParent(parent: Treap.Node[E, D, W]): Output[E, D, W] = withRightTree(parent.withLeftTree(rightTree))
  }

  object Immutable {

    case class Output[E, D <: Domain[E], W](
      override val leftTree: Treap[E, D, W],
      override val rightTree: Treap[E, D, W]
    ) extends TreeSplit.Output[E, D, W] {

      override def withLeftTree(tree: Treap[E, D, W]): Output[E, D, W] = Output(tree, rightTree)

      override def withRightTree(tree: Treap[E, D, W]): Output[E, D, W] = Output(leftTree, tree)
    }

    object Output {

      def initial[E, D <: Domain[E], W]: TreeSplit.Output[E, D, W] = Initial.asInstanceOf[TreeSplit.Output[E, D, W]]

      private lazy val Initial: Output[Any, Domain[Any], Any] = Output(Treap.Empty(), Treap.Empty())
    }
  }

  object Mutable {

    class Output[E, D <: Domain[E], W](
      private var leftTreeVar: Treap[E, D, W],
      private var rightTreeVar: Treap[E, D, W]
    ) extends TreeSplit.Output[E, D, W] {

      override def leftTree: Treap[E, D, W] = leftTreeVar

      override def rightTree: Treap[E, D, W] = rightTreeVar

      override def withLeftTree(tree: Treap[E, D, W]): Output[E, D, W] = {
        this.leftTreeVar = tree
        this
      }

      override def withRightTree(tree: Treap[E, D, W]): Output[E, D, W] = {
        this.rightTreeVar = tree
        this
      }
    }

    object Output {

      def initial[E, D <: Domain[E], W]: TreeSplit.Output[E, D, W] = new Output[E, D, W](Treap.Empty(), Treap.Empty())
    }
  }

  def splitFunc[E, D <: Domain[E], W, C](
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

  def reduce[E, D <: Domain[E], W](
    tree: Treap[E, D, W],
    key: E,
    initial: Output[E, D, W]
  )(
    implicit domain: Domain[E]
  ): Output[E, D, W] = {
    val contextExtract = ContextExtract.reduceAfter(
      tree,
      NodeVisitStack.contextOps[E, D, W].getEmptyContext
    )(
      KeySearch.down(key, NodeVisitStack.of(tree))
    )
    Reduce.before(
      contextExtract.tree,
      contextExtract.context,
      initial
    )(
      KeySearch.up(KeySearch.UpwardStopPredicate.never, NodeVisitStack.of(contextExtract.tree)),
      TreeSplit.splitFunc[E, D, W, NodeVisitStack.Context[E, D, W]](key)
    )
  }
}
