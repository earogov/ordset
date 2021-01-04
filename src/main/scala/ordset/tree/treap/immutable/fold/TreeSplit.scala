package ordset.tree.treap.immutable.fold

import ordset.Order
import ordset.tree.core.eval.TreeVisitStack
import ordset.tree.treap.immutable.traverse.NodeSearch
import ordset.tree.core.Fold
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.eval.NodeVisitContext

/**
 * [[TreeSplit.splitFunc]] implements split operation for treap. Output is a pair of left and right trees.
 * If node key ≤ split key then node is included in left tree, else in right.
 * Both immutable and mutable output tuples are available.
 *
 * {{{
 *
 *  priority
 *                                  initial tree
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
 *         1     2     3     4     5     6     7     8  key
 *
 *                        split at key 4
 *                              V
 *  priority
 *                                 output.rightTree
 *    9  -                               A
 *    8  -        output.leftTree      /   ↘
 *    7  -             B              /       ↘
 *    6  -       ↙      \            /          C
 *    5  -    ↙          \          E             ↘
 *    4  - D              \                         ↘
 *    3  -    ↘            \                         G
 *    2  -       F          \
 *    1  -                   H
 *         |-----|-----|-----|-----|-----|-----|-----|
 *         1     2     3     4     5     6     7     8  key
 * }}}
 * =Usage=
 *
 * 1. Move down from the root to the specified key and save context with stack of visited nodes.
 *    In example above we start from node A and stop at node H. Context will contain list of nodes E, B, A.
 *
 * {{{
 *     val contextExtract = ContextExtract.foldAfter[K, V, Treap.Node, NodeVisitContext[K, V]](
 *       node,
 *       TreeVisitStack.contextOps.getEmptyContext
 *     )(
 *       NodeSearch.down(key, TreeVisitStack.function[K, V, Treap.Node]())(keyOrder)
 *     )
 * }}}
 *
 * 2. Move upward starting from node H (with the received context) and apply [[TreeSplit.splitFunc]] as a fold
 *    function to build left and right subtrees.
 *
 * {{{
 *     val split = Fold.before(
 *       contextExtract.tree,
 *       contextExtract.context,
 *       initial
 *     )(
 *       NodeSearch.up(NodeSearch.UpwardStopPredicate.never, TreeVisitStack.function()),
 *       NodeSplit.splitFunc(key)
 *     )
 *     println("Left tree: " + split.leftTree)
 *     println("Right tree: " + split.rightTree)
 * }}}
 */
object TreeSplit {

  def splitFunc[K, KK >: K, V, C](
    key: KK
  )(
    implicit keyOrder: Order[KK]
  ): NodeFoldFunc[K, V, C, SplitOutput[K, V]] =
    (tree, _, output) =>
      if (keyOrder.compare(tree.key, key) <= 0) output.withLeftParent(tree)
      else output.withRightParent(tree)

  def foldNode[K, KK >: K, V](
    node: ImmutableTreap.Node[K, V],
    key: KK,
    initial: SplitOutput[K, V]
  )(
    implicit keyOrder: Order[KK]
  ): SplitOutput[K, V] = {
    val contextExtract = ContextExtract.foldAfter[K, V, ImmutableTreap.Node, NodeVisitContext[K, V]](
      node,
      TreeVisitStack.contextOps.getEmptyContext
    )(
      NodeSearch.down(key, TreeVisitStack.function[K, V, ImmutableTreap.Node]())(keyOrder)
    )
    Fold.before(
      contextExtract.tree,
      contextExtract.context,
      initial
    )(
      NodeSearch.up(NodeSearch.UpwardStopPredicate.never, TreeVisitStack.function()),
      TreeSplit.splitFunc[K, KK, V, NodeVisitContext[K, V]](key)(keyOrder)
    )
  }

  def foldTreap[K, KK >: K, V](
    tree: ImmutableTreap[K, V],
    key: KK,
    initial: SplitOutput[K, V]
  )(
    implicit keyOrder: Order[KK]
  ): SplitOutput[K, V] =
    tree match {
      case node: ImmutableTreap.Node[K, V] => foldNode(node, key, initial)(keyOrder)
      case _ => initial
    }
}
