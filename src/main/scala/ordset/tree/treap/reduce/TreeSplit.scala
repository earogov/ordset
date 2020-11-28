package ordset.tree.treap.reduce

import ordset.domain.Domain
import ordset.tree.core.eval.TreeVisitStack
import ordset.tree.treap.traverse.NodeSearch
import ordset.tree.treap.Treap
import ordset.tree.core.Reduce
import ordset.tree.core.reduce.ContextExtract

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
 *     val contextExtract = ContextExtract.reduceAfter(
 *       tree,
 *       TreeVisitStack.contextOps[K, V, Treap.Node].getEmptyContext
 *     )(
 *       NodeSearch.down(key, TreeVisitStack.function())
 *     )
 * }}}
 *
 * 2. Move upward starting from node H (with the received context) and apply [[TreeSplit.splitFunc]] as a reduce
 *    function to build left and right subtrees.
 *
 * {{{
 *     val split = Reduce.before(
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

  def splitFunc[K, V, C](
    key: K
  )(
    implicit domain: Domain[K]
  ): NodeReduceFunc[K, V, C, SplitOutput[K, V]] =
    (tree, _, output) =>
      if (domain.elementOrd.compare(tree.key, key) <= 0) output.withLeftParent(tree)
      else output.withRightParent(tree)

  def reduceNode[K, V](
    node: Treap.Node[K, V],
    key: K,
    initial: SplitOutput[K, V]
  )(
    implicit domain: Domain[K]
  ): SplitOutput[K, V] = {
    val contextExtract = ContextExtract.reduceAfter(
      node,
      TreeVisitStack.contextOps[K, V, Treap.Node].getEmptyContext
    )(
      NodeSearch.down(key, TreeVisitStack.function())
    )
    Reduce.before(
      contextExtract.tree,
      contextExtract.context,
      initial
    )(
      NodeSearch.up(NodeSearch.UpwardStopPredicate.never, TreeVisitStack.function()),
      TreeSplit.splitFunc(key)
    )
  }

  def reduceTreap[K, V](
    tree: Treap[K, V],
    key: K,
    initial: SplitOutput[K, V]
  )(
    implicit domain: Domain[K]
  ): SplitOutput[K, V] =
    tree match {
      case node: Treap.Node[K, V] => reduceNode(node, key, initial)(domain)
      case _ => initial
    }
}
