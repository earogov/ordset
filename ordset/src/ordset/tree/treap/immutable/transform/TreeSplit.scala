package ordset.tree.treap.immutable.transform

import ordset.Order
import ordset.tree.core.Fold
import ordset.tree.core.eval.TreeStack
import ordset.tree.treap.immutable.traverse.NodeDownward.Navigation
import ordset.tree.treap.immutable.traverse.{NodeDownward, NodeUpward}
import ordset.tree.treap.immutable.{ImmutableTreap, NodeFoldFunc, NodeStackContext}

/**
 * Split operation breaks initial treap into left and right subtrees at specified key.
 * <div>
 * [[TreeSplit.splitLeftFunc]] includes all nodes with key `≤` specified key into left subtree and
 * all others - into right.
 * </div>
 * <div>
 * [[TreeSplit.splitRightFunc]] includes all nodes with key `<` specified key into left subtree and
 * all others - into right.
 * </div>
 * <div>
 * Output is a pair of left and right subtrees. Both immutable and mutable output tuples are available.
 * </div>
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
 *                    left split at key 4
 *                           V
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
 *     val contextExtract =
 *       NodeDownward.foldForLeftSplit[K, KK, V, NodeVisitContext[K, V]](
 *         node,
 *         TreeVisitStack.contextOps.getEmptyContext,
 *         key,
 *         TreeVisitStack.function
 *       )
 * }}}
 *
 * 2. Move upward starting from node H (with the received context) and apply [[TreeSplit.splitLeftFunc]] as a fold
 *    function to build left and right subtrees.
 *
 * {{{
 *     val split =
 *       Fold.before(
 *         contextExtract.tree,
 *         contextExtract.context,
 *         initial
 *       )(
 *         NodeUpward.function(
 *           NodeSearch.UpwardStopPredicate.never,
 *           TreeVisitStack.function
 *         ),
 *         NodeSplit.splitLeftFunc(key)(keyOrder)
 *       )
 *     println("Left tree: " + split.leftTree)
 *     println("Right tree: " + split.rightTree)
 * }}}
 *
 * ==Providing valid context==
 * <div>
 * At step 2 split function is folded during upward move, so it is applied only to upward nodes.
 * If at step 1 we stop at node `E` the split result will be incorrect: E -> H subtree will not be splitted
 * and node `H` will be on the right side. So it critical to descend at sufficient depth (<b>rule I</b>).
 * </div>
 * <div>
 * Another important rule - don't stop descending if current node has a child at the side of split border
 * (<b>rule II</b>). Otherwise this child may be lost (none of splitted subtrees will contain it).
 * </div>
 * <div>
 * [[ordset.tree.treap.immutable.traverse.NodeDownward.foldDefault]] may seem suitable but it fails in some cases.
 * </div>
 * {{{
 *
 *       A   |
 *         ↘ |
 *           B
 *         ↙ |  ↘
 *       ↙   |    ↘
 *     C    key    D
 * }}}
 * <div>
 * Assume that at some moment of downward move `B` is a current node and its key is equal to the specified `key`.
 * Then [[ordset.tree.treap.immutable.traverse.NodeDownward.foldDefault]] will stop at this node (due to 
 * [[ordset.tree.treap.immutable.traverse.NodeDownward.Navigation.defaultFunc]] stop condition).
 * The result context will contain `B` as a current node and all its upward nodes in stack.
 * </div>
 * <div>
 * Now suppose we want to apply [[TreeSplit.splitRightFunc]] so that `B` became a part of right subtree.
 * The output subtrees should be: A -> C (left) and B -> D (right).
 * </div>
 * <div>
 * But subtree of node `B` (B -> C) will not be splitted because of split function will not visit node `C`.
 * And we will get wrong answer: A -> B -> C (left) and D (right).
 * </div>
 * <div>
 * To fix this issue we should do the following:
 * </div>
 * <div>
 * - for [[TreeSplit.splitRightFunc]] function move from `B` to the left node `C` and continue descending
 * just as ordinary [[ordset.tree.treap.immutable.traverse.NodeDownward.defaultFunc]];
 * </div>
 * <div>
 * - for [[TreeSplit.splitLeftFunc]] function move from `B` to the right node `D` and continue descending.
 * </div>
 * <div>
 * Note that subsequent descending from `C` or `D` is required due to the rule II.
 * </div>
 * <div>
 * Such logic is implemented in  [[ordset.tree.treap.immutable.traverse.NodeDownward.foldForLeftSplit]] 
 * and [[ordset.tree.treap.immutable.traverse.NodeDownward.foldForRightSplit]] functions.
 * </div>
 */
object TreeSplit {

  /**
   * Returns function that implements one step of split operation.
   *
   * It includes node with key `≤` specified key into left subtree and
   * node with key `>` specified key - into right.
   */
  def splitLeftFunc[K, KK >: K, V, C](
    key: KK
  )(
    implicit keyOrder: Order[KK]
  ): NodeFoldFunc[K, V, C, SplitOutput[K, V]] =
    (tree, _, output) =>
      if (keyOrder.compare(tree.key, key) <= 0) output.withLeftParent(tree)
      else output.withRightParent(tree)

  /**
   * Returns function that implements one step of split operation.
   *
   * It includes node with key `<` specified key into left subtree and
   * node with key `≥` specified key - into right.
   */
  def splitRightFunc[K, KK >: K, V, C](
    key: KK
  )(
    implicit keyOrder: Order[KK]
  ): NodeFoldFunc[K, V, C, SplitOutput[K, V]] =
    (tree, _, output) =>
      if (keyOrder.compare(tree.key, key) < 0) output.withLeftParent(tree)
      else output.withRightParent(tree)

  /**
   * Applies split function to the specified `node` at specified `key`.
   *
   * If `splitLeft` == true, [[splitLeftFunc]] function is applied, otherwise - [[splitRightFunc]].
   *
   * `initial` state allows to choose either immutable or mutable tuple for output.
   */
  def foldNode[K, KK >: K, V](
    node: ImmutableTreap.Node[K, V],
    key: KK,
    splitLeft: Boolean,
    initial: SplitOutput[K, V]
  )(
    implicit keyOrder: Order[KK]
  ): SplitOutput[K, V] = {
    val contextExtract =
      if (splitLeft)
        NodeDownward.foldForLeftSplit[K, KK, V, NodeStackContext[K, V]](
          node,
          TreeStack.contextOps[K, V, ImmutableTreap.Node].getEmptyContext,
          key,
          TreeStack.function
        )
      else
        NodeDownward.foldForRightSplit[K, KK, V, NodeStackContext[K, V]](
          node,
          TreeStack.contextOps[K, V, ImmutableTreap.Node].getEmptyContext,
          key,
          TreeStack.function
        )
    Fold.before(
      contextExtract.tree,
      contextExtract.context,
      initial
    )(
      NodeUpward.defaultFunc[K, V, NodeStackContext[K, V]](
        NodeUpward.StopPredicate.never,
        TreeStack.function
      )(
        TreeStack.contextOps
      ),
      if (splitLeft) splitLeftFunc[K, KK, V, NodeStackContext[K, V]](key)(keyOrder)
      else splitRightFunc[K, KK, V, NodeStackContext[K, V]](key)(keyOrder)
    )
  }

  /**
   * Applies split function to the specified `tree` at specified `key`.
   *
   * If `tree` is empty, `initial` state is returned, otherwise result is equivalent to [[foldNode]].
   *
   * If `splitLeft` == true, [[splitLeftFunc]] function is applied, otherwise - [[splitRightFunc]].
   *
   * `initial` state allows to choose either immutable or mutable tuple for output.
   */
  def foldTreap[K, KK >: K, V](
    tree: ImmutableTreap[K, V],
    key: KK,
    splitLeft: Boolean,
    initial: SplitOutput[K, V]
  )(
    implicit keyOrder: Order[KK]
  ): SplitOutput[K, V] =
    tree match {
      case node: ImmutableTreap.Node[K, V] => foldNode(node, key, splitLeft, initial)(keyOrder)
      case _ => initial
    }
}
