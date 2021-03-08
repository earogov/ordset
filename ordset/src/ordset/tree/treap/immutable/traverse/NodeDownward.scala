package ordset.tree.treap.immutable.traverse

import ordset.Order
import ordset.tree.core.Traverse
import ordset.tree.core.BinaryTreeStep
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.NodeEvalFunc

object NodeDownward {

  /**
   * Moves either to the left or right child (if possible) depending on the output of `navigationFunc`.
   * Stops if receives [[BinaryTreeStep.None]] from `navigationFunc` of if required step is impossible.
   */
  def defaultFunc[K, V, C](
    navigationFunc: NodeNavigationFunc[K, V, C],
    evalFunc: NodeEvalFunc[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) => {
      val step = navigationFunc(tree, context)
      step match {
        case BinaryTreeStep.Right => tree match {
          case n: ImmutableTreap.NodeWithRight[K, V] =>
            val newContext = evalFunc(tree, context, step)
            new Traverse.Output(n.right, newContext, step, stop = false)
          case _ =>
            val newContext = evalFunc(tree, context, BinaryTreeStep.None)
            new Traverse.Output(tree, newContext, BinaryTreeStep.None, stop = true)
        }
        case BinaryTreeStep.Left => tree match {
          case n: ImmutableTreap.NodeWithLeft[K, V] =>
            val newContext = evalFunc(tree, context, step)
            new Traverse.Output(n.left, newContext, step, stop = false)
          case _ =>
            val newContext = evalFunc(tree, context, BinaryTreeStep.None)
            new Traverse.Output(tree, newContext, BinaryTreeStep.None, stop = true)
        }
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new Traverse.Output(tree, newContext, BinaryTreeStep.None, stop = true)
      }
    }

  /**
   *  Applies [[defaultFunc]] with [[Navigation.defaultFunc]] in cycle until meets stop condition.
   */
  def foldDefault[K, KK >: K, V, C](
    node: ImmutableTreap.Node[K, V],
    initContext: C,
    key: KK,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit keyOrder: Order[KK]
  ): ContextExtract.Output[K, V, ImmutableTreap.Node, C] =
    ContextExtract.foldAfter(
      node,
      initContext
    )(
      defaultFunc[K, V, C](
        Navigation.defaultFunc[K, KK, V](key)(keyOrder),
        evalFunc
      )
    )

  /**
   * Applies [[defaultFunc]] one time with specified `step` (left or right).
   */
  def foldStep[K, V, C](
    node: ImmutableTreap.Node[K, V],
    initContext: C,
    step: BinaryTreeStep.Type,
    evalFunc: NodeEvalFunc[K, V, C]
  ): ContextExtract.Output[K, V, ImmutableTreap.Node, C] = {
    val traverseOutput = defaultFunc[K, V, C]((_, _) => step, evalFunc)(node, initContext)
    ContextExtract.Output(traverseOutput.tree, traverseOutput.context)
  }

  /**
   * Similar to [[foldDefault]] but with additional handling of special case below.
   * {{{
   *
   *       A   |
   *         ↘ |
   *           B
   *         ↙ |  ↘
   *       ↙   |    ↘
   *     C    key    D
   * }}}
   * Let `B` is a current node and its key is equals to the specified `key`. Then [[foldDefault]] will stop
   * at node `B` (due to [[Navigation.defaultFunc]] stop condition).
   *
   * [[foldForRightSplit]] will move to the left child (node `C`) and continue to descend in its subtree.
   *
   * These additional steps are required for correct subsequent execution of
   * [[ordset.tree.treap.immutable.transform.TreeSplit.splitRightFunc]].
   */
  def foldForRightSplit[K, KK >: K, V, C](
    node: ImmutableTreap.Node[K, V],
    initContext: C,
    key: KK,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit keyOrder: Order[KK]
  ): ContextExtract.Output[K, V, ImmutableTreap.Node, C] =
    ContextExtract.foldAfter(
      node,
      initContext
    )(
      defaultFunc[K, V, C](
        Navigation.rightSplitFunc[K, KK, V](key)(keyOrder),
        evalFunc
      )
    )

  /**
   * Similar to [[foldDefault]] but with additional handling of special case below.
   * {{{
   *
   *       A   |
   *         ↘ |
   *           B
   *         ↙ |  ↘
   *       ↙   |    ↘
   *     C    key    D
   * }}}
   * Let `B` is a current node and its key is equals to the specified `key`. Then [[foldDefault]] will stop
   * at node `B` (due to [[Navigation.defaultFunc]] stop condition).
   *
   * [[foldForLeftSplit]] will move to the right child (node `D`) and continue to descend in its subtree.
   *
   * These additional steps are required for correct subsequent execution of
   * [[ordset.tree.treap.immutable.transform.TreeSplit.splitLeftFunc]].
   */
  def foldForLeftSplit[K, KK >: K, V, C](
    node: ImmutableTreap.Node[K, V],
    initContext: C,
    key: KK,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit keyOrder: Order[KK]
  ): ContextExtract.Output[K, V, ImmutableTreap.Node, C] =
    ContextExtract.foldAfter(
      node,
      initContext
    )(
      defaultFunc[K, V, C](
        Navigation.leftSplitFunc[K, KK, V](key)(keyOrder),
        evalFunc
      )
    )

  object Navigation {

    /**
     * Returns:
     * <tr>[[BinaryTreeStep.Right]] if current node key `<` specified `key`</tr>
     * <tr>[[BinaryTreeStep.Left]] if current node key `>` specified `key`</tr>
     * <tr>[[BinaryTreeStep.None]] if current node key `=` specified `key`</tr>
     */
    def defaultFunc[K, KK >: K, V](
      key: KK
    )(
      implicit keyOrder: Order[KK]
    ): NodeNavigationFunc[K, V, Any] =
      (tree, _) => {
        val cmp = keyOrder.compare(tree.key, key)
        if (cmp < 0) BinaryTreeStep.Right
        else if (cmp > 0) BinaryTreeStep.Left
        else BinaryTreeStep.None
      }

    /**
     * Returns:
     * <tr>[[BinaryTreeStep.Right]] if current node key `<` specified `key`</tr>
     * <tr>[[BinaryTreeStep.Left]] if current node key `>=` specified `key`</tr>
     */
    def rightSplitFunc[K, KK >: K, V](
      key: KK
    )(
      implicit keyOrder: Order[KK]
    ): NodeNavigationFunc[K, V, Any] =
      (tree, _) => {
        if (keyOrder.compare(tree.key, key) < 0) BinaryTreeStep.Right
        else BinaryTreeStep.Left
      }

    /**
     * Returns:
     * <tr>[[BinaryTreeStep.Right]] if current node key `<=` specified `key`</tr>
     * <tr>[[BinaryTreeStep.Left]] if current node key `>` specified `key`</tr>
     */
    def leftSplitFunc[K, KK >: K, V](
      key: KK
    )(
      implicit keyOrder: Order[KK]
    ): NodeNavigationFunc[K, V, Any] =
      (tree, _) => {
        if (keyOrder.compare(tree.key, key) <= 0) BinaryTreeStep.Right
        else BinaryTreeStep.Left
      }
  }
}
