package ordset.tree.treap.immutable.traverse

import ordset.tree.core.Traverse
import ordset.tree.core.BinaryTreeStep
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.{NodeEvalFunc, NodeStackOps}

object NodeUpward {

  /**
   * Moves to upward node (from context stack).
   * Stops if `stopPredicate` returns `true` or if required step is impossible.
   * `stopPredicate` is applied after move, i.e. to the pair of parent node and current node.
   */
  def defaultFunc[K, V, C](
    stopPredicate: StopPredicate[K, V],
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit stackOps: NodeStackOps[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) => {
      val parent = stackOps.getHeadTreeOrDefault(context, null)
      if (parent == null) {
        val newContext = evalFunc(tree, context, BinaryTreeStep.None)
        new Traverse.Output(tree, newContext, BinaryTreeStep.None, stop = true)
      } else {
        val newContext = evalFunc(tree, context, BinaryTreeStep.Up)
        new Traverse.Output(parent, newContext, BinaryTreeStep.Up, stop = stopPredicate(parent, tree))
      }
    }

  /**
   * Applies [[defaultFunc]] one time.
   * Returns parent node if there is one or `node` itself.
   */
  def foldStep[K, V, C](
    node: ImmutableTreap.Node[K, V],
    initContext: C,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit stackOps: NodeStackOps[K, V, C]
  ): ContextExtract.Output[K, V, ImmutableTreap.Node, C] =
    ContextExtract.foldAfter(
      node, initContext
    )(
      defaultFunc(StopPredicate.always, evalFunc)(stackOps)
    )

  /**
   * Applies [[defaultFunc]] in cycle until next key is reached.
   * If there is no next key upwards tree root is returned.
   */
  def foldToNextKey[K, V, C](
    node: ImmutableTreap.Node[K, V],
    initContext: C,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit stackOps: NodeStackOps[K, V, C]
  ): ContextExtract.Output[K, V, ImmutableTreap.Node, C] =
    ContextExtract.foldAfter(
      node, initContext
    )(
      defaultFunc(StopPredicate.toNextKey, evalFunc)(stackOps)
    )

  /**
   * Applies [[defaultFunc]] in cycle until previous key is reached.
   * If there is no previous key upwards tree root is returned.
   */
  def foldToPrevKey[K, V, C](
    node: ImmutableTreap.Node[K, V],
    initContext: C,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit stackOps: NodeStackOps[K, V, C]
  ): ContextExtract.Output[K, V, ImmutableTreap.Node, C] =
    ContextExtract.foldAfter(
      node, initContext
    )(
      defaultFunc(StopPredicate.toPrevKey, evalFunc)(stackOps)
    )

  /**
   * Applies [[defaultFunc]] in cycle until tree root is reached.
   */
  def foldToRoot[K, V, C](
    node: ImmutableTreap.Node[K, V],
    initContext: C,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit stackOps: NodeStackOps[K, V, C]
  ): ContextExtract.Output[K, V, ImmutableTreap.Node, C] =
    ContextExtract.foldAfter(
      node, initContext
    )(
      defaultFunc(StopPredicate.never, evalFunc)(stackOps)
    )

  trait StopPredicate[-K, -V] {

    def apply(parent: ImmutableTreap.Node[K, V], child: ImmutableTreap.Node[K, V]): Boolean
  }

  object StopPredicate {

    /**
     * Returns `true` if next key is reached.
     */
    def toNextKey[K, V]: StopPredicate[K, V] = NextKeyInstance

    /**
     * Returns `true` if previous key is reached.
     */
    def toPrevKey[K, V]: StopPredicate[K, V] = PrevKeyInstance

    /**
     * Never returns `true`.
     */
    def never[K, V]: StopPredicate[K, V] = NeverInstance

    /**
     * Always returns `true`.
     */
    def always[K, V]: StopPredicate[K, V] = AlwaysInstance

    // Private section ---------------------------------------------------------- //
    private lazy val NextKeyInstance: StopPredicate[Any, Any] = (parent, tree) => parent.hasLeftInstance(tree)

    private lazy val PrevKeyInstance: StopPredicate[Any, Any] = (parent, tree) => parent.hasRightInstance(tree)

    private lazy val NeverInstance: StopPredicate[Any, Any] = (_, _) => false

    private lazy val AlwaysInstance: StopPredicate[Any, Any] = (_, _) => true
  }
}
