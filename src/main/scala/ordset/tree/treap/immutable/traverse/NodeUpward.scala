package ordset.tree.treap.immutable.traverse

import ordset.tree.core.BinaryTreeStep
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.{NodeEvalFunc, NodeStackOps}

object NodeUpward {

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
        new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      } else {
        val newContext = evalFunc(tree, context, BinaryTreeStep.Up)
        new NodeTraverseOutput(parent, newContext, BinaryTreeStep.Up, stop = stopPredicate(parent, tree))
      }
    }

  trait StopPredicate[-K, -V] {

    def apply(parent: ImmutableTreap.Node[K, V], child: ImmutableTreap.Node[K, V]): Boolean
  }

  object StopPredicate {

    def toNextKey[K, V]: StopPredicate[K, V] = NextKeyInstance

    def toPrevKey[K, V]: StopPredicate[K, V] = PrevKeyInstance

    def never[K, V]: StopPredicate[K, V] = NeverInstance

    def always[K, V]: StopPredicate[K, V] = AlwaysInstance

    // PRIVATE SECTION
    private lazy val NextKeyInstance: StopPredicate[Any, Any] = (parent, tree) => parent.hasLeftInstance(tree)

    private lazy val PrevKeyInstance: StopPredicate[Any, Any] = (parent, tree) => parent.hasRightInstance(tree)

    private lazy val NeverInstance: StopPredicate[Any, Any] = (_, _) => false

    private lazy val AlwaysInstance: StopPredicate[Any, Any] = (_, _) => true
  }
}
