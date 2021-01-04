package ordset.tree.treap.immutable.traverse

import ordset.Order
import ordset.tree.core.BinaryTreeStep
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.eval.{NodeEvalFunc, NodeStackOps, NodeVisitContext, NodeVisitContextOps}

object NodeSearch {

  trait UpwardStopPredicate[-K, -V] {

    def apply(parent: ImmutableTreap.Node[K, V], child: ImmutableTreap.Node[K, V]): Boolean
  }

  object UpwardStopPredicate {

    def toNextKey[K, V]: UpwardStopPredicate[K, V] = NextKeyInstance

    def toPrevKey[K, V]: UpwardStopPredicate[K, V] = PrevKeyInstance

    def never[K, V]: UpwardStopPredicate[K, V] = NeverInstance

    def always[K, V]: UpwardStopPredicate[K, V] = AlwaysInstance

    // PRIVATE SECTION
    private lazy val NextKeyInstance: UpwardStopPredicate[Any, Any] = (parent, tree) => parent.hasLeftInstance(tree)

    private lazy val PrevKeyInstance: UpwardStopPredicate[Any, Any] = (parent, tree) => parent.hasRightInstance(tree)

    private lazy val NeverInstance: UpwardStopPredicate[Any, Any] = (_, _) => false

    private lazy val AlwaysInstance: UpwardStopPredicate[Any, Any] = (_, _) => true
  }

  def down[K, KK >: K, V, C](
    key: KK,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit keyOrder: Order[KK]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) => {
      val cmp = keyOrder.compare(key, tree.key)
      if (cmp > 0) tree match {
        case n: ImmutableTreap.NodeWithRight[K, V] =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.Right)
          new NodeTraverseOutput(n.right, newContext, BinaryTreeStep.Right, stop = false)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }
      else if (cmp < 0) tree match {
        case n: ImmutableTreap.NodeWithLeft[K, V] =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.Left)
          new NodeTraverseOutput(n.left, newContext, BinaryTreeStep.Left, stop = false)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }
      else {
        val newContext = evalFunc(tree, context, BinaryTreeStep.None)
        new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }
    }

  def up[K, V, C](
    stopPredicate: UpwardStopPredicate[K, V],
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

  def nextKey[K, V, C <: NodeVisitContext[K, V]](
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit contextOps: NodeVisitContextOps[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) =>
      tree match {
        case tree: ImmutableTreap.NodeWithRight[K, V] =>
          val traverseLeftFunc = NodeDepthFirst.standard(NodeDepthFirst.leftOnlyNavigation, evalFunc)(contextOps)
          val rightContext = evalFunc(tree, context, BinaryTreeStep.Right)
          val contextExtract = ContextExtract.foldAfter(tree.right, rightContext)(traverseLeftFunc)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Right, stop = true)
        case tree: ImmutableTreap.Node[K, V] =>
          val traverseUpFunc = up(UpwardStopPredicate.toNextKey, evalFunc)(contextOps)
          val contextExtract = ContextExtract.foldAfter(tree, context)(traverseUpFunc)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Up, stop = true)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }

  def prevKey[K, V, C <: NodeVisitContext[K, V]](
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit contextOps: NodeVisitContextOps[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) =>
      tree match {
        case tree: ImmutableTreap.NodeWithLeft[K, V] =>
          val traverseRightFunc = NodeDepthFirst.standard(NodeDepthFirst.rightOnlyNavigation, evalFunc)(contextOps)
          val leftContext = evalFunc(tree, context, BinaryTreeStep.Left)
          val contextExtract = ContextExtract.foldAfter(tree.left, leftContext)(traverseRightFunc)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Left, stop = true)
        case tree: ImmutableTreap.Node[K, V] =>
          val traverseUpFunc = up(UpwardStopPredicate.toPrevKey, evalFunc)(contextOps)
          val contextExtract = ContextExtract.foldAfter(tree, context)(traverseUpFunc)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Up, stop = true)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }

  def minKey[K, V, C](
    evalFunc: NodeEvalFunc[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) =>
      tree match {
        case tree: ImmutableTreap.NodeWithLeft[K, V] =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.Left)
          new NodeTraverseOutput(tree.left, newContext, BinaryTreeStep.Left, stop = false)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree,newContext, BinaryTreeStep.None, stop = true)
      }

  def maxKey[K, V, C](
    evalFunc: NodeEvalFunc[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) =>
      tree match {
        case tree: ImmutableTreap.NodeWithRight[K, V] =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.Left)
          new NodeTraverseOutput(tree.right, newContext, BinaryTreeStep.Left, stop = false)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }
}
