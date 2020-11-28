package ordset.tree.treap.traverse

import ordset.domain.Domain
import ordset.tree.core.BinaryTreeStep
import ordset.tree.core.reduce.ContextExtract
import ordset.tree.treap.Treap
import ordset.tree.treap.eval.{NodeEvalFunc, NodeStackOps, NodeVisitContext, NodeVisitContextOps}

object NodeSearch {

  trait UpwardStopPredicate[K, V] {

    def apply(parent: Treap.Node[K, V], child: Treap.Node[K, V]): Boolean
  }

  object UpwardStopPredicate {

    def toNextKey[K, V]: UpwardStopPredicate[K, V] = NextKeyInstance.asInstanceOf[UpwardStopPredicate[K, V]]

    def toPrevKey[K, V]: UpwardStopPredicate[K, V] = PrevKeyInstance.asInstanceOf[UpwardStopPredicate[K, V]]

    def never[K, V]: UpwardStopPredicate[K, V] = NeverInstance.asInstanceOf[UpwardStopPredicate[K, V]]

    def always[K, V]: UpwardStopPredicate[K, V] = AlwaysInstance.asInstanceOf[UpwardStopPredicate[K, V]]

    // PRIVATE SECTION
    private lazy val NextKeyInstance: UpwardStopPredicate[Any, Any] = (parent, tree) => parent.hasLeftInstance(tree)

    private lazy val PrevKeyInstance: UpwardStopPredicate[Any, Any] = (parent, tree) => parent.hasRightInstance(tree)

    private lazy val NeverInstance: UpwardStopPredicate[Any, Any] = (_, _) => false

    private lazy val AlwaysInstance: UpwardStopPredicate[Any, Any] = (_, _) => true
  }

  def down[K, V, C](
    key: K,
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit domain: Domain[K]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) => {
      val cmp = domain.elementOrd.compare(key, tree.key)
      if (cmp > 0) tree match {
        case n: Treap.NodeWithRight[K, V] =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.Right)
          new NodeTraverseOutput(n.right, newContext, BinaryTreeStep.Right, stop = false)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }
      else if (cmp < 0) tree match {
        case n: Treap.NodeWithLeft[K, V] =>
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
        case tree: Treap.NodeWithRight[K, V] =>
          val traverseLeftFunc = NodeDepthFirst.standard(NodeDepthFirst.leftOnlyNavigate, evalFunc)(contextOps)
          val rightContext = evalFunc(tree, context, BinaryTreeStep.Right)
          val contextExtract = ContextExtract.reduceAfter(tree.right, rightContext)(traverseLeftFunc)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Right, stop = true)
        case tree: Treap.Node[K, V] =>
          val traverseUpFunc = up(UpwardStopPredicate.toNextKey, evalFunc)(contextOps)
          val contextExtract = ContextExtract.reduceAfter(tree, context)(traverseUpFunc)
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
        case tree: Treap.NodeWithLeft[K, V] =>
          val traverseRightFunc = NodeDepthFirst.standard(NodeDepthFirst.rightOnlyNavigate, evalFunc)(contextOps)
          val leftContext = evalFunc(tree, context, BinaryTreeStep.Left)
          val contextExtract = ContextExtract.reduceAfter(tree.left, leftContext)(traverseRightFunc)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Left, stop = true)
        case tree: Treap.Node[K, V] =>
          val traverseUpFunc = up(UpwardStopPredicate.toPrevKey, evalFunc)(contextOps)
          val contextExtract = ContextExtract.reduceAfter(tree, context)(traverseUpFunc)
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
        case tree: Treap.NodeWithLeft[K, V] =>
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
        case tree: Treap.NodeWithRight[K, V] =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.Left)
          new NodeTraverseOutput(tree.right, newContext, BinaryTreeStep.Left, stop = false)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }
}
