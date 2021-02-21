package ordset.tree.treap.immutable.traverse

import ordset.tree.core.BinaryTreeStep
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.{NodeEvalFunc, NodeVisitContext, NodeVisitContextOps}

object NodeAside {

  def nextKeyFunc[K, V, C <: NodeVisitContext[K, V]](
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit contextOps: NodeVisitContextOps[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) =>
      tree match {
        case tree: ImmutableTreap.NodeWithRight[K, V] =>
          val traverseLeftFunc = NodeDepthFirst.defaultFunc(NodeDepthFirst.Navigation.leftOnlyFunc, evalFunc)(contextOps)
          val rightContext = evalFunc(tree, context, BinaryTreeStep.Right)
          val contextExtract = ContextExtract.foldAfter(tree.right, rightContext)(traverseLeftFunc)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Right, stop = true)
        case tree: ImmutableTreap.Node[K, V] =>
          val contextExtract = NodeUpward.foldToNextKey(tree, context, evalFunc)(contextOps)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Up, stop = true)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }

  def prevKeyFunc[K, V, C <: NodeVisitContext[K, V]](
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit contextOps: NodeVisitContextOps[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) =>
      tree match {
        case tree: ImmutableTreap.NodeWithLeft[K, V] =>
          val traverseRightFunc = NodeDepthFirst.defaultFunc(NodeDepthFirst.Navigation.rightOnlyFunc, evalFunc)(contextOps)
          val leftContext = evalFunc(tree, context, BinaryTreeStep.Left)
          val contextExtract = ContextExtract.foldAfter(tree.left, leftContext)(traverseRightFunc)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Left, stop = true)
        case tree: ImmutableTreap.Node[K, V] =>
          val contextExtract = NodeUpward.foldToPrevKey(tree, context, evalFunc)(contextOps)
          new NodeTraverseOutput(contextExtract.tree, contextExtract.context, BinaryTreeStep.Up, stop = true)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
      }

  def minKeyFunc[K, V, C](
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

  def maxKeyFunc[K, V, C](
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
