package ordset.tree.treap.immutable.traverse

import ordset.tree.core.Traverse
import ordset.tree.core.BinaryTreeStep
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.{NodeEvalFunc, NodeVisitContext, NodeVisitContextOps}

object NodeAside {

  /**
   * Moves to the the next key.
   * If there is no next key in treap then current node is returned.
   */
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
          new Traverse.Output(contextExtract.tree, contextExtract.context, BinaryTreeStep.Right, stop = true)
        case _ =>
          val contextExtract = NodeUpward.foldToNextKey(tree, context, evalFunc)(contextOps)
          new Traverse.Output(contextExtract.tree, contextExtract.context, BinaryTreeStep.Up, stop = true)
      }

  /**
   * Moves to the the previous key.
   * If there is no previous key in treap then current node is returned.
   */
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
          new Traverse.Output(contextExtract.tree, contextExtract.context, BinaryTreeStep.Left, stop = true)
        case _ =>
          val contextExtract = NodeUpward.foldToPrevKey(tree, context, evalFunc)(contextOps)
          new Traverse.Output(contextExtract.tree, contextExtract.context, BinaryTreeStep.Up, stop = true)
      }

  /**
   * Moves to the minimal key in treap (the most left).
   */
  def minKeyFunc[K, V, C](
    evalFunc: NodeEvalFunc[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) =>
      tree match {
        case tree: ImmutableTreap.NodeWithLeft[K, V] =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.Left)
          new Traverse.Output(tree.left, newContext, BinaryTreeStep.Left, stop = false)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new Traverse.Output(tree,newContext, BinaryTreeStep.None, stop = true)
      }

  /**
   * Moves to the maximal key in treap (the most right).
   */
  def maxKeyFunc[K, V, C](
    evalFunc: NodeEvalFunc[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) =>
      tree match {
        case tree: ImmutableTreap.NodeWithRight[K, V] =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.Left)
          new Traverse.Output(tree.right, newContext, BinaryTreeStep.Left, stop = false)
        case _ =>
          val newContext = evalFunc(tree, context, BinaryTreeStep.None)
          new Traverse.Output(tree, newContext, BinaryTreeStep.None, stop = true)
      }
}
