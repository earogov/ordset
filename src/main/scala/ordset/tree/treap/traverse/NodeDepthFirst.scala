package ordset.tree.treap.traverse

import ordset.tree.core.{BinaryTreeStep, BinaryTreeVisit}
import ordset.tree.treap.Treap
import ordset.tree.treap.eval.{NodeEvalFunc, NodeVisitContext, NodeVisitContextOps}

import scala.annotation.tailrec

object NodeDepthFirst {

  def standard[K, V, C <: NodeVisitContext[K, V]](
    navigateFunc: NodeNavigateFunc[K, V, C],
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit contextOps: NodeVisitContextOps[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) => {
      @tailrec
      def traverse(tree: Treap.Node[K, V], visits: BinaryTreeVisit.Type): NodeTraverseOutput[K, V, C] = {
        val step = navigateFunc(tree, contextOps.getContextWithVisits(context, visits))
        step match {
          case BinaryTreeStep.Left => tree match {
            case n: Treap.NodeWithLeft[K, V] =>
              new NodeTraverseOutput(n.left, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              traverse(tree, BinaryTreeVisit.addLeftVisit(visits))
          }
          case BinaryTreeStep.Right => tree match {
            case n: Treap.NodeWithRight[K, V] =>
              new NodeTraverseOutput(n.right, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              traverse(tree, BinaryTreeVisit.addRightVisit(visits))
          }
          case BinaryTreeStep.Up => context.stack match {
            case head :: _ =>
              new NodeTraverseOutput(head.tree, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              new NodeTraverseOutput(tree, evalFunc(tree, context, step), BinaryTreeStep.None, stop = true)
          }
          case BinaryTreeStep.None =>
            new NodeTraverseOutput(tree, evalFunc(tree, context, step), step, stop = true)
        }
      }
      traverse(tree, context.currentVisits)
    }

  def extended[K, V, C <: NodeVisitContext[K, V]](
    navigateFunc: NodeNavigateFunc[K, V, C],
    evalFunc: NodeEvalFunc[K, V, C],
    dummy: Treap.Node[K, V]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) => {
      val step = navigateFunc(tree, context)
      val newContext = evalFunc(tree, context, step)
      step match {
        case BinaryTreeStep.Left => tree match {
          case n: Treap.NodeWithLeft[K, V] =>
            new NodeTraverseOutput(n.left, newContext, step, stop = false)
          case _ =>
            new NodeTraverseOutput(dummy, newContext, step, stop = false)
        }
        case BinaryTreeStep.Right => tree match {
          case n: Treap.NodeWithRight[K, V] =>
            new NodeTraverseOutput(n.right, newContext, step, stop = false)
          case _ =>
            new NodeTraverseOutput(dummy, newContext, step, stop = false)
        }
        case BinaryTreeStep.Up => context.stack match {
          case head :: _ =>
            new NodeTraverseOutput(head.tree, newContext, step, stop = false)
          case _ =>
            new NodeTraverseOutput(tree, newContext, BinaryTreeStep.None, stop = true)
        }
        case BinaryTreeStep.None =>
          new NodeTraverseOutput(tree, newContext, step, stop = true)
      }
    }

  def leftFirstNavigate[K, V, C <: NodeVisitContext[K, V]]: NodeNavigateFunc[K, V, C] =
    LeftFirstNavigate.asInstanceOf[NodeNavigateFunc[K, V, C]]

  def leftOnlyNavigate[K, V, C <: NodeVisitContext[K, V]]: NodeNavigateFunc[K, V, C] =
    LeftOnlyNavigate.asInstanceOf[NodeNavigateFunc[K, V, C]]

  def rightFirstNavigate[K, V, C <: NodeVisitContext[K, V]]: NodeNavigateFunc[K, V, C] =
    RightFirstNavigate.asInstanceOf[NodeNavigateFunc[K, V, C]]

  def rightOnlyNavigate[K, V, C <: NodeVisitContext[K, V]]: NodeNavigateFunc[K, V, C] =
    RightOnlyNavigate.asInstanceOf[NodeNavigateFunc[K, V, C]]

  // PRIVATE SECTION
  private lazy val LeftFirstNavigate: NodeNavigateFunc[Any, Any, NodeVisitContext[Any, Any]] =
    (_, context) =>
      if (BinaryTreeVisit.isLeftUnvisited(context.currentVisits)) BinaryTreeStep.Left
      else if (BinaryTreeVisit.isRightUnvisited(context.currentVisits)) BinaryTreeStep.Right
      else BinaryTreeStep.Up

  private lazy val LeftOnlyNavigate: NodeNavigateFunc[Any, Any, NodeVisitContext[Any, Any]] =
    (_, context) =>
      if (BinaryTreeVisit.isLeftVisited(context.currentVisits)) BinaryTreeStep.None
      else BinaryTreeStep.Left

  private lazy val RightFirstNavigate: NodeNavigateFunc[Any, Any, NodeVisitContext[Any, Any]] =
    (_, context) =>
      if (BinaryTreeVisit.isRightUnvisited(context.currentVisits)) BinaryTreeStep.Right
      else if (BinaryTreeVisit.isLeftUnvisited(context.currentVisits)) BinaryTreeStep.Left
      else BinaryTreeStep.Up

  private lazy val RightOnlyNavigate: NodeNavigateFunc[Any, Any, NodeVisitContext[Any, Any]] =
    (_, context) =>
      if (BinaryTreeVisit.isRightVisited(context.currentVisits)) BinaryTreeStep.None
      else BinaryTreeStep.Right
}