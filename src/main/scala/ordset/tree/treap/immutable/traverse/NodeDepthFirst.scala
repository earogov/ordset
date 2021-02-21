package ordset.tree.treap.immutable.traverse

import ordset.tree.core.{BinaryTreeStep, BinaryTreeVisit}
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.{NodeEvalFunc, NodeVisitContext, NodeVisitContextOps}

import scala.annotation.tailrec

object NodeDepthFirst {

  def defaultFunc[K, V, C <: NodeVisitContext[K, V]](
    navigationFunc: NodeNavigationFunc[K, V, C],
    evalFunc: NodeEvalFunc[K, V, C]
  )(
    implicit contextOps: NodeVisitContextOps[K, V, C]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) => {
      @tailrec
      def traverse(tree: ImmutableTreap.Node[K, V], visits: BinaryTreeVisit.Type): NodeTraverseOutput[K, V, C] = {
        val step = navigationFunc(tree, contextOps.getContextWithVisits(context, visits))
        step match {
          case BinaryTreeStep.Left => tree match {
            case n: ImmutableTreap.NodeWithLeft[K, V] =>
              new NodeTraverseOutput(n.left, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              traverse(tree, BinaryTreeVisit.addLeftVisit(visits))
          }
          case BinaryTreeStep.Right => tree match {
            case n: ImmutableTreap.NodeWithRight[K, V] =>
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

  def extendedFunc[K, V, C <: NodeVisitContext[K, V]](
    navigationFunc: NodeNavigationFunc[K, V, C],
    evalFunc: NodeEvalFunc[K, V, C],
    dummy: ImmutableTreap.Node[K, V]
  ): NodeTraverseFunc[K, V, C] =
    (tree, context) => {
      val step = navigationFunc(tree, context)
      val newContext = evalFunc(tree, context, step)
      step match {
        case BinaryTreeStep.Left => tree match {
          case n: ImmutableTreap.NodeWithLeft[K, V] =>
            new NodeTraverseOutput(n.left, newContext, step, stop = false)
          case _ =>
            new NodeTraverseOutput(dummy, newContext, step, stop = false)
        }
        case BinaryTreeStep.Right => tree match {
          case n: ImmutableTreap.NodeWithRight[K, V] =>
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

  object Navigation {

    def leftFirstFunc[K, V, C <: NodeVisitContext[K, V]]: NodeNavigationFunc[K, V, C] =
      LeftFirstInstance.asInstanceOf[NodeNavigationFunc[K, V, C]]

    def leftOnlyFunc[K, V, C <: NodeVisitContext[K, V]]: NodeNavigationFunc[K, V, C] =
      LeftOnlyInstance.asInstanceOf[NodeNavigationFunc[K, V, C]]

    def rightFirstFunc[K, V, C <: NodeVisitContext[K, V]]: NodeNavigationFunc[K, V, C] =
      RightFirstInstance.asInstanceOf[NodeNavigationFunc[K, V, C]]

    def rightOnlyFunc[K, V, C <: NodeVisitContext[K, V]]: NodeNavigationFunc[K, V, C] =
      RightOnlyInstance.asInstanceOf[NodeNavigationFunc[K, V, C]]

    // PRIVATE SECTION
    private lazy val LeftFirstInstance: NodeNavigationFunc[Any, Any, NodeVisitContext[Any, Any]] =
      (_, context) =>
        if (BinaryTreeVisit.isLeftUnvisited(context.currentVisits)) BinaryTreeStep.Left
        else if (BinaryTreeVisit.isRightUnvisited(context.currentVisits)) BinaryTreeStep.Right
        else BinaryTreeStep.Up

    private lazy val LeftOnlyInstance: NodeNavigationFunc[Any, Any, NodeVisitContext[Any, Any]] =
      (_, context) =>
        if (BinaryTreeVisit.isLeftVisited(context.currentVisits)) BinaryTreeStep.None
        else BinaryTreeStep.Left

    private lazy val RightFirstInstance: NodeNavigationFunc[Any, Any, NodeVisitContext[Any, Any]] =
      (_, context) =>
        if (BinaryTreeVisit.isRightUnvisited(context.currentVisits)) BinaryTreeStep.Right
        else if (BinaryTreeVisit.isLeftUnvisited(context.currentVisits)) BinaryTreeStep.Left
        else BinaryTreeStep.Up

    private lazy val RightOnlyInstance: NodeNavigationFunc[Any, Any, NodeVisitContext[Any, Any]] =
      (_, context) =>
        if (BinaryTreeVisit.isRightVisited(context.currentVisits)) BinaryTreeStep.None
        else BinaryTreeStep.Right
  }
}