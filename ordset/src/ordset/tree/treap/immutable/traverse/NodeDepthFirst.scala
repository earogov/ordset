package ordset.tree.treap.immutable.traverse

import ordset.tree.core.Traverse
import ordset.tree.core.{BinaryTreeStep, BinaryTreeVisit}
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.{NodeEvalFunc, NodeVisitContext, NodeVisitContextOps}

import scala.annotation.tailrec

object NodeDepthFirst {

  /**
   * Implements one step of depth first traverse.
   * Precedence of left/right visited can be customized with `navigationFunc`.
   */
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
              new Traverse.Output(n.left, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              traverse(tree, BinaryTreeVisit.addLeftVisit(visits))
          }
          case BinaryTreeStep.Right => tree match {
            case n: ImmutableTreap.NodeWithRight[K, V] =>
              new Traverse.Output(n.right, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              traverse(tree, BinaryTreeVisit.addRightVisit(visits))
          }
          case BinaryTreeStep.Up => context.stack match {
            case head :: _ =>
              new Traverse.Output(head.tree, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              new Traverse.Output(tree, evalFunc(tree, context, step), BinaryTreeStep.None, stop = true)
          }
          case BinaryTreeStep.None =>
            new Traverse.Output(tree, evalFunc(tree, context, step), step, stop = true)
        }
      }
      traverse(tree, context.currentVisits)
    }

  /**
   * Same as [[defaultFunc]] but returns `dummy` node when child node is absent
   * (and should be visited according to depth first order).
   */
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
            new Traverse.Output(n.left, newContext, step, stop = false)
          case _ =>
            new Traverse.Output(dummy, newContext, step, stop = false)
        }
        case BinaryTreeStep.Right => tree match {
          case n: ImmutableTreap.NodeWithRight[K, V] =>
            new Traverse.Output(n.right, newContext, step, stop = false)
          case _ =>
            new Traverse.Output(dummy, newContext, step, stop = false)
        }
        case BinaryTreeStep.Up => context.stack match {
          case head :: _ =>
            new Traverse.Output(head.tree, newContext, step, stop = false)
          case _ =>
            new Traverse.Output(tree, newContext, BinaryTreeStep.None, stop = true)
        }
        case BinaryTreeStep.None =>
          new Traverse.Output(tree, newContext, step, stop = true)
      }
    }

  object Navigation {

    /**
     * Specifies traverse order: left children before right.
     */
    def leftFirstFunc[K, V, C <: NodeVisitContext[K, V]]: NodeNavigationFunc[K, V, C] =
      leftFirstInstance.asInstanceOf[NodeNavigationFunc[K, V, C]]

    /**
     * Specifies traverse order: right children before left.
     */
    def leftOnlyFunc[K, V, C <: NodeVisitContext[K, V]]: NodeNavigationFunc[K, V, C] =
      leftOnlyInstance.asInstanceOf[NodeNavigationFunc[K, V, C]]

    /**
     * Specifies traverse order: only left children.
     */
    def rightFirstFunc[K, V, C <: NodeVisitContext[K, V]]: NodeNavigationFunc[K, V, C] =
      rightFirstInstance.asInstanceOf[NodeNavigationFunc[K, V, C]]

    /**
     * Specifies traverse order: only right children.
     */
    def rightOnlyFunc[K, V, C <: NodeVisitContext[K, V]]: NodeNavigationFunc[K, V, C] =
      rightOnlyInstance.asInstanceOf[NodeNavigationFunc[K, V, C]]

    // Private section ---------------------------------------------------------- //
    private lazy val leftFirstInstance: NodeNavigationFunc[Any, Any, NodeVisitContext[Any, Any]] =
      (_, context) =>
        if (BinaryTreeVisit.isLeftUnvisited(context.currentVisits)) BinaryTreeStep.Left
        else if (BinaryTreeVisit.isRightUnvisited(context.currentVisits)) BinaryTreeStep.Right
        else BinaryTreeStep.Up

    private lazy val leftOnlyInstance: NodeNavigationFunc[Any, Any, NodeVisitContext[Any, Any]] =
      (_, context) =>
        if (BinaryTreeVisit.isLeftVisited(context.currentVisits)) BinaryTreeStep.None
        else BinaryTreeStep.Left

    private lazy val rightFirstInstance: NodeNavigationFunc[Any, Any, NodeVisitContext[Any, Any]] =
      (_, context) =>
        if (BinaryTreeVisit.isRightUnvisited(context.currentVisits)) BinaryTreeStep.Right
        else if (BinaryTreeVisit.isLeftUnvisited(context.currentVisits)) BinaryTreeStep.Left
        else BinaryTreeStep.Up

    private lazy val rightOnlyInstance: NodeNavigationFunc[Any, Any, NodeVisitContext[Any, Any]] =
      (_, context) =>
        if (BinaryTreeVisit.isRightVisited(context.currentVisits)) BinaryTreeStep.None
        else BinaryTreeStep.Right
  }
}