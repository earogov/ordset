package ordset.treap.traverse

import ordset.domain.Domain
import ordset.treap.{Eval, Navigate, Traverse, TraverseStep, TraverseVisit, Treap}
import ordset.treap.eval.NodeVisitStack

import scala.annotation.tailrec

object DepthFirst {

  type Context      [E, D <: Domain[E], W]                         = NodeVisitStack.Context    [E, D, W]
  type ContextOps   [E, D <: Domain[E], W, C <: Context[E, D, W]]  = NodeVisitStack.ContextOps [E, D, W, C]
  type Output       [E, D <: Domain[E], W, C <: Context[E, D, W]]  = Traverse.DefaultOutput    [E, D, W, C]
  type NavigateFunc [E, D <: Domain[E], W]                         = Navigate.DefaultFunc      [E, D, W, Context[E, D, W]]
  type EvalFunc     [E, D <: Domain[E], W, C <: Context[E, D, W]]  = Eval.DefaultFunc          [E, D, W, C]

  def withEmpty[E, D <: Domain[E], W, C <: Context[E, D, W]](
    navigateFunc: NavigateFunc[E, D, W],
    evalFunc: EvalFunc[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) => {
      val step = navigateFunc(tree, context)
      val newContext = evalFunc(tree, context, step)
      step match {
        case TraverseStep.Left => tree match {
          case n: Treap.NodeWithLeft[E, D, W] =>
            new Output(n.left, newContext, step, stop = false)
          case _ =>
            new Output(Treap.Empty(), newContext, step, stop = false)
        }
        case TraverseStep.Right => tree match {
          case n: Treap.NodeWithRight[E, D, W] =>
            new Output(n.right, newContext, step, stop = false)
          case _ =>
            new Output(Treap.Empty(), newContext, step, stop = false)
        }
        case TraverseStep.Up => context.stack match {
          case head :: _ =>
            new Output(head.tree, newContext, step, stop = false)
          case _ =>
            new Output(Treap.Empty(), newContext, step, stop = true)
        }
        case TraverseStep.None =>
          new Output(tree, newContext, step, stop = true)
      }
    }

  def nonEmpty[E, D <: Domain[E], W, C <: Context[E, D, W]](
    navigateFunc: NavigateFunc[E, D, W],
    evalFunc: EvalFunc[E, D, W, C]
  )(
    implicit contextOps: ContextOps[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) => {
      @tailrec
      def traverse(tree: Treap[E, D, W], visits: TraverseVisit.Type): Output[E, D, W, C] = {
        val step = navigateFunc(tree, contextOps.getContextWithVisits(context, visits))
        step match {
          case TraverseStep.Left => tree match {
            case n: Treap.NodeWithLeft[E, D, W] =>
              new Output(n.left, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              traverse(tree, TraverseVisit.addLeftVisit(visits))
          }
          case TraverseStep.Right => tree match {
            case n: Treap.NodeWithRight[E, D, W] =>
              new Output(n.right, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              traverse(tree, TraverseVisit.addRightVisit(visits))
          }
          case TraverseStep.Up => context.stack match {
            case head :: _ =>
              new Output(head.tree, evalFunc(tree, context, step), step, stop = false)
            case _ =>
              new Output(Treap.Empty(), evalFunc(tree, context, step), step, stop = true)
          }
          case TraverseStep.None =>
            new Output(tree, evalFunc(tree, context, step), step, stop = true)
        }
      }
      traverse(tree, context.currentVisits)
    }

  def leftFirstNavigate[E, D <: Domain[E], W]: NavigateFunc[E, D, W] =
    LeftFirstNavigate.asInstanceOf[NavigateFunc[E, D, W]]

  def leftOnlyNavigate[E, D <: Domain[E], W]: NavigateFunc[E, D, W] =
    LeftOnlyNavigate.asInstanceOf[NavigateFunc[E, D, W]]

  def rightFirstNavigate[E, D <: Domain[E], W]: NavigateFunc[E, D, W] =
    RightFirstNavigate.asInstanceOf[NavigateFunc[E, D, W]]

  def rightOnlyNavigate[E, D <: Domain[E], W]: NavigateFunc[E, D, W] =
    RightOnlyNavigate.asInstanceOf[NavigateFunc[E, D, W]]

  private lazy val LeftFirstNavigate: NavigateFunc[Any, Domain[Any], Any] =
    (tree, context) =>
      if (!tree.isNode) TraverseStep.Up
      else if (TraverseVisit.isLeftUnvisited(context.currentVisits)) TraverseStep.Left
      else if (TraverseVisit.isRightUnvisited(context.currentVisits)) TraverseStep.Right
      else TraverseStep.Up

  private lazy val LeftOnlyNavigate: NavigateFunc[Any, Domain[Any], Any] =
    (tree, context) =>
      if (!tree.isNode || TraverseVisit.isLeftVisited(context.currentVisits)) TraverseStep.None
      else TraverseStep.Left

  private lazy val RightFirstNavigate: NavigateFunc[Any, Domain[Any], Any] =
    (tree, context) =>
      if (!tree.isNode) TraverseStep.Up
      else if (TraverseVisit.isRightUnvisited(context.currentVisits)) TraverseStep.Right
      else if (TraverseVisit.isLeftUnvisited(context.currentVisits)) TraverseStep.Left
      else TraverseStep.Up

  private lazy val RightOnlyNavigate: NavigateFunc[Any, Domain[Any], Any] =
    (tree, context) =>
      if (!tree.isNode || TraverseVisit.isRightVisited(context.currentVisits)) TraverseStep.None
      else TraverseStep.Right
}