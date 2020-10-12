package ordset.treap.traverse

import ordset.domain.Domain
import ordset.treap.{Eval, Navigate, Traverse, TraverseStep, TraverseVisit, Treap}
import ordset.treap.eval.NodeVisitStack

import scala.annotation.tailrec

object DepthFirst {

  type Context      [E, D <: Domain[E]]                      = NodeVisitStack.Context    [E, D]
  type ContextOps   [E, D <: Domain[E], C <: Context[E, D]]  = NodeVisitStack.ContextOps [E, D, C]
  type Output       [E, D <: Domain[E], C <: Context[E, D]]  = Traverse.DefaultOutput    [E, D, C]
  type NavigateFunc [E, D <: Domain[E]]                      = Navigate.DefaultFunc      [E, D, Context[E, D]]
  type EvalFunc     [E, D <: Domain[E], C <: Context[E, D]]  = Eval.DefaultFunc          [E, D, C]

  def withEmpty[E, D <: Domain[E], C <: Context[E, D]](
    navigateFunc: NavigateFunc[E, D],
    evalFunc: EvalFunc[E, D, C]
  ): Traverse.DefaultFunc[E, D, C] =
    (tree, context) => {
      val step = navigateFunc(tree, context)
      val output: Output[E, D, C] = step match {
        case TraverseStep.Left => tree match {
          case n: Treap.NodeWithLeft[E, D] => new Output(n.left, context, step, stop = false)
          case _ => new Output(Treap.Empty(), context, step, stop = false)
        }
        case TraverseStep.Right => tree match {
          case n: Treap.NodeWithRight[E, D] => new Output(n.right, context, step, stop = false)
          case _ => new Output(Treap.Empty(), context, step, stop = false)
        }
        case TraverseStep.Up => context.stack match {
          case head :: _ => new Output(head.tree, context, step, stop = false)
          case _ => new Output(Treap.Empty(), context, TraverseStep.None, stop = true)
        }
        case TraverseStep.None => new Output(tree, context, TraverseStep.None, stop = true)
      }
      output.eval(evalFunc)
    }

  def nonEmpty[E, D <: Domain[E], C <: Context[E, D]](
    navigateFunc: NavigateFunc[E, D],
    evalFunc: EvalFunc[E, D, C]
  )(
    implicit contextOps: ContextOps[E, D, C]
  ): Traverse.DefaultFunc[E, D, C] =
    (tree, context) => {
      @tailrec
      def traverse(tree: Treap[E, D], visits: TraverseVisit.Type): Output[E, D, C] = {
        val step = navigateFunc(tree, contextOps.withVisits(context, visits))
        step match {
          case TraverseStep.Left => tree match {
            case n: Treap.NodeWithLeft[E, D] => new Output(n.left, context, step, stop = false)
            case _ => traverse(tree, TraverseVisit.addLeftVisit(visits))
          }
          case TraverseStep.Right => tree match {
            case n: Treap.NodeWithRight[E, D] => new Output(n.right, context, step, stop = false)
            case _ => traverse(tree, TraverseVisit.addRightVisit(visits))
          }
          case TraverseStep.Up => context.stack match {
            case head :: _ => new Output(head.tree, context, step, stop = false)
            case _ => new Output(Treap.Empty(), context, TraverseStep.None, stop = true)
          }
          case TraverseStep.None => new Output(tree, context, TraverseStep.None, stop = true)
        }
      }
      traverse(tree, context.currentVisits).eval(evalFunc)
    }

  def leftFirstNavigate[E, D <: Domain[E]]: NavigateFunc[E, D] =
    LeftFirstNavigate.asInstanceOf[NavigateFunc[E, D]]

  def leftOnlyNavigate[E, D <: Domain[E]]: NavigateFunc[E, D] =
    LeftOnlyNavigate.asInstanceOf[NavigateFunc[E, D]]

  def rightFirstNavigate[E, D <: Domain[E]]: NavigateFunc[E, D] =
    RightFirstNavigate.asInstanceOf[NavigateFunc[E, D]]

  def rightOnlyNavigate[E, D <: Domain[E]]: NavigateFunc[E, D] =
    RightOnlyNavigate.asInstanceOf[NavigateFunc[E, D]]

  private lazy val LeftFirstNavigate: NavigateFunc[Any, Domain[Any]] =
    (tree, context) =>
      if (tree.isEmpty) TraverseStep.Up
      else context.currentVisits match {
        case TraverseVisit.None => TraverseStep.Left
        case TraverseVisit.Left => TraverseStep.Right
        case _ => TraverseStep.Up
      }

  private lazy val LeftOnlyNavigate: NavigateFunc[Any, Domain[Any]] =
    (tree, context) =>
      if (tree.isEmpty) TraverseStep.None
      else context.currentVisits match {
        case TraverseVisit.None => TraverseStep.Left
        case _ => TraverseStep.None
      }

  private lazy val RightFirstNavigate: NavigateFunc[Any, Domain[Any]] =
    (tree, context) =>
      if (tree.isEmpty) TraverseStep.Up
      else context.currentVisits match {
        case TraverseVisit.None => TraverseStep.Right
        case TraverseVisit.Left => TraverseStep.Left
        case _ => TraverseStep.Up
      }

  private lazy val RightOnlyNavigate: NavigateFunc[Any, Domain[Any]] =
    (tree, context) =>
      if (tree.isEmpty) TraverseStep.None
      else context.currentVisits match {
        case TraverseVisit.None => TraverseStep.Right
        case _ => TraverseStep.None
      }
}