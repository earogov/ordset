package ordset.treap.traverse

import ordset.Order
import ordset.treap.{Eval, Navigate, Traverse, TraverseStep, TraverseVisit, Treap}
import ordset.treap.eval.NodeVisitStack

import scala.annotation.tailrec

object DepthFirst {

  type Context[K, Ord <: Order[K]] = NodeVisitStack.Context[K, Ord]
  type ContextOps[K, Ord <: Order[K], C <: Context[K, Ord]] = NodeVisitStack.ContextOps[K, Ord, C]
  type Output[K, Ord <: Order[K], C <: Context[K, Ord]] = Traverse.Output[K, Ord, C, TraverseStep.Type]
  type NavigateFunc[K, Ord <: Order[K]] = Navigate.Func[K, Ord, Context[K, Ord], TraverseStep.Type]
  type EvalFunc[K, Ord <: Order[K], C <: Context[K, Ord]] = Eval.Func[K, Ord, C, TraverseStep.Type]

  def withEmpty[K, Ord <: Order[K], C <: Context[K, Ord]](
    navigateFunc: NavigateFunc[K, Ord],
    evalFunc: EvalFunc[K, Ord, C]
  ): Traverse.DefaultFunc[K, Ord, C] = (tree, context) => {
      val step = navigateFunc(tree, context)
      val output: Output[K, Ord, C] = step match {
        case TraverseStep.Left => tree match {
          case n: Treap.NodeWithLeft[K, Ord] => Traverse.Output(n.left, context, step, stop = false)
          case _ => Traverse.Output(Treap.Empty(), context, step, stop = false)
        }
        case TraverseStep.Right => tree match {
          case n: Treap.NodeWithRight[K, Ord] => Traverse.Output(n.right, context, step, stop = false)
          case _ => Traverse.Output(Treap.Empty(), context, step, stop = false)
        }
        case TraverseStep.Up => context.stack match {
          case head :: _ => Traverse.Output(head.tree, context, step, stop = false)
          case _ => Traverse.Output(Treap.Empty(), context, TraverseStep.None, stop = true)
        }
        case TraverseStep.None => Traverse.Output(tree, context, TraverseStep.None, stop = true)
      }
      output.eval(evalFunc)
    }

  def nonEmpty[K, Ord <: Order[K], C <: Context[K, Ord]](
    navigateFunc: NavigateFunc[K, Ord],
    evalFunc: EvalFunc[K, Ord, C]
  )(
    implicit contextOps: ContextOps[K, Ord, C]
  ): Traverse.DefaultFunc[K, Ord, C] = (tree, context) => {
      @tailrec
      def traverse(tree: Treap[K, Ord], visits: TraverseVisit.Type): Output[K, Ord, C] = {
        val step = navigateFunc(tree, contextOps.withVisits(context, visits))
        step match {
          case TraverseStep.Left => tree match {
            case n: Treap.NodeWithLeft[K, Ord] => Traverse.Output(n.left, context, step, stop = false)
            case _ => traverse(tree, TraverseVisit.addLeftVisit(visits))
          }
          case TraverseStep.Right => tree match {
            case n: Treap.NodeWithRight[K, Ord] => Traverse.Output(n.right, context, step, stop = false)
            case _ => traverse(tree, TraverseVisit.addRightVisit(visits))
          }
          case TraverseStep.Up => context.stack match {
            case head :: _ => Traverse.Output(head.tree, context, step, stop = false)
            case _ => Traverse.Output(Treap.Empty(), context, TraverseStep.None, stop = true)
          }
          case TraverseStep.None => Traverse.Output(tree, context, TraverseStep.None, stop = true)
        }
      }
      traverse(tree, context.currentVisits).eval(evalFunc)
    }

  def leftFirstNavigate[K, Ord <: Order[K]]: NavigateFunc[K, Ord] =
    LeftFirstNavigate.asInstanceOf[NavigateFunc[K, Ord]]

  def leftOnlyNavigate[K, Ord <: Order[K]]: NavigateFunc[K, Ord] =
    LeftOnlyNavigate.asInstanceOf[NavigateFunc[K, Ord]]

  def rightFirstNavigate[K, Ord <: Order[K]]: NavigateFunc[K, Ord] =
    RightFirstNavigate.asInstanceOf[NavigateFunc[K, Ord]]

  def rightOnlyNavigate[K, Ord <: Order[K]]: NavigateFunc[K, Ord] =
    RightOnlyNavigate.asInstanceOf[NavigateFunc[K, Ord]]

  private lazy val LeftFirstNavigate: NavigateFunc[Any, Order[Any]] = (tree, context) =>
    if (tree.isEmpty) TraverseStep.Up
    else context.currentVisits match {
      case TraverseVisit.None => TraverseStep.Left
      case TraverseVisit.Left => TraverseStep.Right
      case _ => TraverseStep.Up
    }

  private lazy val LeftOnlyNavigate: NavigateFunc[Any, Order[Any]] = (tree, context) =>
    if (tree.isEmpty) TraverseStep.None
    else context.currentVisits match {
      case TraverseVisit.None => TraverseStep.Left
      case _ => TraverseStep.None
    }

  private lazy val RightFirstNavigate: NavigateFunc[Any, Order[Any]] = (tree, context) =>
    if (tree.isEmpty) TraverseStep.Up
    else context.currentVisits match {
      case TraverseVisit.None => TraverseStep.Right
      case TraverseVisit.Left => TraverseStep.Left
      case _ => TraverseStep.Up
    }

  private lazy val RightOnlyNavigate: NavigateFunc[Any, Order[Any]] = (tree, context) =>
    if (tree.isEmpty) TraverseStep.None
    else context.currentVisits match {
      case TraverseVisit.None => TraverseStep.Right
      case _ => TraverseStep.None
    }
}