package ordset.treap.traverse

import ordset.Order
import ordset.treap.{Navigate, Traverse, TraverseStep, TraverseVisit, Treap}
import ordset.treap.eval.NodeVisitStack

import scala.annotation.tailrec

object DepthFirst {

  type Context[K, Ord <: Order[K]] = NodeVisitStack.Context[K, Ord]
  type Output[K, Ord <: Order[K]] = Traverse.Output[K, Ord, Context[K, Ord], TraverseStep.Type]
  type NavigateFunc[K, Ord <: Order[K]] = Navigate.Func[K, Ord, Context[K, Ord], TraverseStep.Type]

  def withEmpty[K, Ord <: Order[K]](
    navigate: NavigateFunc[K, Ord]
  ): Traverse.DefaultFunc[K, Ord, Context[K, Ord]] = (tree, context) => {
      val step = navigate(tree, context)
      val output: Output[K, Ord] = step match {
        case TraverseStep.Left => tree match {
          case n: Treap.NodeWithLeft[K, Ord] => Traverse.Output(context, n.left, step, stop = false)
          case _ => Traverse.Output(context, Treap.Empty(), step, stop = false)
        }
        case TraverseStep.Right => tree match {
          case n: Treap.NodeWithRight[K, Ord] => Traverse.Output(context, n.right, step, stop = false)
          case _ => Traverse.Output(context, Treap.Empty(), step, stop = false)
        }
        case TraverseStep.Up => context.stack match {
          case head :: _ => Traverse.Output(context, head._1, step, stop = false)
          case _ => Traverse.Output(context, Treap.Empty(), TraverseStep.None, stop = true)
        }
        case TraverseStep.None => Traverse.Output(context, tree, TraverseStep.None, stop = true)
      }
      output.eval(NodeVisitStack())
    }

  def nonEmpty[K, Ord <: Order[K]](
    navigate: NavigateFunc[K, Ord]
  ): Traverse.DefaultFunc[K, Ord, Context[K, Ord]] = (tree, context) => {
      @tailrec
      def traverse(tree: Treap[K, Ord], context: Context[K, Ord]): Output[K, Ord] = {
        val step = navigate(tree, context)
        step match {
          case TraverseStep.Left => tree match {
            case n: Treap.NodeWithLeft[K, Ord] => Traverse.Output(context, n.left, step, stop = false)
            case _ => traverse(tree, context.leftVisitAdded())
          }
          case TraverseStep.Right => tree match {
            case n: Treap.NodeWithRight[K, Ord] => Traverse.Output(context, n.right, step, stop = false)
            case _ => traverse(tree, context.rightVisitAdded())
          }
          case TraverseStep.Up => context.stack match {
            case head :: _ => Traverse.Output(context, head._1, step, stop = false)
            case _ => Traverse.Output(context, Treap.Empty(), TraverseStep.None, stop = true)
          }
          case TraverseStep.None => Traverse.Output(context, tree, TraverseStep.None, stop = true)
        }
      }
      traverse(tree, context).eval(NodeVisitStack())
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