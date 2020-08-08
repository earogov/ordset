package ordset.treap.eval

import ordset.Order
import ordset.treap.{Eval, TraverseStep, Treap}

object NodeStepStack {

  type StackElement[K, Ord <: Order[K]] = (Treap[K, Ord], TraverseStep.Type)
  type Stack[K, Ord <: Order[K]] = List[StackElement[K, Ord]]
  type EvalFunc[K, Ord <: Order[K]] = Eval.Func[K, Ord, Stack[K, Ord], TraverseStep.Type]

  def apply[K, Ord <: Order[K]](): EvalFunc[K, Ord] = EvalFunc.asInstanceOf[EvalFunc[K, Ord]]

  def of[K, Ord <: Order[K]](example: Treap[K, Ord]): EvalFunc[K, Ord] = EvalFunc.asInstanceOf[EvalFunc[K, Ord]]

  private lazy val EvalFunc: EvalFunc[Any, Order[Any]] =
    (tree, stack, step) => step match {
      case TraverseStep.Up => stack match {
        case _ :: tail => tail
        case _ => Nil
      }
      case TraverseStep.None => stack
      case _ => (tree, step) :: stack
    }
}