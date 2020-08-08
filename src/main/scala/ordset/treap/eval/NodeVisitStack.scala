package ordset.treap.eval

import ordset.Order
import ordset.Show
import ordset.treap.{Eval, TraverseStep, TraverseVisit, Treap}

object NodeVisitStack {

  type StackElement[K, Ord <: Order[K]] = (Treap[K, Ord], TraverseVisit.Type)
  type Stack[K, Ord <: Order[K]] = List[StackElement[K, Ord]]
  type EvalFunc[K, Ord <: Order[K]] = Eval.Func[K, Ord, Context[K, Ord], TraverseStep.Type]

  case class Context[K, Ord <: Order[K]](currentVisits: TraverseVisit.Type, stack: Stack[K, Ord]) {

    def leftVisitAdded(): Context[K, Ord] = Context(TraverseVisit.addLeftVisit(currentVisits), stack)

    def rightVisitAdded(): Context[K, Ord] = Context(TraverseVisit.addRightVisit(currentVisits), stack)
  }

  def apply[K, Ord <: Order[K]](): EvalFunc[K, Ord] = EvalFunc.asInstanceOf[EvalFunc[K, Ord]]

  def of[K, Ord <: Order[K]](tree: Treap[K, Ord]): EvalFunc[K, Ord] = EvalFunc.asInstanceOf[EvalFunc[K, Ord]]

  implicit def contextShow[K, Ord <: Order[K]](
    implicit visitShow: Show[TraverseVisit.Type], stackShow: Show[Stack[K, Ord]]
  ): Show[Context[K, Ord]] =
    Show.show(c => s"Context(currentVisits: ${visitShow.show(c.currentVisits)}, stack: ${stackShow.show(c.stack)})")

  private lazy val EvalFunc: EvalFunc[Any, Order[Any]] =
    (tree, context, step) =>
      step match {
        case TraverseStep.Up => context.stack match {
          case head :: tail =>
            Context(head._2, tail)
          case _ =>
            Context(TraverseVisit.None, Nil)
        }
        case TraverseStep.Left =>
          Context(
            TraverseVisit.None,
            (tree, TraverseVisit.addLeftVisit(context.currentVisits)) :: context.stack
          )
        case TraverseStep.Right =>
          Context(
            TraverseVisit.None,
            (tree, TraverseVisit.addRightVisit(context.currentVisits)) :: context.stack
          )
        case TraverseStep.None =>
          context
      }
}