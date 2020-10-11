package ordset.treap.eval

import ordset.Order
import ordset.Show
import ordset.treap.{Eval, TraverseStep, TraverseVisit, Treap}

object NodeVisitStack {

  type Stack[K, Ord <: Order[K]] = List[Element[K, Ord]]
  type EvalFunc[K, Ord <: Order[K]] = Eval.Func[K, Ord, Context[K, Ord], TraverseStep.Type]

  class Element[K, Ord <: Order[K]](val tree: Treap[K, Ord], val visits: TraverseVisit.Type)

  class Context[K, Ord <: Order[K]](val currentVisits: TraverseVisit.Type, val stack: Stack[K, Ord])

  trait ContextOps[K, Ord <: Order[K], C <: Context[K, Ord]] {

    def leftVisitAdded(context: C): C

    def rightVisitAdded(context: C): C

    def withVisits(context: C, visits: TraverseVisit.Type): C
  }

  def apply[K, Ord <: Order[K]](): EvalFunc[K, Ord] = EvalFunc.asInstanceOf[EvalFunc[K, Ord]]

  def of[K, Ord <: Order[K]](tree: Treap[K, Ord]): EvalFunc[K, Ord] = EvalFunc.asInstanceOf[EvalFunc[K, Ord]]

  implicit  def elementShow[K, Ord <: Order[K]](
    implicit visitShow: Show[TraverseVisit.Type], treeShow: Show[Treap[K, Ord]]
  ): Show[Element[K, Ord]] =
    Show.show(e => s"Element(tree: ${treeShow.show(e.tree)}, visits: ${visitShow.show(e.visits)})")

  implicit def contextShow[K, Ord <: Order[K]](
    implicit visitShow: Show[TraverseVisit.Type], stackShow: Show[Stack[K, Ord]]
  ): Show[Context[K, Ord]] =
    Show.show(c => s"Context(currentVisits: ${visitShow.show(c.currentVisits)}, stack: ${stackShow.show(c.stack)})")

  implicit def contextOps[K, Ord <: Order[K]]: ContextOps[K, Ord, Context[K, Ord]] =
    ContextOpsImpl.asInstanceOf[ContextOps[K, Ord, Context[K, Ord]]]

  private lazy val ContextOpsImpl = new ContextOps[Any, Order[Any], Context[Any, Order[Any]]] {

    override def leftVisitAdded(context: Context[Any, Order[Any]]): Context[Any, Order[Any]] =
      withVisits(context, TraverseVisit.addLeftVisit(context.currentVisits))

    override def rightVisitAdded(context: Context[Any, Order[Any]]): Context[Any, Order[Any]] =
      withVisits(context, TraverseVisit.addRightVisit(context.currentVisits))

    override def withVisits(context: Context[Any, Order[Any]], visits: TraverseVisit.Type): Context[Any, Order[Any]] =
      new Context(visits, context.stack)
  }

  private lazy val EvalFunc: EvalFunc[Any, Order[Any]] =
    (tree, context, step) =>
      step match {
        case TraverseStep.Up => context.stack match {
          case head :: tail =>
            new Context(head.visits, tail)
          case _ =>
            new Context(TraverseVisit.None, Nil)
        }
        case TraverseStep.Left =>
          new Context(
            TraverseVisit.None,
            new Element(tree, TraverseVisit.addLeftVisit(context.currentVisits)) :: context.stack
          )
        case TraverseStep.Right =>
          new Context(
            TraverseVisit.None,
            new Element(tree, TraverseVisit.addRightVisit(context.currentVisits)) :: context.stack
          )
        case TraverseStep.None =>
          context
      }
}