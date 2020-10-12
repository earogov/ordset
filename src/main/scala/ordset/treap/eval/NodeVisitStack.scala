package ordset.treap.eval

import ordset.Show
import ordset.domain.Domain
import ordset.treap.{Eval, TraverseStep, TraverseVisit, Treap}

object NodeVisitStack {

  type Stack[E, D <: Domain[E]] = List[Element[E, D]]

  type EvalFunc[E, D <: Domain[E]] = Eval.DefaultFunc[E, D, Context[E, D]]

  class Element[E, D <: Domain[E]](val tree: Treap[E, D], val visits: TraverseVisit.Type)

  class Context[E, D <: Domain[E]](val currentVisits: TraverseVisit.Type, val stack: Stack[E, D])

  trait ContextOps[E, D <: Domain[E], C <: Context[E, D]] {

    def leftVisitAdded(context: C): C

    def rightVisitAdded(context: C): C

    def withVisits(context: C, visits: TraverseVisit.Type): C
  }

  def apply[E, D <: Domain[E]](): EvalFunc[E, D] = EvalFunc.asInstanceOf[EvalFunc[E, D]]

  def of[E, D <: Domain[E]](tree: Treap[E, D]): EvalFunc[E, D] = EvalFunc.asInstanceOf[EvalFunc[E, D]]

  implicit  def elementShow[E, D <: Domain[E]](
    implicit visitShow: Show[TraverseVisit.Type], treeShow: Show[Treap[E, D]]
  ): Show[Element[E, D]] =
    Show.show(e => s"Element(tree: ${treeShow.show(e.tree)}, visits: ${visitShow.show(e.visits)})")

  // Implicit function must be imported!
  // Stack is type alias, so implicit resolver doesn't search here for its implicits by default.
  implicit def stackShow[E, D <: Domain[E]](
    implicit elementShow: Show[Element[E, D]]
  ): Show[Stack[E, D]] =
    ordset.instances.List.listShow(elementShow)

  implicit def contextShow[E, D <: Domain[E]](
    implicit visitShow: Show[TraverseVisit.Type], stackShow: Show[Stack[E, D]]
  ): Show[Context[E, D]] =
    Show.show(c => s"Context(currentVisits: ${visitShow.show(c.currentVisits)}, stack: ${stackShow.show(c.stack)})")

  implicit def contextOps[E, D <: Domain[E]]: ContextOps[E, D, Context[E, D]] =
    ContextOpsImpl.asInstanceOf[ContextOps[E, D, Context[E, D]]]

  private lazy val ContextOpsImpl = new ContextOps[Any, Domain[Any], Context[Any, Domain[Any]]] {

    override def leftVisitAdded(context: Context[Any, Domain[Any]]): Context[Any, Domain[Any]] =
      withVisits(context, TraverseVisit.addLeftVisit(context.currentVisits))

    override def rightVisitAdded(context: Context[Any, Domain[Any]]): Context[Any, Domain[Any]] =
      withVisits(context, TraverseVisit.addRightVisit(context.currentVisits))

    override def withVisits(context: Context[Any, Domain[Any]], visits: TraverseVisit.Type): Context[Any, Domain[Any]] =
      new Context(visits, context.stack)
  }

  private lazy val EvalFunc: EvalFunc[Any, Domain[Any]] =
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