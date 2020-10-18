package ordset.treap.eval

import ordset.Show
import ordset.domain.Domain
import ordset.treap.{Eval, TraverseStep, TraverseVisit, Treap}

object NodeVisitStack {

  type Stack[E, D <: Domain[E], W] = List[Element[E, D, W]]

  type EvalFunc[E, D <: Domain[E], W] = Eval.DefaultFunc[E, D, W, Context[E, D, W]]

  class Element[E, D <: Domain[E], W](val tree: Treap[E, D, W], val visits: TraverseVisit.Type)

  class Context[E, D <: Domain[E], W](val currentVisits: TraverseVisit.Type, val stack: Stack[E, D, W])

  trait ContextOps[E, D <: Domain[E], W, C <: Context[E, D, W]] {

    def leftVisitAdded(context: C): C

    def rightVisitAdded(context: C): C

    def withVisits(context: C, visits: TraverseVisit.Type): C
  }

  def apply[E, D <: Domain[E], W](): EvalFunc[E, D, W] = EvalFunc.asInstanceOf[EvalFunc[E, D, W]]

  def of[E, D <: Domain[E], W](tree: Treap[E, D, W]): EvalFunc[E, D, W] = EvalFunc.asInstanceOf[EvalFunc[E, D, W]]

  implicit  def elementShow[E, D <: Domain[E], W](
    implicit visitShow: Show[TraverseVisit.Type], treeShow: Show[Treap[E, D, W]]
  ): Show[Element[E, D, W]] =
    Show.show(e => s"Element(tree: ${treeShow.show(e.tree)}, visits: ${visitShow.show(e.visits)})")

  // Implicit function must be imported!
  // `Stack` is type alias, so implicit resolver doesn't search here for its implicits by default.
  implicit def stackShow[E, D <: Domain[E], W](
    implicit elementShow: Show[Element[E, D, W]]
  ): Show[Stack[E, D, W]] =
    ordset.instances.List.listShow(elementShow)

  implicit def contextShow[E, D <: Domain[E], W](
    implicit visitShow: Show[TraverseVisit.Type], stackShow: Show[Stack[E, D, W]]
  ): Show[Context[E, D, W]] =
    Show.show(c => s"Context(currentVisits: ${visitShow.show(c.currentVisits)}, stack: ${stackShow.show(c.stack)})")

  implicit def contextOps[E, D <: Domain[E], W]: ContextOps[E, D, W, Context[E, D, W]] =
    ContextOpsImpl.asInstanceOf[ContextOps[E, D, W, Context[E, D, W]]]

  private lazy val ContextOpsImpl = new ContextOps[Any, Domain[Any], Any, Context[Any, Domain[Any], Any]] {

    override def leftVisitAdded(
      context: Context[Any, Domain[Any], Any]
    ): Context[Any, Domain[Any], Any] =
      withVisits(context, TraverseVisit.addLeftVisit(context.currentVisits))

    override def rightVisitAdded(
      context: Context[Any, Domain[Any], Any]
    ): Context[Any, Domain[Any], Any] =
      withVisits(context, TraverseVisit.addRightVisit(context.currentVisits))

    override def withVisits(
      context: Context[Any, Domain[Any], Any], visits: TraverseVisit.Type
    ): Context[Any, Domain[Any], Any] =
      new Context(visits, context.stack)
  }

  private lazy val EvalFunc: EvalFunc[Any, Domain[Any], Any] =
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