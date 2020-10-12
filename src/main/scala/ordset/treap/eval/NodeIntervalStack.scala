package ordset.treap.eval

import ordset.domain.Domain
import ordset.{Bound, Interval, Show}
import ordset.treap.{Eval, TraverseStep, TraverseVisit, Treap}

object NodeIntervalStack {

  type Stack[E, D <: Domain[E]] = List[Element[E, D]]

  type EvalFunc[E, D <: Domain[E]] = Eval.DefaultFunc[E, D, Context[E, D]]

  class Element[E, D <: Domain[E]](
    val interval: Interval[E, D],
    override val tree: Treap[E, D],
    override val visits: TraverseVisit.Type
  ) extends NodeVisitStack.Element[E, D](
    tree = tree,
    visits = visits
  )

  class Context[E, D <: Domain[E]](
    val currentInterval: Interval[E, D],
    override val currentVisits: TraverseVisit.Type,
    override val stack: Stack[E, D]
  ) extends NodeVisitStack.Context[E, D](
    currentVisits = currentVisits,
    stack = stack
  )

  trait ContextOps[E, D <: Domain[E], C <: Context[E, D]] extends NodeVisitStack.ContextOps[E, D, C] {

    def leftVisitAdded(context: C): C

    def rightVisitAdded(context: C): C

    def withVisits(context: C, visits: TraverseVisit.Type): C

    def withInterval(context: C, interval: Interval[E, D]): C
  }

  def apply[E, D <: Domain[E]](): EvalFunc[E, D] = EvalFunc.asInstanceOf[EvalFunc[E, D]]

  def of[E, D <: Domain[E]](tree: Treap[E, D]): EvalFunc[E, D] = EvalFunc.asInstanceOf[EvalFunc[E, D]]

  implicit  def elementShow[E, D <: Domain[E]](
    implicit intervalShow: Show[Interval[E, D]], visitShow: Show[TraverseVisit.Type], treeShow: Show[Treap[E, D]]
  ): Show[Element[E, D]] =
    Show.show(e =>
      s"Element(" +
        s"tree: ${treeShow.show(e.tree)}, " +
        s"interval: ${intervalShow.show(e.interval)}, " +
        s"visits: ${visitShow.show(e.visits)})"
    )

  // Implicit function must be imported!
  // Stack is type alias, so implicit resolver doesn't search here for its implicits by default.
  implicit def stackShow[E, D <: Domain[E]](
    implicit elementShow: Show[Element[E, D]]
  ): Show[Stack[E, D]] =
    ordset.instances.List.listShow(elementShow)

  implicit def contextShow[E, D <: Domain[E]]()(
    implicit intervalShow: Show[Interval[E, D]], visitShow: Show[TraverseVisit.Type], stackShow: Show[Stack[E, D]]
  ): Show[Context[E, D]] =
    Show.show(c =>
      s"Context(" +
        s"currentInterval: ${intervalShow.show(c.currentInterval)}, " +
        s"currentVisits: ${visitShow.show(c.currentVisits)}, " +
        s"stack: ${stackShow.show(c.stack)})"
    )

  implicit def contextOps[E, D <: Domain[E]](
    context: Context[E, D]
  ): ContextOps[E, D, Context[E, D]] =
    ContextOpsImpl.asInstanceOf[ContextOps[E, D, Context[E, D]]]

  private lazy val ContextOpsImpl = new ContextOps[Any, Domain[Any], Context[Any, Domain[Any]]] {

    override def leftVisitAdded(
      context: Context[Any, Domain[Any]]
    ): Context[Any, Domain[Any]] =
      withVisits(context, TraverseVisit.addLeftVisit(context.currentVisits))

    override def rightVisitAdded(
      context: Context[Any, Domain[Any]]
    ): Context[Any, Domain[Any]] =
      withVisits(context, TraverseVisit.addRightVisit(context.currentVisits))

    override def withVisits(
      context: Context[Any, Domain[Any]],
      visits: TraverseVisit.Type
    ): Context[Any, Domain[Any]] =
      new Context(context.currentInterval, visits, context.stack)

    override def withInterval(
      context: Context[Any, Domain[Any]],
      interval: Interval[Any, Domain[Any]]
    ): Context[Any, Domain[Any]] =
      new Context(interval, context.currentVisits, context.stack)
  }

  private lazy val EvalFunc: EvalFunc[Any, Domain[Any]] =
    (tree, context, step) => {
      val domainOps = context.currentInterval.domainOps
      step match {
        // Step upward to the parent node (no matter from left or right subtree).
        //
        //                    A
        //       ---------------------------
        //               ↗
        //           B
        //       -------------]              context.currentInterval
        //                  A.key
        case TraverseStep.Up => context.stack match {
          case head :: tail =>
            new Context(head.interval, head.visits, tail)
          case _ =>
            new Context(domainOps.interval.universal, TraverseVisit.None, Nil)
        }
        // Step into left subtree.
        //
        //                    A
        //       --------------------------- context.currentInterval
        //               ↙
        //           B
        //       -------------]
        //                  A.key
        //
        // We should consistently include parent key either into left subtree interval or right.
        // Let's choose left, so upper bound should be included here.
        case TraverseStep.Left =>
          val interval = tree match {
            case tree: Treap.Node[Any, Domain[Any]] =>
              domainOps.intervalOps.cutAbove(
                Bound.Upper[Any](tree.key, isInclusive = true),
                context.currentInterval
              )
            case _ => domainOps.interval.empty
          }
          new Context(
            interval,
            TraverseVisit.None,
            new Element(interval, tree, TraverseVisit.addLeftVisit(context.currentVisits)) :: context.stack
          )
        // Step into right subtree.
        //
        //                    A
        //       --------------------------- context.currentInterval
        //                         ↘
        //                             B
        //                    (-------------
        //                  A.key
        //
        // We have chosen to include parent key into left subtree interval, so lower bound should not be included here.
        case TraverseStep.Right =>
          val interval = tree match {
            case tree: Treap.Node[Any, Domain[Any]] =>
              domainOps.intervalOps.cutBelow(
                Bound.Lower[Any](tree.key, isInclusive = false),
                context.currentInterval
              )
            case _ => domainOps.interval.empty
          }
          new Context(
            interval,
            TraverseVisit.None,
            new Element(interval, tree, TraverseVisit.addRightVisit(context.currentVisits)) :: context.stack
          )
        case TraverseStep.None =>
          context
      }
    }
}