//package ordset.treap.eval
//
//import ordset.domain.Domain
//import ordset.{Bound, Interval, Show}
//import ordset.treap.{Eval, TraverseStep, TraverseVisit, Treap}
//
//object NodeIntervalStack {
//
//  type Stack[E, D <: Domain[E], W] = List[Element[E, D, W]]
//
//  type EvalFunc[E, D <: Domain[E], W] = Eval.DefaultFunc[E, D, W, Context[E, D, W]]
//
//  class Element[E, D <: Domain[E], W](
//    val interval: Interval[E, D],
//    override val tree: Treap[E, D, W],
//    override val visits: TraverseVisit.Type
//  ) extends NodeVisitStack.Element[E, D, W](
//    tree = tree,
//    visits = visits
//  )
//
//  class Context[E, D <: Domain[E], W](
//    val currentInterval: Interval[E, D],
//    override val currentVisits: TraverseVisit.Type,
//    override val stack: Stack[E, D, W]
//  ) extends NodeVisitStack.Context[E, D, W](
//    currentVisits = currentVisits,
//    stack = stack
//  )
//
//  trait ContextOps[E, D <: Domain[E], W, C <: Context[E, D, W]] extends NodeVisitStack.ContextOps[E, D, W, C] {
//
//    def withInterval(context: C, interval: Interval[E, D]): C
//  }
//
//  def apply[E, D <: Domain[E], W](): EvalFunc[E, D, W] = EvalFunc.asInstanceOf[EvalFunc[E, D, W]]
//
//  def of[E, D <: Domain[E], W](tree: Treap[E, D, W]): EvalFunc[E, D, W] = EvalFunc.asInstanceOf[EvalFunc[E, D, W]]
//
//  implicit  def elementShow[E, D <: Domain[E], W](
//    implicit intervalShow: Show[Interval[E, D]], visitShow: Show[TraverseVisit.Type], treeShow: Show[Treap[E, D, W]]
//  ): Show[Element[E, D, W]] =
//    Show.show(e =>
//      s"Element(" +
//        s"tree: ${treeShow.show(e.tree)}, " +
//        s"interval: ${intervalShow.show(e.interval)}, " +
//        s"visits: ${visitShow.show(e.visits)})"
//    )
//
//  // Implicit function must be imported!
//  // `Stack` is type alias, so implicit resolver doesn't search here for its implicits by default.
//  implicit def stackShow[E, D <: Domain[E], W](
//    implicit elementShow: Show[Element[E, D, W]]
//  ): Show[Stack[E, D, W]] =
//    ordset.instances.List.listShow(elementShow)
//
//  implicit def contextShow[E, D <: Domain[E], W]()(
//    implicit intervalShow: Show[Interval[E, D]], visitShow: Show[TraverseVisit.Type], stackShow: Show[Stack[E, D, W]]
//  ): Show[Context[E, D, W]] =
//    Show.show(c =>
//      s"Context(" +
//        s"currentInterval: ${intervalShow.show(c.currentInterval)}, " +
//        s"currentVisits: ${visitShow.show(c.currentVisits)}, " +
//        s"stack: ${stackShow.show(c.stack)})"
//    )
//
//  implicit def contextOps[E, D <: Domain[E], W]: ContextOps[E, D, W, Context[E, D, W]] =
//    ContextOpsImpl.asInstanceOf[ContextOps[E, D, W, Context[E, D, W]]]
//
//  private lazy val ContextOpsImpl = new ContextOps[Any, Domain[Any], Any, Context[Any, Domain[Any], Any]] {
//
//    override def leftVisitAdded(
//      context: Context[Any, Domain[Any], Any]
//    ): Context[Any, Domain[Any], Any] =
//      withVisits(context, TraverseVisit.addLeftVisit(context.currentVisits))
//
//    override def rightVisitAdded(
//      context: Context[Any, Domain[Any], Any]
//    ): Context[Any, Domain[Any], Any] =
//      withVisits(context, TraverseVisit.addRightVisit(context.currentVisits))
//
//    override def withVisits(
//      context: Context[Any, Domain[Any], Any],
//      visits: TraverseVisit.Type
//    ): Context[Any, Domain[Any], Any] =
//      new Context(context.currentInterval, visits, context.stack)
//
//    override def withInterval(
//      context: Context[Any, Domain[Any], Any],
//      interval: Interval[Any, Domain[Any]]
//    ): Context[Any, Domain[Any], Any] =
//      new Context(interval, context.currentVisits, context.stack)
//
//    override def emptyContext(): Context[Any, Domain[Any], Any] =
//      new Context(TraverseVisit.None, Nil)
//  }
//
//  private lazy val EvalFunc: EvalFunc[Any, Domain[Any], Any] =
//    (tree, context, step) => {
//      val domainOps = context.currentInterval.domainOps
//      step match {
//        // Step upward to the parent node (no matter from left or right subtree).
//        //
//        //                    A
//        //       ---------------------------
//        //               ↗
//        //           B
//        //       -------------]              context.currentInterval
//        //                  A.key
//        case TraverseStep.Up => context.stack match {
//          case head :: tail =>
//            new Context(head.interval, head.visits, tail)
//          case _ =>
//            new Context(domainOps.interval.universal, TraverseVisit.None, Nil)
//        }
//        // Step into left subtree.
//        //
//        //                    A
//        //       --------------------------- context.currentInterval
//        //               ↙
//        //           B
//        //       -------------]
//        //                  A.key
//        //
//        // We should consistently include parent key either into left subtree interval or right.
//        // Let's choose left, so upper bound should be included here.
//        case TraverseStep.Left =>
//          val interval = tree match {
//            case tree: Treap.Node[Any, Domain[Any], Any] =>
//              domainOps.intervalOps.cutAbove(
//                Bound.Upper[Any](tree.key, isInclusive = true),
//                context.currentInterval
//              )
//            case _ => domainOps.interval.empty
//          }
//          val element = new Element(context.currentInterval, tree, TraverseVisit.addLeftVisit(context.currentVisits))
//          new Context(interval, TraverseVisit.None, element :: context.stack)
//        // Step into right subtree.
//        //
//        //                    A
//        //       --------------------------- context.currentInterval
//        //                         ↘
//        //                             B
//        //                    (-------------
//        //                  A.key
//        //
//        // We have chosen to include parent key into left subtree interval, so lower bound should not be included here.
//        case TraverseStep.Right =>
//          val interval = tree match {
//            case tree: Treap.Node[Any, Domain[Any], Any] =>
//              domainOps.intervalOps.cutBelow(
//                Bound.Lower[Any](tree.key, isInclusive = false),
//                context.currentInterval
//              )
//            case _ => domainOps.interval.empty
//          }
//          val element = new Element(context.currentInterval, tree, TraverseVisit.addRightVisit(context.currentVisits))
//          new Context(interval, TraverseVisit.None, element :: context.stack)
//        case TraverseStep.None =>
//          context
//      }
//    }
//}