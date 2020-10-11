//package ordset.treap.eval
//
//import ordset.domain.Domain
//import ordset.{Interval, Order, Show}
//import ordset.treap.{Eval, TraverseStep, TraverseVisit, Treap}
//
//object NodeIntervalStack {
//
//  type Stack[K, D <: Domain[K]] = List[Element[K, D]]
//  type EvalFunc[K, D <: Domain[K]] = Eval.Func[K, D, Context[K, D], TraverseStep.Type]
//
//  class Element[K, D <: Domain[K]](
//    val interval: Interval[K, D],
//    override val tree: Treap[K, Ord],
//    override val visits: TraverseVisit.Type
//  ) extends NodeVisitStack.Element[K, Ord](
//    tree = tree,
//    visits = visits
//  )
//
//  class Context[K, Ord <: Order[K]](
//    val currentInterval: Interval[K],
//    override val currentVisits: TraverseVisit.Type,
//    override val stack: Stack[K, Ord]
//  ) extends NodeVisitStack.Context[K, Ord](
//    currentVisits = currentVisits,
//    stack = stack
//  )
//
//  trait ContextOps[K, Ord <: Order[K], C <: Context[K, Ord]] extends NodeVisitStack.ContextOps[K, Ord, C] {
//
//    def leftVisitAdded(context: C): C
//
//    def rightVisitAdded(context: C): C
//
//    def withVisits(context: C, visits: TraverseVisit.Type): C
//
//    def withInterval(context: C, interval: Interval[K]): C
//  }
//
//  def apply[K, Ord <: Order[K]](): EvalFunc[K, Ord] = EvalFunc.asInstanceOf[EvalFunc[K, Ord]]
//
//  def of[K, Ord <: Order[K]](tree: Treap[K, Ord]): EvalFunc[K, Ord] = EvalFunc.asInstanceOf[EvalFunc[K, Ord]]
//
//  implicit  def elementShow[K, Ord <: Order[K]](
//    implicit intervalShow: Show[Interval[K]], visitShow: Show[TraverseVisit.Type], treeShow: Show[Treap[K, Ord]]
//  ): Show[Element[K, Ord]] =
//    Show.show(e =>
//      s"Element(" +
//        s"tree: ${treeShow.show(e.tree)}, " +
//        s"interval: ${intervalShow.show(e.interval)}, " +
//        s"visits: ${visitShow.show(e.visits)})"
//    )
//
//  implicit def contextShow[K, Ord <: Order[K]]()(
//    implicit intervalShow: Show[Interval[K]], visitShow: Show[TraverseVisit.Type], stackShow: Show[Stack[K, Ord]]
//  ): Show[Context[K, Ord]] =
//    Show.show(c =>
//      s"Context(" +
//        s"currentInterval: ${intervalShow.show(c.currentInterval)}, " +
//        s"currentVisits: ${visitShow.show(c.currentVisits)}, " +
//        s"stack: ${stackShow.show(c.stack)})"
//    )
//
//  implicit def contextOps[K, Ord <: Order[K]](
//    context: Context[K, Ord]
//  ): ContextOps[K, Ord, Context[K, Ord]] =
//    ContextOpsImpl.asInstanceOf[ContextOps[K, Ord, Context[K, Ord]]]
//
//  private lazy val ContextOpsImpl = new ContextOps[Any, Order[Any], Context[Any, Order[Any]]] {
//
//    override def leftVisitAdded(context: Context[Any, Order[Any]]): Context[Any, Order[Any]] =
//      withVisits(context, TraverseVisit.addLeftVisit(context.currentVisits))
//
//    override def rightVisitAdded(context: Context[Any, Order[Any]]): Context[Any, Order[Any]] =
//      withVisits(context, TraverseVisit.addRightVisit(context.currentVisits))
//
//    override def withVisits(context: Context[Any, Order[Any]], visits: TraverseVisit.Type): Context[Any, Order[Any]] =
//      new Context(context.currentInterval, visits, context.stack)
//
//    override def withInterval(context: Context[Any, Order[Any]], interval: Interval[Any]): Context[Any, Order[Any]] =
//      new Context(interval, context.currentVisits, context.stack)
//  }
//
//  private lazy val EvalFunc: EvalFunc[Any, Order[Any]] =
//    (tree, context, step) =>
//      step match {
//        case TraverseStep.Up => context.stack match {
//          case head :: tail =>
//            new Context(head.interval, head.visits, tail)
//          case _ =>
//            new Context(Interval.Unbounded, TraverseVisit.None, Nil)
//        }
//        case TraverseStep.Left =>
//          val interval = context.currentInterval.cross(getLeftTreeInterval(tree))
//          new Context(
//            TraverseVisit.None,
//            new Element(interval, tree, TraverseVisit.addLeftVisit(context.currentVisits)) :: context.stack
//          )
//        case TraverseStep.Right =>
//          val interval = context.currentInterval.cross(getRightTreeInterval(tree))
//          new Context(
//            TraverseVisit.None,
//            new Element(interval, tree, TraverseVisit.addRightVisit(context.currentVisits)) :: context.stack
//          )
//        case TraverseStep.None =>
//          context
//      }
//
//  private def getLeftTreeInterval[K, Ord <: Order[K]](tree: Treap[K, Ord]): Interval[K] = tree match {
//    case n: Treap.Node[K, Ord] => Interval.leftUnbounded(n.key, isInclusive = true)
//    case _ => Interval.Empty
//  }
//
//  private def getRightTreeInterval[K, Ord <: Order[K]](tree: Treap[K, Ord]): Interval[K] = tree match {
//    case n: Treap.Node[K, Ord] => Interval.rightUnbounded(n.key, isInclusive = false)
//    case _ => Interval.Empty
//  }
//}