package ordset.treap.eval

import ordset.Show
import ordset.domain.Domain
import ordset.treap.{Eval, TraverseStep, TraverseVisit, Treap}

object NodeVisitStack {

  type Stack[E, D <: Domain[E], W] = List[Element[E, D, W]]
  type EvalFunc[E, D <: Domain[E], W] = Eval.DefaultFunc[E, D, W, Context[E, D, W]]

  class Element[E, D <: Domain[E], W](val tree: Treap.Node[E, D, W], val visits: TraverseVisit.Type)

  class Context[E, D <: Domain[E], W](val currentVisits: TraverseVisit.Type, val stack: Stack[E, D, W])

  trait ContextOps[E, D <: Domain[E], W, C <: Context[E, D, W]] extends NodeStackOps[E, D, W, C] {

    override def isEmptyStack(context: C): Boolean = context.stack.isEmpty

    override def getEmptyContext: C

    override def getHeadNodeOrNull(context: C): Treap.Node[E, D, W] =
      context.stack match {
        case head :: _ => head.tree
        case _ => null
      }

    override def getHeadTree(context: C): Treap[E, D, W] =
      context.stack match {
        case head :: _ => head.tree
        case _ => Treap.Empty()
      }

    override def getParentContextOrNull(context: C): C

    override def getParentContext(context: C): C

    def getContextWithStack(context: C, stack: Stack[E, D, W]): C

    def getContextWithVisits(context: C, visits: TraverseVisit.Type): C

    def getContextWithLeftVisitAdded(context: C): C =
      getContextWithVisits(context, TraverseVisit.addLeftVisit(context.currentVisits))

    def getContextWithRightVisitAdded(context: C): C =
      getContextWithVisits(context, TraverseVisit.addRightVisit(context.currentVisits))

    def getLeftChildNodeContext(context: C, node: Treap.Node[E, D, W]): C

    def getLeftChildTreeContext(context: C, tree: Treap[E, D, W]): C =
      tree match {
        case node: Treap.Node[E, D, W] => getLeftChildNodeContext(context, node)
        case _ => context
      }

    def getRightChildNodeContext(context: C, node: Treap.Node[E, D, W]): C

    def getRightChildTreeContext(context: C, tree: Treap[E, D, W]): C =
      tree match {
        case node: Treap.Node[E, D, W] => getRightChildNodeContext(context, node)
        case _ => context
      }
  }

  def apply[E, D <: Domain[E], W](): EvalFunc[E, D, W] = EvalFunc.asInstanceOf[EvalFunc[E, D, W]]

  def of[E, D <: Domain[E], W](tree: Treap[E, D, W]): EvalFunc[E, D, W] = EvalFunc.asInstanceOf[EvalFunc[E, D, W]]

  implicit  def elementShow[E, D <: Domain[E], W](
    implicit visitShow: Show[TraverseVisit.Type], treeShow: Show[Treap.Node[E, D, W]]
  ): Show[Element[E, D, W]] =
    Show.show(e => s"Element(tree: ${treeShow.show(e.tree)}, visits: ${visitShow.show(e.visits)})")

  implicit def stackShow[E, D <: Domain[E], W](
    implicit elementShow: Show[Element[E, D, W]]
  ): Show[Stack[E, D, W]] =
    ordset.instances.List.listShow(elementShow)

  implicit def contextShow[E, D <: Domain[E], W](
    implicit visitShow: Show[TraverseVisit.Type], stackShow: Show[Stack[E, D, W]]
  ): Show[Context[E, D, W]] =
    Show.show(c => s"Context(currentVisits: ${visitShow.show(c.currentVisits)}, stack: ${stackShow.show(c.stack)})")

  implicit def contextOps[E, D <: Domain[E], W]: ContextOps[E, D, W, Context[E, D, W]] =
    ContextOps.asInstanceOf[ContextOps[E, D, W, Context[E, D, W]]]

  private lazy val ContextOps = new ContextOps[Any, Domain[Any], Any, Context[Any, Domain[Any], Any]] {

    override def getEmptyContext: Context[Any, Domain[Any], Any] =
      new Context(TraverseVisit.None, Nil)

    override def getParentContextOrNull(
      context: Context[Any, Domain[Any], Any]
    ): Context[Any, Domain[Any], Any] =
      context.stack match {
        case head :: tail => new Context(head.visits, tail)
        case _ => null
      }

    override def getParentContext(
      context: Context[Any, Domain[Any], Any]
    ): Context[Any, Domain[Any], Any] =
      context.stack match {
        case head :: tail => new Context(head.visits, tail)
        case _ => new Context(TraverseVisit.None, Nil)
      }

    override def getContextWithStack(
      context: Context[Any, Domain[Any], Any],
      stack: Stack[Any, Domain[Any], Any]
    ): Context[Any, Domain[Any], Any] =
      new Context(context.currentVisits, stack)

    override def getContextWithVisits(
      context: Context[Any, Domain[Any], Any],
      visits: TraverseVisit.Type
    ): Context[Any, Domain[Any], Any] =
      new Context(visits, context.stack)

    override def getLeftChildNodeContext(
      context: Context[Any, Domain[Any], Any],
      node: Treap.Node[Any, Domain[Any], Any]
    ): Context[Any, Domain[Any], Any] =
      new Context(
        TraverseVisit.None,
        new Element(node, TraverseVisit.addLeftVisit(context.currentVisits)) :: context.stack
      )

    override def getRightChildNodeContext(
      context: Context[Any, Domain[Any], Any],
      node: Treap.Node[Any, Domain[Any], Any]
    ): Context[Any, Domain[Any], Any] =
      new Context(
        TraverseVisit.None,
        new Element(node, TraverseVisit.addRightVisit(context.currentVisits)) :: context.stack
      )
  }

  private lazy val EvalFunc: EvalFunc[Any, Domain[Any], Any] =
    (tree, context, step) =>
      step match {
        case TraverseStep.Up => ContextOps.getParentContext(context)
        case TraverseStep.Left => tree match {
          case tree: Treap.Node[Any, Domain[Any], Any] => ContextOps.getLeftChildNodeContext(context, tree)
          case _ => context
        }
        case TraverseStep.Right => tree match {
          case tree: Treap.Node[Any, Domain[Any], Any] => ContextOps.getRightChildNodeContext(context, tree)
          case _ => context
        }
        case TraverseStep.None => context
      }
}