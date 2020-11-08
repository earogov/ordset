package ordset.treap.eval

import ordset.Show
import ordset.domain.Domain
import ordset.treap.{Eval, TraverseStep, Treap}

object NodeStack {

  type Stack[E, D <: Domain[E], W] = List[Treap.Node[E, D, W]]
  type EvalFunc[E, D <: Domain[E], W] = Eval.DefaultFunc[E, D, W, Context[E, D, W]]
  type Context[E, D <: Domain[E], W] = Stack[E, D, W]

  trait ContextOps[E, D <: Domain[E], W] extends NodeStackOps[E, D, W, Context[E, D, W]] {

    override def isEmptyStack(context: Context[E, D, W]): Boolean = context.isEmpty

    override def getEmptyContext: Context[E, D, W] = Nil

    override def getHeadNodeOrNull(context: Context[E, D, W]): Treap.Node[E, D, W] =
      context match {
        case head :: _ => head
        case _ => null
      }

    override def getHeadTree(context: Context[E, D, W]): Treap[E, D, W] =
      context match {
        case head :: _ => head
        case _ => Treap.Empty()
      }

    override def getParentContextOrNull(context: Context[E, D, W]): Context[E, D, W] =
      context match {
        case _ :: tail => tail
        case _ => null
      }

    override def getParentContext(context: Context[E, D, W]): Context[E, D, W] =
      context match {
        case _ :: tail => tail
        case _ => getEmptyContext
      }

    def getChildNodeContext(context: Context[E, D, W], node: Treap.Node[E, D, W]): Context[E, D, W] =
      node :: context

    def getChildTreeContext(context: Context[E, D, W], tree: Treap[E, D, W]): Context[E, D, W] =
      tree match {
        case node: Treap.Node[E, D, W] => getChildNodeContext(context, node)
        case _ => context
      }
  }

  def apply[E, D <: Domain[E], W](): EvalFunc[E, D, W] = EvalFunc.asInstanceOf[EvalFunc[E, D, W]]

  def of[E, D <: Domain[E], W](tree: Treap[E, D, W]): EvalFunc[E, D, W] = EvalFunc.asInstanceOf[EvalFunc[E, D, W]]

  implicit def contextShow[E, D <: Domain[E], W](
    implicit treeShow: Show[Treap.Node[E, D, W]]
  ): Show[Context[E, D, W]] =
    ordset.instances.List.listShow(treeShow)

  implicit def contextOps[E, D <: Domain[E], W]: ContextOps[E, D, W] =
    ContextOps.asInstanceOf[ContextOps[E, D, W]]

  private lazy val ContextOps = new ContextOps[Any, Domain[Any], Any] {}

  private lazy val EvalFunc: EvalFunc[Any, Domain[Any], Any] =
    (tree, context, step) =>
      step match {
        case TraverseStep.Up => ContextOps.getParentContext(context)
        case TraverseStep.Left | TraverseStep.Right => tree match {
          case tree: Treap.Node[Any, Domain[Any], Any] => ContextOps.getChildNodeContext(context, tree)
          case _ => context
        }
        case TraverseStep.None => context
      }
}