package ordset.tree.core.eval

import ordset.Show
import ordset.tree.core.{BinaryTreeStep, Eval}

object TreeStack {

  type Stack[K, V, +Tree[KK, VV]] = List[Tree[K, V]]

  type Context[K, V, +Tree[KK, VV]] = Stack[K, V, Tree]

  type EvalFunc[K, V, Tree[KK, VV]] = Eval.BinaryFunc[K, V, Tree, Context[K, V, Tree]]

  /**
   * Common operations for [[Context]].
   */
  trait ContextOps[K, V, Tree[KK, VV]] extends TreeStackOps[K, V, Tree, Context[K, V, Tree]] {

    override def isEmptyStack(context: Context[K, V, Tree]): Boolean = context.isEmpty

    override def getEmptyContext: Context[K, V, Tree] = Nil

    override def getHeadTreeOrDefault(
      context: Context[K, V, Tree],
      default: Tree[K, V]
    ): Tree[K, V] =
      context match {
        case head :: _ => head
        case _ => default
      }

    override def getTailContextOrDefault(
      context: Context[K, V, Tree],
      default: Context[K, V, Tree]
    ): Context[K, V, Tree] =
      context match {
        case _ :: tail => tail
        case _ => default
      }

    def addToStack(context: Context[K, V, Tree], tree: Tree[K, V]): Context[K, V, Tree] =
      tree :: context
  }

  /**
   * Create eval function for [[Context]].
   */
  def function[K, V, Tree[KK, VV]](): EvalFunc[K, V, Tree] =
    EvalFunc.asInstanceOf[EvalFunc[K, V, Tree]]

  /**
   * Create eval function for [[Context]].
   * @param tree any tree of appropriate type (is used to simplify type inference).
   */
  def functionFor[K, V, Tree[KK, VV]](tree: Tree[K, V]): EvalFunc[K, V, Tree] =
    EvalFunc.asInstanceOf[EvalFunc[K, V, Tree]]

  /**
   * Implicit [[Show]] instance for [[Context]].
   */
  implicit def contextShow[K, V, Tree[KK, VV]](
    implicit treeShow: Show[Tree[K, V]]
  ): Show[Context[K, V, Tree]] =
    ordset.core.instances.List.listShow(treeShow)

  /**
   * Implicit [[ContextOps]] instance for [[Context]].
   */
  implicit def contextOps[K, V, Tree[KK, VV]]: ContextOps[K, V, Tree] =
    ContextOps.asInstanceOf[ContextOps[K, V, Tree]]

  // PRIVATE SECTION
  private lazy val ContextOps: ContextOps[Any, Any, Any] = new ContextOps[Any, Any, Any] {}

  private lazy val EvalFunc: EvalFunc[Any, Any, Any] = makeEvalFunc(ContextOps)

  private def makeEvalFunc[K, V, Tree[KK, VV]](
    contextOps: ContextOps[K, V, Tree]
  ): EvalFunc[K, V, Tree] =
    (tree, context, step) =>
      step match {
        case BinaryTreeStep.Up => contextOps.getTailContextOrEmpty(context)
        case BinaryTreeStep.Left | BinaryTreeStep.Right => contextOps.addToStack(context, tree)
        case BinaryTreeStep.None => context
      }
}