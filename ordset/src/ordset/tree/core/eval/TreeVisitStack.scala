package ordset.tree.core.eval

import ordset.Show
import ordset.tree.core.{BinaryTreeStep, BinaryTreeVisit, Eval}

object TreeVisitStack {

  type Stack[K, V, +Tree[KK, VV]] = List[Element[K, V, Tree]]

  type EvalFunc[K, V, Tree[KK, VV]] = Eval.BinaryFunc[K, V, Tree, Context[K, V, Tree]]

  class Element[K, V, +Tree[KK, VV]](val tree: Tree[K, V], val visits: BinaryTreeVisit.Type)

  class Context[K, V, +Tree[KK, VV]](val currentVisits: BinaryTreeVisit.Type, val stack: Stack[K, V, Tree])

  /**
   * Common operations for [[Context]].
   */
  trait ContextOps[K, V, Tree[KK, VV], C <: Context[K, V, Tree]]
    extends TreeStackOps[K, V, Tree, C] {

    override def isEmptyStack(context: C): Boolean = context.stack.isEmpty

    override def getEmptyContext: C

    override def getHeadTreeOrDefault(context: C, default: Tree[K, V]): Tree[K, V] =
      context.stack match {
        case head :: _ => head.tree
        case _ => default
      }

    override def getTailContextOrDefault(context: C, default: C): C

    def getContextWithStack(context: C, stack: Stack[K, V, Tree]): C

    def getContextWithVisits(context: C, visits: BinaryTreeVisit.Type): C

    def getContextWithLeftVisitAdded(context: C): C =
      getContextWithVisits(context, BinaryTreeVisit.addLeftVisit(context.currentVisits))

    def getContextWithRightVisitAdded(context: C): C =
      getContextWithVisits(context, BinaryTreeVisit.addRightVisit(context.currentVisits))

    def addLeftToStack(context: C, tree: Tree[K, V]): C

    def addRightToStack(context: C, tree: Tree[K, V]): C
  }

  /**
   * Create eval function for [[Context]].
   */
  def function[K, V, Tree[KK, VV]]: EvalFunc[K, V, Tree] =
    evalFuncInstance.asInstanceOf[EvalFunc[K, V, Tree]]

  /**
   * Create eval function for [[Context]].
   * @param tree any tree of appropriate type (is used to simplify type inference).
   */
  def functionFor[K, V, Tree[KK, VV]](tree: Tree[K, V]): EvalFunc[K, V, Tree] =
    evalFuncInstance.asInstanceOf[EvalFunc[K, V, Tree]]

  /**
   * Implicit [[Show]] instance for [[Element]].
   */
  implicit  def elementShow[K, V, Tree[KK, VV]](
    implicit visitShow: Show[BinaryTreeVisit.Type], treeShow: Show[Tree[K, V]]
  ): Show[Element[K, V, Tree]] =
    Show.show(e => s"Element(tree: ${treeShow.show(e.tree)}, visits: ${visitShow.show(e.visits)})")

  /**
   * Implicit [[Show]] instance for [[Stack]].
   */
  implicit def stackShow[K, V, Tree[KK, VV]](
    implicit elementShow: Show[Element[K, V, Tree]]
  ): Show[Stack[K, V, Tree]] =
    ordset.givens.list.listShow(elementShow)

  /**
   * Implicit [[Show]] instance for [[Context]].
   */
  implicit def contextShow[K, V, Tree[KK, VV]](
    implicit visitShow: Show[BinaryTreeVisit.Type], stackShow: Show[Stack[K, V, Tree]]
  ): Show[Context[K, V, Tree]] =
    Show.show(c => s"Context(currentVisits: ${visitShow.show(c.currentVisits)}, stack: ${stackShow.show(c.stack)})")

  /**
   * Implicit [[ContextOps]] instance for [[Context]].
   */
  implicit def contextOps[K, V, Tree[KK, VV]]: ContextOps[K, V, Tree, Context[K, V, Tree]] =
    contextOpsInstance.asInstanceOf[ContextOps[K, V, Tree, Context[K, V, Tree]]]

  // Private section ---------------------------------------------------------- //
  private lazy val contextOpsInstance: ContextOps[Any, Any, [KK, VV] =>> Any, Context[Any, Any, [KK, VV] =>> Any]] =
    makeContextOps[Any, Any, [KK, VV] =>> Any]

  private lazy val evalFuncInstance: EvalFunc[Any, Any, [KK, VV] =>> Any] =
    makeEvalFunc(contextOpsInstance)

  private def makeContextOps[K, V, Tree[KK, VV]]: ContextOps[K, V, Tree, Context[K, V, Tree]] =
    new ContextOps[K, V, Tree, Context[K, V, Tree]] {

      override def getEmptyContext: Context[K, V, Tree] =
        EmptyContext.asInstanceOf[Context[K, V, Tree]]

      override def getTailContextOrDefault(
        context: Context[K, V, Tree],
        default: Context[K, V, Tree]
      ): Context[K, V, Tree] =
        context.stack match {
          case head :: tail => new Context(head.visits, tail)
          case _ => default
        }

      override def getContextWithStack(
        context: Context[K, V, Tree],
        stack: Stack[K, V, Tree]
      ): Context[K, V, Tree] =
        new Context(context.currentVisits, stack)

      override def getContextWithVisits(
        context: Context[K, V, Tree],
        visits: BinaryTreeVisit.Type
      ): Context[K, V, Tree] =
        new Context(visits, context.stack)

      override def addLeftToStack(
        context: Context[K, V, Tree],
        tree: Tree[K, V]
      ): Context[K, V, Tree] =
        new Context[K, V, Tree](
          BinaryTreeVisit.None,
          context.stack.prepended(new Element[K, V, Tree](tree, BinaryTreeVisit.addLeftVisit(context.currentVisits)))
        )

      override def addRightToStack(
        context: Context[K, V, Tree],
        tree: Tree[K, V]
      ): Context[K, V, Tree] =
        new Context[K, V, Tree](
          BinaryTreeVisit.None,
          context.stack.prepended(new Element[K, V, Tree](tree, BinaryTreeVisit.addRightVisit(context.currentVisits)))
        )

      private lazy val EmptyContext: Context[Any, Any, [KK, VV] =>> Any] = new Context(BinaryTreeVisit.None, Nil)
    }

  private def makeEvalFunc[K, V, Tree[KK, VV]](
    contextOps: ContextOps[K, V, Tree, Context[K, V, Tree]]
  ): EvalFunc[K, V, Tree] =
    (tree, context, step) =>
      step match {
        case BinaryTreeStep.Up => contextOps.getTailContextOrEmpty(context)
        case BinaryTreeStep.Left => contextOps.addLeftToStack(context, tree)
        case BinaryTreeStep.Right => contextOps.addRightToStack(context, tree)
        case BinaryTreeStep.None => context
      }
}