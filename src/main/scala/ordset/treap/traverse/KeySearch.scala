package ordset.treap.traverse

import ordset.domain.Domain
import ordset.treap.eval.{NodeStackOps, NodeVisitStack}
import ordset.treap.reduce.ContextExtract
import ordset.treap.{Eval, Traverse, TraverseStep, Treap}

object KeySearch {

  trait UpwardStopPredicate[E, D <: Domain[E], W] {

    def apply(parent: Treap.Node[E, D, W], child: Treap[E, D, W]): Boolean
  }

  object UpwardStopPredicate {

    def toNextKey[E, D <: Domain[E], W]: UpwardStopPredicate[E, D, W] =
      toNextKeyInstance.asInstanceOf[UpwardStopPredicate[E, D, W]]

    def toPrevKey[E, D <: Domain[E], W]: UpwardStopPredicate[E, D, W] =
      toPrevKeyInstance.asInstanceOf[UpwardStopPredicate[E, D, W]]

    def never[E, D <: Domain[E], W]: UpwardStopPredicate[E, D, W] =
      neverInstance.asInstanceOf[UpwardStopPredicate[E, D, W]]

    def always[E, D <: Domain[E], W]: UpwardStopPredicate[E, D, W] =
      alwaysInstance.asInstanceOf[UpwardStopPredicate[E, D, W]]

    private lazy val toNextKeyInstance: UpwardStopPredicate[Any, Domain[Any], Any] =
      (parent, tree) => parent.hasLeftInstance(tree)

    private lazy val toPrevKeyInstance: UpwardStopPredicate[Any, Domain[Any], Any] =
      (parent, tree) => parent.hasRightInstance(tree)

    private lazy val neverInstance: UpwardStopPredicate[Any, Domain[Any], Any] =
      (_, _) => false

    private lazy val alwaysInstance: UpwardStopPredicate[Any, Domain[Any], Any] =
      (_, _) => true
  }

  def down[E, D <: Domain[E], W, C](
    key: E,
    evalFunc: Eval.DefaultFunc[E, D, W, C]
  )(
    implicit domain: Domain[E]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) =>
      tree match {
        case tree: Treap.Node[E, D, W] =>
          val cmp = domain.elementOrd.compare(key, tree.key)
          if (cmp > 0) tree match {
            case n: Treap.NodeWithRight[E, D, W] =>
              Traverse.Output(n.right, evalFunc(tree, context, TraverseStep.Right), TraverseStep.Right, stop = false)
            case _ =>
              Traverse.Output(tree, evalFunc(tree, context, TraverseStep.None), TraverseStep.None, stop = true)
          }
          else if (cmp < 0) tree match {
            case n: Treap.NodeWithLeft[E, D, W] =>
              Traverse.Output(n.left, evalFunc(tree, context, TraverseStep.Left), TraverseStep.Left, stop = false)
            case _ =>
              Traverse.Output(tree, evalFunc(tree, context, TraverseStep.None), TraverseStep.None, stop = true)
          }
          else
            Traverse.Output(tree, evalFunc(tree, context, TraverseStep.None), TraverseStep.None, stop = true)
        case _ =>
          Traverse.Output(tree, evalFunc(tree, context, TraverseStep.None), TraverseStep.None, stop = true)
      }

  def up[E, D <: Domain[E], W, C](
    stopPredicate: UpwardStopPredicate[E, D, W],
    evalFunc: Eval.DefaultFunc[E, D, W, C]
  )(
    implicit contextOps: NodeStackOps[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) => {
      val parent = contextOps.getHeadNodeOrNull(context)
      val newContext = evalFunc(tree, context, TraverseStep.Up)
      if (parent == null) {
        Traverse.Output(Treap.Empty(), newContext, TraverseStep.Up, stop = true)
      } else {
        Traverse.Output(parent, newContext, TraverseStep.Up, stop = stopPredicate(parent, tree))
      }
    }

  def nextKey[E, D <: Domain[E], W, C <: NodeVisitStack.Context[E, D, W]](
    evalFunc: Eval.DefaultFunc[E, D, W, C]
  )(
    implicit contextOps: NodeVisitStack.ContextOps[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) =>
      tree match {
        case tree: Treap.NodeWithRight[E, D, W] =>
          val traverseLeftFunc = DepthFirst.nonEmpty(DepthFirst.leftOnlyNavigate, evalFunc)(contextOps)
          val rightContext = evalFunc(tree, context, TraverseStep.Right)
          val contextExtract = ContextExtract.reduceAfter(tree.right, rightContext)(traverseLeftFunc)
          Traverse.Output(contextExtract.tree, contextExtract.context, TraverseStep.Right, stop = true)
        case tree: Treap.Node[E, D, W] =>
          val traverseUpFunc = up(UpwardStopPredicate.toNextKey, evalFunc)
          val contextExtract = ContextExtract.reduceAfter(tree, context)(traverseUpFunc)
          Traverse.Output(contextExtract.tree, contextExtract.context, TraverseStep.Up, stop = true)
        case _ =>
          Traverse.Output(tree, evalFunc(tree, context, TraverseStep.None), TraverseStep.None, stop = true)
        }

  def prevKey[E, D <: Domain[E], W, C <: NodeVisitStack.Context[E, D, W]](
    evalFunc: Eval.DefaultFunc[E, D, W, C]
  )(
    implicit contextOps: NodeVisitStack.ContextOps[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) =>
      tree match {
        case tree: Treap.NodeWithLeft[E, D, W] =>
          val traverseRightFunc = DepthFirst.nonEmpty(DepthFirst.rightOnlyNavigate, evalFunc)(contextOps)
          val leftContext = evalFunc(tree, context, TraverseStep.Left)
          val contextExtract = ContextExtract.reduceAfter(tree.left, leftContext)(traverseRightFunc)
          Traverse.Output(contextExtract.tree, contextExtract.context, TraverseStep.Left, stop = true)
        case tree: Treap.Node[E, D, W] =>
          val traverseUpFunc = up(UpwardStopPredicate.toPrevKey , evalFunc)
          val contextExtract = ContextExtract.reduceAfter(tree, context)(traverseUpFunc)
          Traverse.Output(contextExtract.tree, contextExtract.context, TraverseStep.Up, stop = true)
        case _ =>
          Traverse.Output(tree, evalFunc(tree, context, TraverseStep.None), TraverseStep.None, stop = true)
      }

  def minKey[E, D <: Domain[E], W, C](
    evalFunc: Eval.DefaultFunc[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) =>
      tree match {
        case tree: Treap.NodeWithLeft[E, D, W] =>
          Traverse.Output(tree.left, evalFunc(tree, context, TraverseStep.Left), TraverseStep.Left, stop = false)
        case _ =>
          Traverse.Output(tree, evalFunc(tree, context, TraverseStep.None), TraverseStep.None, stop = true)
      }

  def maxKey[E, D <: Domain[E], W, C](
    evalFunc: Eval.DefaultFunc[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) =>
      tree match {
        case tree: Treap.NodeWithRight[E, D, W] =>
          Traverse.Output(tree.right, evalFunc(tree, context, TraverseStep.Left), TraverseStep.Left, stop = false)
        case _ =>
          Traverse.Output(tree, evalFunc(tree, context, TraverseStep.None), TraverseStep.None, stop = true)
      }
}
