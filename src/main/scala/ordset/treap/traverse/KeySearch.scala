package ordset.treap.traverse

import ordset.domain.Domain
import ordset.treap.eval.NodeVisitStack
import ordset.treap.reduce.ContextExtract
import ordset.treap.{Eval, Navigate, Traverse, TraverseStep, Treap}

object KeySearch {

  type Context      [E, D <: Domain[E], W]                         = NodeVisitStack.Context    [E, D, W]
  type ContextOps   [E, D <: Domain[E], W, C <: Context[E, D, W]]  = NodeVisitStack.ContextOps [E, D, W, C]
  type Output       [E, D <: Domain[E], W, C <: Context[E, D, W]]  = Traverse.DefaultOutput    [E, D, W, C]
  type NavigateFunc [E, D <: Domain[E], W]                         = Navigate.DefaultFunc      [E, D, W, Context[E, D, W]]
  type EvalFunc     [E, D <: Domain[E], W, C <: Context[E, D, W]]  = Eval.DefaultFunc          [E, D, W, C]

  def down[E, D <: Domain[E], W, C <: Context[E, D, W]](
    key: E,
    evalFunc: EvalFunc[E, D, W, C]
  )(
    implicit domain: Domain[E]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) => {
      val output = tree match {
        case tree: Treap.Node[E, D, W] =>
          val cmp = domain.elementOrd.compare(key, tree.key)
          if (cmp > 0) tree match {
            case n: Treap.NodeWithRight[E, D, W] =>
              new Output(n.right, context, TraverseStep.Right, stop = false)
            case _ =>
              new Output(tree, context, TraverseStep.None, stop = true)
          }
          else if (cmp < 0) tree match {
            case n: Treap.NodeWithLeft[E, D, W] =>
              new Output(n.left, context, TraverseStep.Left, stop = false)
            case _ =>
              new Output(tree, context, TraverseStep.None, stop = true)
          }
          else
            new Output(tree, context, TraverseStep.None, stop = true)
        case _ =>
          new Output(tree, context, TraverseStep.None, stop = true)
      }
      output.withContext(evalFunc(tree, output.context, output.step))
    }

  def up[E, D <: Domain[E], W, C <: Context[E, D, W]](
    stopPredicate: Treap.Node[E, D, W] => Boolean,
    evalFunc: EvalFunc[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) => {
      val output: Output[E, D, W, C] = context.stack match {
        case head :: _ => head.tree match {
          case headTree: Treap.Node[E, D, W] =>
            new Output(headTree, context, TraverseStep.Up, stop = stopPredicate(headTree))
          case _ =>
            new Output(head.tree, context, TraverseStep.Up, stop = true)
        }
        case _ =>
          new Output(Treap.Empty(), context, TraverseStep.Up, stop = true)
      }
      output.withContext(evalFunc(tree, output.context, output.step))
    }

  def nextKey[E, D <: Domain[E], W, C <: Context[E, D, W]](
    evalFunc: EvalFunc[E, D, W, C]
  )(
    implicit domain: Domain[E],
    contextOps: ContextOps[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) =>
      tree match {
        case tree: Treap.NodeWithRight[E, D, W] =>
          val traverseLeftFunc = DepthFirst.nonEmpty(DepthFirst.leftOnlyNavigate, evalFunc)(contextOps)
          val rightContext = evalFunc(tree, context, TraverseStep.Right)
          val contextExtract = ContextExtract.reduceAfter(tree.right, rightContext)(traverseLeftFunc)
          new Output(contextExtract.tree, contextExtract.context, TraverseStep.Right, stop = true)
        case tree: Treap.Node[E, D, W] =>
          val stopPredicate = (node: Treap.Node[E, D, W]) => domain.elementOrd.compare(node.key, tree.key) > 0
          val traverseUpFunc = up(stopPredicate, evalFunc)
          val contextExtract = ContextExtract.reduceAfter(tree, context)(traverseUpFunc)
          new Output(contextExtract.tree, contextExtract.context, TraverseStep.Up, stop = true)
        case _ =>
          val output = new Output(tree, context, TraverseStep.None, stop = true)
          output.withContext(evalFunc(tree, output.context, output.step))
        }

  def prevKey[E, D <: Domain[E], W, C <: Context[E, D, W]](
    evalFunc: EvalFunc[E, D, W, C]
  )(
    implicit domain: Domain[E],
    contextOps: ContextOps[E, D, W, C]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) =>
      tree match {
        case tree: Treap.NodeWithLeft[E, D, W] =>
          val traverseRightFunc = DepthFirst.nonEmpty(DepthFirst.rightOnlyNavigate, evalFunc)(contextOps)
          val leftContext = evalFunc(tree, context, TraverseStep.Left)
          val contextExtract = ContextExtract.reduceAfter(tree.left, leftContext)(traverseRightFunc)
          new Output(contextExtract.tree, contextExtract.context, TraverseStep.Left, stop = true)
        case tree: Treap.Node[E, D, W] =>
          val stopPredicate = (node: Treap.Node[E, D, W]) => domain.elementOrd.compare(node.key, tree.key) < 0
          val traverseUpFunc = up(stopPredicate, evalFunc)
          val contextExtract = ContextExtract.reduceAfter(tree, context)(traverseUpFunc)
          new Output(contextExtract.tree, contextExtract.context, TraverseStep.Up, stop = true)
        case _ =>
          val output = new Output(tree, context, TraverseStep.None, stop = true)
          output.withContext(evalFunc(tree, output.context, output.step))
      }
}
