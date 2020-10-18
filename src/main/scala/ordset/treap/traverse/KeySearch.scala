package ordset.treap.traverse

import ordset.domain.Domain
import ordset.treap.eval.NodeVisitStack
import ordset.treap.{Eval, Navigate, Traverse, TraverseStep, Treap}

object KeySearch {

  type Context      [E, D <: Domain[E], W]                         = NodeVisitStack.Context    [E, D, W]
  type ContextOps   [E, D <: Domain[E], W, C <: Context[E, D, W]]  = NodeVisitStack.ContextOps [E, D, W, C]
  type Output       [E, D <: Domain[E], W, C <: Context[E, D, W]]  = Traverse.DefaultOutput    [E, D, W, C]
  type NavigateFunc [E, D <: Domain[E], W]                         = Navigate.DefaultFunc      [E, D, W, Context[E, D, W]]
  type EvalFunc     [E, D <: Domain[E], W, C <: Context[E, D, W]]  = Eval.DefaultFunc          [E, D, W, C]

  def constContext[E, D <: Domain[E], W, C <: Context[E, D, W]](
    key: E,
  )(
    implicit domain: Domain[E]
  ): Traverse.DefaultFunc[E, D, W, C] =
    (tree, context) => tree match {
      case tree: Treap.Node[E, D, W] =>
        val cmp = domain.elementOrd.compare(key, tree.key)
        if (cmp > 0) tree match {
          case n: Treap.NodeWithRight[E, D, W] => new Output(n.right, context, TraverseStep.Right, stop = false)
          case _ => new Output(tree, context, TraverseStep.None, stop = true)
        }
        else if (cmp < 0) tree match {
          case n: Treap.NodeWithLeft[E, D, W] => new Output(n.left, context, TraverseStep.Left, stop = false)
          case _ => new Output(tree, context, TraverseStep.None, stop = true)
        }
        else new Output(tree, context, TraverseStep.None, stop = true)
      case _ => new Output(tree, context, TraverseStep.None, stop = true)
    }

  def evalContext[E, D <: Domain[E], W, C <: Context[E, D, W]](
    key: E,
    evalFunc: EvalFunc[E, D, W, C]
  )(
    implicit domain: Domain[E]
  ): Traverse.DefaultFunc[E, D, W, C] =
    constContext[E, D, W, C](key).thenEval(evalFunc)
}
