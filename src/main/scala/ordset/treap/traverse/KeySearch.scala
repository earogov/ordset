package ordset.treap.traverse

import ordset.domain.Domain
import ordset.treap.eval.NodeVisitStack
import ordset.treap.{Eval, Navigate, Traverse, TraverseStep, Treap}

object KeySearch {

  type Context      [E, D <: Domain[E]]                      = NodeVisitStack.Context    [E, D]
  type ContextOps   [E, D <: Domain[E], C <: Context[E, D]]  = NodeVisitStack.ContextOps [E, D, C]
  type Output       [E, D <: Domain[E], C <: Context[E, D]]  = Traverse.DefaultOutput    [E, D, C]
  type NavigateFunc [E, D <: Domain[E]]                      = Navigate.DefaultFunc      [E, D, Context[E, D]]
  type EvalFunc     [E, D <: Domain[E], C <: Context[E, D]]  = Eval.DefaultFunc          [E, D, C]

  def constContext[E, D <: Domain[E], C <: Context[E, D]](
    key: E,
  )(
    implicit domain: Domain[E]
  ): Traverse.DefaultFunc[E, D, C] =
    (tree, context) => tree match {
      case tree: Treap.Node[E, D] =>
        val cmp = domain.elementOrd.compare(key, tree.key)
        if (cmp > 0) tree match {
          case n: Treap.NodeWithRight[E, D] => new Output(n.right, context, TraverseStep.Right, stop = false)
          case _ => new Output(tree, context, TraverseStep.None, stop = true)
        }
        else if (cmp < 0) tree match {
          case n: Treap.NodeWithLeft[E, D] => new Output(n.left, context, TraverseStep.Left, stop = false)
          case _ => new Output(tree, context, TraverseStep.None, stop = true)
        }
        else new Output(tree, context, TraverseStep.None, stop = true)
      case _ => new Output(tree, context, TraverseStep.None, stop = true)
    }

  def evalContext[E, D <: Domain[E], C <: Context[E, D]](
    key: E,
    evalFunc: EvalFunc[E, D, C]
  )(
    implicit domain: Domain[E]
  ): Traverse.DefaultFunc[E, D, C] =
    constContext[E, D, C](key).thenEval(evalFunc)
}
