package ordset.treap

import ordset.Order

object Eval {

  trait Func[K, Ord <: Order[K], C, -S] extends ((Treap[K, Ord], C, S) => C)

  implicit def toEvalOps[K, Ord <: Order[K], C, S](
    evalFunc: Func[K, Ord, C, S]
  ): EvalOps[K, Ord, C, S] = new EvalOps(evalFunc)

  final class EvalOps[K, Ord <: Order[K], C, S](val evalFunc: Func[K, Ord, C, S]) extends AnyVal {

    def thenEval(nextEvalFunc: Eval.Func[K, Ord, C, S]): Eval.Func[K, Ord, C, S] =
      (tree, context, step) => {
        val nextContext = evalFunc(tree, context, step)
        nextEvalFunc(tree, nextContext, step)
      }
  }
}
