package ordset.treap

import ordset.domain.Domain

object Eval {

  trait Func[E, D <: Domain[E], C, -S] extends ((Treap[E, D], C, S) => C)

  type DefaultFunc[E, D <: Domain[E], C] = Func[E, D, C, TraverseStep.Type]

  type GenericFunc[E, D <: Domain[E], C] = Func[E, D, C, Any]

  implicit def toEvalOps[E, D <: Domain[E], C, S](
    evalFunc: Func[E, D, C, S]
  ): EvalOps[E, D, C, S] = new EvalOps(evalFunc)

  final class EvalOps[E, D <: Domain[E], C, S](val evalFunc: Func[E, D, C, S]) extends AnyVal {

    def thenEval(nextEvalFunc: Eval.Func[E, D, C, S]): Eval.Func[E, D, C, S] =
      (tree, context, step) => {
        val nextContext = evalFunc(tree, context, step)
        nextEvalFunc(tree, nextContext, step)
      }
  }
}
