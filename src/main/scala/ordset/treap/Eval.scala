package ordset.treap

import ordset.domain.Domain

object Eval {

  trait Func[E, D <: Domain[E], W, C, -S] extends ((Treap[E, D, W], C, S) => C)

  type DefaultFunc[E, D <: Domain[E], W, C] = Func[E, D, W, C, TraverseStep.Type]

  type GenericFunc[E, D <: Domain[E], W, C] = Func[E, D, W, C, Any]

  def none[E, D <: Domain[E], W, C, S]: Func[E, D, W, C, S] = NoneInstance.asInstanceOf[Func[E, D, W, C, S]]

  implicit def toEvalOps[E, D <: Domain[E], W, C, S](
    evalFunc: Func[E, D, W, C, S]
  ): EvalOps[E, D, W, C, S] = new EvalOps(evalFunc)

  final class EvalOps[E, D <: Domain[E], W, C, S](val evalFunc: Func[E, D, W, C, S]) extends AnyVal {

    def thenEval(nextEvalFunc: Eval.Func[E, D, W, C, S]): Eval.Func[E, D, W, C, S] =
      (tree, context, step) => {
        val nextContext = evalFunc(tree, context, step)
        nextEvalFunc(tree, nextContext, step)
      }
  }

  private lazy val NoneInstance: Func[Any, Domain[Any], Any, Any, Any] = (_, context, _) => context
}
