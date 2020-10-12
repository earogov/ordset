package ordset.treap

import ordset.domain.Domain

object Traverse {

  trait Func[E, D <: Domain[E], C, +S] extends ((Treap[E, D], C) => Output[E, D, C, S])

  type DefaultFunc[E, D <: Domain[E], C] = Func[E, D, C, TraverseStep.Type]

  type GenericFunc[E, D <: Domain[E], C] = Func[E, D, C, Any]

  type DefaultOutput[E, D <: Domain[E], C] = Output[E, D, C, TraverseStep.Type]

  case class Output[E, D <: Domain[E], C, +S](tree: Treap[E, D], context: C, step: S, stop: Boolean) {

    def withContext(c: C): Output[E, D, C, S] =
      Output(tree, c, step, stop)

    def eval(evalFunc: Eval.Func[E, D, C, S]): Output[E, D, C, S] =
      Output(tree, evalFunc(tree, context, step), step, stop)
  }

  implicit def toTraverseOps[E, D <: Domain[E], C, S](
    traverseFunc: Func[E, D, C, S]
  ): TraverseOps[E, D, C, S] = new TraverseOps(traverseFunc)

  final class TraverseOps[E, D <: Domain[E], C, S](val traverseFunc: Func[E, D, C, S]) extends AnyVal {

    def thenEval(evalFunc: Eval.Func[E, D, C, S]): Func[E, D, C, S] =
      (tree, context) => {
        val traverseOutput = traverseFunc(tree, context)
        traverseOutput.withContext(evalFunc(tree, traverseOutput.context, traverseOutput.step))
      }
  }
}
