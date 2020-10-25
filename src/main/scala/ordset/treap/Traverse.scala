package ordset.treap

import ordset.domain.Domain

object Traverse {

  trait Func[E, D <: Domain[E], W, C, +S] extends ((Treap[E, D, W], C) => Output[E, D, W, C, S])

  type DefaultFunc[E, D <: Domain[E], W, C] = Func[E, D, W, C, TraverseStep.Type]

  type GenericFunc[E, D <: Domain[E], W, C] = Func[E, D, W, C, Any]

  type DefaultOutput[E, D <: Domain[E], W, C] = Output[E, D, W, C, TraverseStep.Type]

  case class Output[E, D <: Domain[E], W, C, +S](tree: Treap[E, D, W], context: C, step: S, stop: Boolean) {

    def withTree(t: Treap[E, D, W]): Output[E, D, W, C, S] = Output(t, context, step, stop)

    def withContext(c: C): Output[E, D, W, C, S] = Output(tree, c, step, stop)

    def withStop(s: Boolean): Output[E, D, W, C, S] = Output(tree, context, step, s)
  }

  implicit def toTraverseOps[E, D <: Domain[E], W, C, S](
    traverseFunc: Func[E, D, W, C, S]
  ): TraverseOps[E, D, W, C, S] = new TraverseOps(traverseFunc)

  final class TraverseOps[E, D <: Domain[E], W, C, S](val traverseFunc: Func[E, D, W, C, S]) extends AnyVal {

    def thenEval(evalFunc: Eval.Func[E, D, W, C, S]): Func[E, D, W, C, S] =
      (tree, context) => {
        val traverseOutput = traverseFunc(tree, context)
        traverseOutput.withContext(evalFunc(tree, traverseOutput.context, traverseOutput.step))
      }
  }
}
