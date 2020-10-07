package ordset.treap

import ordset.Order

object Traverse {

  trait Func[K, Ord <: Order[K], C, +S] extends ((Treap[K, Ord], C) => Output[K, Ord, C, S])

  type DefaultFunc[K, Ord <: Order[K], C] = Func[K, Ord, C, TraverseStep.Type]

  type GenericFunc[K, Ord <: Order[K], C] = Func[K, Ord, C, Any]

  case class Output[K, Ord <: Order[K], C, +S](tree: Treap[K, Ord], context: C, step: S, stop: Boolean) {

    def withContext(c: C): Output[K, Ord, C, S] =
      Output(tree, c, step, stop)

    def eval(evalFunc: Eval.Func[K, Ord, C, S]): Output[K, Ord, C, S] =
      Output(tree, evalFunc(tree, context, step), step, stop)
  }

  implicit def toTraverseOps[K, Ord <: Order[K], C, S](
    traverseFunc: Func[K, Ord, C, S]
  ): TraverseOps[K, Ord, C, S] = new TraverseOps(traverseFunc)

  final class TraverseOps[K, Ord <: Order[K], C, S](val traverseFunc: Func[K, Ord, C, S]) extends AnyVal {

    def thenEval(evalFunc: Eval.Func[K, Ord, C, S]): Func[K, Ord, C, S] =
      (tree, context) => {
        val traverseOutput = traverseFunc(tree, context)
        traverseOutput.withContext(evalFunc(tree, traverseOutput.context, traverseOutput.step))
      }
  }
}
