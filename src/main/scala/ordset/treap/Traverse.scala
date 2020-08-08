package ordset.treap

import ordset.Order

object Traverse {

  trait Func[K, Ord <: Order[K], C, +S] extends ((Treap[K, Ord], C) => Output[K, Ord, C, S])

  type DefaultFunc[K, Ord <: Order[K], C] = Func[K, Ord, C, TraverseStep.Type]

  type GenericFunc[K, Ord <: Order[K], C] = Func[K, Ord, C, Any]

  case class Output[K, Ord <: Order[K], C, +S](context: C, tree: Treap[K, Ord], step: S, stop: Boolean) {

    def withContext(c: C): Output[K, Ord, C, S] =
      Output(c, tree, step, stop)

    def eval(evalFunc: Eval.Func[K, Ord, C, S]): Output[K, Ord, C, S] =
      Output(evalFunc(tree, context, step), tree, step, stop)
  }

  implicit def toTraverseOps[K, Ord <: Order[K], C, S](
    traverseFunc: Func[K, Ord, C, S]
  ): TraverseOps[K, Ord, C, S] = new TraverseOps(traverseFunc)

  class TraverseOps[K, Ord <: Order[K], C, S](val traverseFunc: Func[K, Ord, C, S]) extends AnyVal {

    def thenEval(evalFunc: Eval.Func[K, Ord, C, S]): Func[K, Ord, C, S] =
      (tree, context) => {
        val traverseOutput = traverseFunc(tree, context)
        traverseOutput.withContext(evalFunc(tree, traverseOutput.context, traverseOutput.step))
      }
  }
}
