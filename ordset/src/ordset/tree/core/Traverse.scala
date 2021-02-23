package ordset.tree.core

object Traverse {

  trait Func[K, V, Tree[KK, VV], C, +S] extends ((Tree[K, V], C) => Output[K, V, Tree, C, S])

  type BinaryFunc[K, V, Tree[KK, VV], C] = Func[K, V, Tree, C, BinaryTreeStep.Type]

  type GenericFunc[K, V, Tree[KK, VV], C] = Func[K, V, Tree, C, Any]

  type BinaryOutput[K, V, Tree[KK, VV], C] = Output[K, V, Tree, C, BinaryTreeStep.Type]

  case class Output[K, V, Tree[KK, VV], C, +S](tree: Tree[K, V], context: C, step: S, stop: Boolean) {

    def withTree(t: Tree[K, V]): Output[K, V, Tree, C, S] = Output(t, context, step, stop)

    def withContext(c: C): Output[K, V, Tree, C, S] = Output(tree, c, step, stop)

    def withStop(s: Boolean): Output[K, V, Tree, C, S] = Output(tree, context, step, s)
  }

  implicit def toTraverseOps[K, V, Tree[KK, VV], C, S](
    traverseFunc: Func[K, V, Tree, C, S]
  ): TraverseOps[K, V, Tree, C, S] = new TraverseOps(traverseFunc)

  final class TraverseOps[K, V, Tree[KK, VV], C, S](
    val traverseFunc: Func[K, V, Tree, C, S]
  ) extends AnyVal {

    def thenEval(evalFunc: Eval.Func[K, V, Tree, C, S]): Func[K, V, Tree, C, S] =
      (tree, context) => {
        val traverseOutput = traverseFunc(tree, context)
        traverseOutput.withContext(evalFunc(tree, traverseOutput.context, traverseOutput.step))
      }
  }
}
