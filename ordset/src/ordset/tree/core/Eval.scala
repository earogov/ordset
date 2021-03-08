package ordset.tree.core

object Eval {

  trait Func[K, V, -Tree[KK, VV], C, -S] extends ((Tree[K, V], C, S) => C)

  type BinaryFunc[K, V, -Tree[KK, VV], C] = Func[K, V, Tree, C, BinaryTreeStep.Type]

  type GenericFunc[K, V, -Tree[KK, VV], C] = Func[K, V, Tree, C, Any]

  def none[K, V, Tree[KK, VV], C, S]: Func[K, V, Tree, C, S] =
    NoneInstance.asInstanceOf[Func[K, V, Tree, C, S]]

  implicit def toEvalOps[K, V, Tree[KK, VV], C, S](
    evalFunc: Func[K, V, Tree, C, S]
  ): EvalOps[K, V, Tree, C, S] = new EvalOps(evalFunc)

  final class EvalOps[K, V, Tree[KK, VV], C, S](
    val evalFunc: Func[K, V, Tree, C, S]
  ) extends AnyVal {

    def thenEval(nextEvalFunc: Eval.Func[K, V, Tree, C, S]): Eval.Func[K, V, Tree, C, S] =
      (tree, context, step) => {
        val nextContext = evalFunc(tree, context, step)
        nextEvalFunc(tree, nextContext, step)
      }
  }

  private lazy val NoneInstance: Func[Any, Any, [KK, VV] =>> Any, Any, Any] = (_, context, _) => context
}
