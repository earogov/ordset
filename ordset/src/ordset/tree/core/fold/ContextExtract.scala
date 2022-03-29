package ordset.tree.core.fold

import ordset.tree.core.{Fold, Traverse}

object ContextExtract {

  case class Output[K, V, Tree[KK, VV], C](tree: Tree[K, V], context: C)

  def function[K, V, Tree[KK, VV], C]: Fold.Func[K, V, Tree, C, Output[K, V, Tree, C]] =
    extractFuncInstance.asInstanceOf[Fold.Func[K, V, Tree, C, Output[K, V, Tree, C]]]

  def foldBefore[K, V, Tree[KK, VV], C](
    tree: Tree[K, V],
    initContext: C
  )(
    traverseFunc: Traverse.GenericFunc[K, V, Tree, C]
  ): Output[K, V, Tree, C] = {
    import scala.language.unsafeNulls
    Fold.before[K, V, Tree, C, Output[K, V, Tree, C]](
      tree,
      initContext,
      null // function ignores `output` argument => `null` is safe
    )(
      traverseFunc,
      function[K, V, Tree, C]
    )
  }

  def foldAfter[K, V, Tree[KK, VV], C](
    tree: Tree[K, V],
    initContext: C,
  )(
    traverseFunc: Traverse.GenericFunc[K, V, Tree, C]
  ): Output[K, V, Tree, C] = {
    import scala.language.unsafeNulls
    Fold.after[K, V, Tree, C, Output[K, V, Tree, C]](
      tree,
      initContext,
      null // function ignores `output` argument => `null` is safe
    )(
      traverseFunc,
      function[K, V, Tree, C]
    )
  }

  // Private section ---------------------------------------------------------- //
  private type AnyOutput = Output[Any, Any, [KK, VV] =>> Any, Any]

  private lazy val extractFuncInstance: Fold.Func[Any, Any, [KK, VV] =>> Any, Any, AnyOutput] =
    makeExtractFunc[Any, Any, [KK, VV] =>> Any, Any]

  private def makeExtractFunc[K, V, Tree[KK, VV], C]: Fold.Func[K, V, Tree, C, Output[K, V, Tree, C]] =
    (tree, context, _) => Output(tree, context)
}
