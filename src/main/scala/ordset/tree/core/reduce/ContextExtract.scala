package ordset.tree.core.reduce

import ordset.tree.core.{Reduce, Traverse}

object ContextExtract {

  case class Output[K, V, Tree[KK, VV], C](tree: Tree[K, V], context: C)

  def function[K, V, Tree[KK, VV], C]: Reduce.Func[K, V, Tree, C, Output[K, V, Tree, C]] =
    ExtractFunc.asInstanceOf[Reduce.Func[K, V, Tree, C, Output[K, V, Tree, C]]]

  def reduceBefore[K, V, Tree[KK, VV], C](
    treap: Tree[K, V],
    initContext: C
  )(
    traverseFunc: Traverse.GenericFunc[K, V, Tree, C]
  ): Output[K, V, Tree, C] =
    Reduce.before[K, V, Tree, C, Output[K, V, Tree, C]](
      treap,
      initContext,
      null // function ignores `output` argument => `null` is safe
    )(
      traverseFunc,
      function[K, V, Tree, C]
    )

  def reduceAfter[K, V, Tree[KK, VV], C](
    treap: Tree[K, V],
    initContext: C,
  )(
    traverseFunc: Traverse.GenericFunc[K, V, Tree, C]
  ): Output[K, V, Tree, C] =
    Reduce.after[K, V, Tree, C, Output[K, V, Tree, C]](
      treap,
      initContext,
      null // function ignores `output` argument => `null` is safe
    )(
      traverseFunc,
      function[K, V, Tree, C]
    )

  // PRIVATE SECTION
  private lazy val ExtractFunc: Reduce.Func[Any, Any, Any, Any, Output[Any, Any, Any, Any]] =
    makeExtractFunc[Any, Any, Any, Any]

  private def makeExtractFunc[K, V, Tree[KK, VV], C]: Reduce.Func[K, V, Tree, C, Output[K, V, Tree, C]] =
    (tree, context, _) => Output(tree, context)
}
