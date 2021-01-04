package ordset.tree.core

object Fold {

  trait Func[K, V, -Tree[KK, VV], -C, R] extends ((Tree[K, V], C, R) => R)

  def before[K, V, Tree[KK, VV], C, R](
    initTree: Tree[K, V],
    initContext: C,
    initValue: R
  )(
    traverseFunc: Traverse.GenericFunc[K, V, Tree, C],
    foldFunc: Func[K, V, Tree, C, R]
  ): R = {
    var result = initValue
    var context = initContext
    var tree = initTree
    var stop = false
    while (!stop) {
      result = foldFunc(tree, context, result)
      val traverse = traverseFunc(tree, context)
      context = traverse.context
      tree = traverse.tree
      stop = traverse.stop
    }
    result
  }

  def after[K, V, Tree[KK, VV], C, R](
    initTree: Tree[K, V],
    initContext: C,
    initValue: R
  )(
    traverseFunc: Traverse.GenericFunc[K, V, Tree, C],
    foldFunc: Func[K, V, Tree, C, R]
  ): R = {
    var result = initValue
    var context = initContext
    var tree = initTree
    var stop = false
    while (!stop) {
      val traverse = traverseFunc(tree, context)
      context = traverse.context
      tree = traverse.tree
      result = foldFunc(tree, context, result)
      stop = traverse.stop
    }
    result
  }
}