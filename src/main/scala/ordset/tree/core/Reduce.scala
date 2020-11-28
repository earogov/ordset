package ordset.tree.core

object Reduce {

  trait Func[K, V, Tree[KK, VV], -C, R] extends ((Tree[K, V], C, R) => R)

  def before[K, V, Tree[KK, VV], C, R](
    initTree: Tree[K, V],
    initContext: C,
    initValue: R
  )(
    traverseFunc: Traverse.GenericFunc[K, V, Tree, C],
    reduceFunc: Func[K, V, Tree, C, R]
  ): R = {
    var result = initValue
    var context = initContext
    var tree = initTree
    var stop = false
    while (!stop) {
      result = reduceFunc(tree, context, result)
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
    reduceFunc: Func[K, V, Tree, C, R]
  ): R = {
    var result = initValue
    var context = initContext
    var tree = initTree
    var stop = false
    while (!stop) {
      val traverse = traverseFunc(tree, context)
      context = traverse.context
      tree = traverse.tree
      result = reduceFunc(tree, context, result)
      stop = traverse.stop
    }
    result
  }
}