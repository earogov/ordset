package ordset.treap

import ordset.Order

object Reduce {

  trait Func[K, Ord <: Order[K], -C, R] extends ((Treap[K, Ord], C, R) => R)

  def apply[K, Ord <: Order[K], C, R](
    treap: Treap[K, Ord],
    initContext: C,
    initValue: R
  )(
    traverseFunc: Traverse.GenericFunc[K, Ord, C],
    reduceFunc: Func[K, Ord, C, R]
  ): R = {
    var result = initValue
    var context = initContext
    var tree = treap
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
}