package ordset.treap

import ordset.domain.Domain

object Reduce {

  trait Func[E, D <: Domain[E], W, -C, R] extends ((Treap[E, D, W], C, R) => R)

  def before[E, D <: Domain[E], W, C, R](
    treap: Treap[E, D, W],
    initContext: C,
    initValue: R
  )(
    traverseFunc: Traverse.GenericFunc[E, D, W, C],
    reduceFunc: Func[E, D, W, C, R]
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

  def after[E, D <: Domain[E], W, C, R](
    treap: Treap[E, D, W],
    initContext: C,
    initValue: R
  )(
    traverseFunc: Traverse.GenericFunc[E, D, W, C],
    reduceFunc: Func[E, D, W, C, R]
  ): R = {
    var result = initValue
    var context = initContext
    var tree = treap
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