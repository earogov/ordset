package ordset.treap

import ordset.domain.Domain

object Reduce {

  trait Func[E, D <: Domain[E], -C, R] extends ((Treap[E, D], C, R) => R)

  def apply[E, D <: Domain[E], C, R](
    treap: Treap[E, D],
    initContext: C,
    initValue: R
  )(
    traverseFunc: Traverse.GenericFunc[E, D, C],
    reduceFunc: Func[E, D, C, R]
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