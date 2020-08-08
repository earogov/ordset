package ordset.treap

import ordset.Order

object Navigate {

  trait Func[K, Ord <: Order[K], -C, +S] extends ((Treap[K, Ord], C) => S)
}
