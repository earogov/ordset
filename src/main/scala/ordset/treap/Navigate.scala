package ordset.treap

import ordset.domain.Domain

object Navigate {

  trait Func[E, D <: Domain[E], W, -C, +S] extends ((Treap[E, D, W], C) => S)

  type DefaultFunc[E, D <: Domain[E], W, C] = Func[E, D, W, C, TraverseStep.Type]

  type GenericFunc[E, D <: Domain[E], W, C] = Func[E, D, W, C, Any]

}
