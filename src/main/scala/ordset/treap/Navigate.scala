package ordset.treap

import ordset.domain.Domain

object Navigate {

  trait Func[E, D <: Domain[E], -C, +S] extends ((Treap[E, D], C) => S)

  type DefaultFunc[E, D <: Domain[E], C] = Func[E, D, C, TraverseStep.Type]

  type GenericFunc[E, D <: Domain[E], C] = Func[E, D, C, Any]

}
