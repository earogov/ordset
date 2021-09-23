package ordset.core

import ordset.core.domain.Domain

package object map {
  
  // General map -------------------------------------------------------------- //
  type OrderedMap[E, D <: Domain[E], V] = SegmentSeq[E, D, V]

  type OrderedMapT[E, D <: Domain[E], V, +S] = SegmentSeqT[E, D, V, S]
  
  // Treap map ---------------------------------------------------------------- //
  type TreapOrderedMap[E, D <: Domain[E], V] = NonuniformTreapOrderedMap[E, D, V] | UniformOrderedMap[E, D, V]

  // Lazy map ----------------------------------------------------------------- //
  type LazyOrderedMap[E, D <: Domain[E], V] = LazyTreapOrderedMap[E, D, V]

  // Map supplier ------------------------------------------------------------- //
  type SupplierOrderedMap[E, D <: Domain[E], V] = SupplierSegmentSeq[E, D, V]
}
