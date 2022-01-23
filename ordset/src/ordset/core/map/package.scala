package ordset.core

import ordset.core.domain.Domain

package object map {
  
  // General map -------------------------------------------------------------- //
  type OrderedMap[E, D[X] <: Domain[X], V] = SegmentSeq[E, D, V]

  type OrderedMapT[E, D[X] <: Domain[X], V, +S] = SegmentSeqT[E, D, V, S]
  
  // Treap map ---------------------------------------------------------------- //
  type TreapOrderedMap[E, D[X] <: Domain[X], V] = NonuniformTreapOrderedMap[E, D, V] | UniformOrderedMap[E, D, V]

  // Lazy map ----------------------------------------------------------------- //
  type LazyOrderedMap[E, D[X] <: Domain[X], V] = LazyTreapOrderedMap[E, D, V]

  // Map supplier ------------------------------------------------------------- //
  type SupplierOrderedMap[E, D[X] <: Domain[X], V] = SupplierSegmentSeq[E, D, V]
}
