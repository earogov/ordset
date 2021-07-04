package ordset

import ordset.core.domain.{DirectedOrder, Domain, OrderDir}
import ordset.util.label.Label

package object core {

  object OrderLabels {

    val BoundDefault: Label = Label("BoundDefault")

    val SegmentByUpperBound: Label = Label("SegmentByUpperBound")

    val SegmentByLowerBound: Label = Label("SegmentByLowerBound")
  }

  type SegmentOrder[E, D <: Domain[E]] = Order[Segment[E, D, Any]]

  type SegmentOrderWithDir[E, D <: Domain[E],  +Dir <: OrderDir] = DirectedOrder[Segment[E, D, Any], Dir]

  
  type SegmentLike[E, D <: Domain[E], V] = SegmentLikeT[E, D, V, Any]
  
  type Segment[E, D <: Domain[E], V] = SegmentT[E, D, V, Any]

  type SegmentSeq[E, D <: Domain[E], V] = SegmentSeqT[E, D, V, Any]

  
  type UniformSegmentSeq[E, D <: Domain[E], V] = AbstractUniformSegmentSeq[E, D, V]
  
  type NonuniformIndexedSegmentSeq[E, D <: Domain[E], V] = AbstractIndexedSegmentSeq[E, D, V]
  
  type IndexedSegmentSeq[E, D <: Domain[E], V] = NonuniformIndexedSegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  type NonuniformArraySegmentSeq[E, D <: Domain[E], V] = AbstractArraySegmentSeq[E, D, V]

  type ArraySegmentSeq[E, D <: Domain[E], V] = NonuniformArraySegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  type NonuniformTreapSegmentSeq[E, D <: Domain[E], V] = AbstractTreapSegmentSeq[E, D, V]
  
  type TreapSegmentSeq[E, D <: Domain[E], V] = NonuniformTreapSegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]
  
  type ZippedSegmentSeq[E, D <: Domain[E], U1, U2, V, S1, S2] = AbstractZippedSegmentSeq[E, D, U1, U2, V, S1, S2]

  type LazySegmentSeq[E, D <: Domain[E], V] = AbstractLazyTreapSegmentSeq[E, D, V]
  
  // type MappedSegmentSeq[E, D <: Domain[E], U, V, S] = AbstractMappedSegmentSeq[E, D, U, V, S]
}
