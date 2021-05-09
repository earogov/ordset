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

  
  type Segment[E, D <: Domain[E], V] = SegmentT[E, D, V, Any]


  type SegmentSeq[E, D <: Domain[E], V] = SegmentSeqT[E, D, V, Any]

  type OrderedMap[E, D <: Domain[E], V] = SegmentSeqT[E, D, V, Any]

  type OrderedSet[E, D <: Domain[E]] = SegmentSeq[E, D, Boolean]


  type SetSegment[E, D <: Domain[E]] = Segment[E, D, Boolean]

  type SetSegmentWithPrev[E, D <: Domain[E]] = Segment.WithPrev[E, D, Boolean]

  type SetSegmentWithNext[E, D <: Domain[E]] = Segment.WithNext[E, D, Boolean]

  type SetSegmentInner[E, D <: Domain[E]] = Segment.Inner[E, D, Boolean]

  type SetSegmentSingle[E, D <: Domain[E]] = Segment.Single[E, D, Boolean]

  type SetIntervalRelation[E, D <: Domain[E]] = IntervalRelation[E, D, Boolean]

  
  type UniformSegmentSeq[E, D <: Domain[E], V] = AbstractUniformSegmentSeq[E, D, V]
  
  type IndexedSegmentSeq[E, D <: Domain[E], V] = AbstractIndexedSegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  type TreapSegmentSeq[E, D <: Domain[E], V] = AbstractTreapSegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]
  
  type ZippedSegmentSeq[E, D <: Domain[E], U1, U2, V] = AbstractZippedSegmentSeq[E, D, U1, U2, V]
}
