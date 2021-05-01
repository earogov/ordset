package ordset.core

import domain.Domain

object Segment {

  type WithNext[E, D <: Domain[E], V] = SegmentT.WithNext[E, D, V, Any]

  type WithPrev[E, D <: Domain[E], V] = SegmentT.WithPrev[E, D, V, Any]

  type First[E, D <: Domain[E], V] = SegmentT.First[E, D, V, Any]

  type Last[E, D <: Domain[E], V] = SegmentT.Last[E, D, V, Any]

  type Initial[E, D <: Domain[E], V] = SegmentT.Initial[E, D, V, Any]

  type Terminal[E, D <: Domain[E], V] = SegmentT.Terminal[E, D, V, Any]

  type Inner[E, D <: Domain[E], V] = SegmentT.Inner[E, D, V, Any]

  type Single[E, D <: Domain[E], V] = SegmentT.Single[E, D, V, Any]
}
