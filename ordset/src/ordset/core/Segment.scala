package ordset.core

import domain.Domain

object Segment {

  type WithNext[E, D[X] <: Domain[X], V] = SegmentT.WithNext[E, D, V, Any]

  type WithPrev[E, D[X] <: Domain[X], V] = SegmentT.WithPrev[E, D, V, Any]

  type First[E, D[X] <: Domain[X], V] = SegmentT.First[E, D, V, Any]

  type Last[E, D[X] <: Domain[X], V] = SegmentT.Last[E, D, V, Any]

  type Initial[E, D[X] <: Domain[X], V] = SegmentT.Initial[E, D, V, Any]

  type Terminal[E, D[X] <: Domain[X], V] = SegmentT.Terminal[E, D, V, Any]

  type Inner[E, D[X] <: Domain[X], V] = SegmentT.Inner[E, D, V, Any]

  type Single[E, D[X] <: Domain[X], V] = SegmentT.Single[E, D, V, Any]
}
