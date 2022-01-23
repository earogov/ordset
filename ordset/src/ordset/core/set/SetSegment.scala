package ordset.core.set

import ordset.core.SegmentT
import ordset.core.domain.Domain

object SetSegment {

  type WithNext[E, D[X] <: Domain[X]] = SetSegmentT.WithNext[E, D, Any]

  type WithPrev[E, D[X] <: Domain[X]] = SetSegmentT.WithPrev[E, D, Any]

  type First[E, D[X] <: Domain[X]] = SetSegmentT.First[E, D, Any]

  type Last[E, D[X] <: Domain[X]] = SetSegmentT.Last[E, D, Any]

  type Initial[E, D[X] <: Domain[X]] = SetSegmentT.Initial[E, D, Any]

  type Terminal[E, D[X] <: Domain[X]] = SetSegmentT.Terminal[E, D, Any]

  type Inner[E, D[X] <: Domain[X]] = SetSegmentT.Inner[E, D, Any]

  type Single[E, D[X] <: Domain[X]] = SetSegmentT.Single[E, D, Any]
}
