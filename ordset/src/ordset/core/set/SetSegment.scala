package ordset.core.set

import ordset.core.SegmentT
import ordset.core.domain.Domain

object SetSegment {

  type WithNext[E, D <: Domain[E]] = SetSegmentT.WithNext[E, D, Any]

  type WithPrev[E, D <: Domain[E]] = SetSegmentT.WithPrev[E, D, Any]

  type First[E, D <: Domain[E]] = SetSegmentT.First[E, D, Any]

  type Last[E, D <: Domain[E]] = SetSegmentT.Last[E, D, Any]

  type Initial[E, D <: Domain[E]] = SetSegmentT.Initial[E, D, Any]

  type Terminal[E, D <: Domain[E]] = SetSegmentT.Terminal[E, D, Any]

  type Inner[E, D <: Domain[E]] = SetSegmentT.Inner[E, D, Any]

  type Single[E, D <: Domain[E]] = SetSegmentT.Single[E, D, Any]
}
