package ordset.core.set

import ordset.core.SegmentT
import ordset.core.domain.Domain

object SetSegmentT {

  type WithNext[E, D <: Domain[E], +S] = SegmentT.WithNext[E, D, Boolean, S]

  type WithPrev[E, D <: Domain[E], +S] = SegmentT.WithPrev[E, D, Boolean, S]

  type First[E, D <: Domain[E], +S] = SegmentT.First[E, D, Boolean, S]

  type Last[E, D <: Domain[E], +S] = SegmentT.Last[E, D, Boolean, S]

  type Initial[E, D <: Domain[E], +S] = SegmentT.Initial[E, D, Boolean, S]

  type Terminal[E, D <: Domain[E], +S] = SegmentT.Terminal[E, D, Boolean, S]

  type Inner[E, D <: Domain[E], +S] = SegmentT.Inner[E, D, Boolean, S]

  type Single[E, D <: Domain[E], +S] = SegmentT.Single[E, D, Boolean, S]
}
