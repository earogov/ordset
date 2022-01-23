package ordset.core.set

import ordset.core.SegmentT
import ordset.core.domain.Domain

object SetSegmentT {

  type WithNext[E, D[X] <: Domain[X], +S] = SegmentT.WithNext[E, D, Boolean, S]

  type WithPrev[E, D[X] <: Domain[X], +S] = SegmentT.WithPrev[E, D, Boolean, S]

  type First[E, D[X] <: Domain[X], +S] = SegmentT.First[E, D, Boolean, S]

  type Last[E, D[X] <: Domain[X], +S] = SegmentT.Last[E, D, Boolean, S]

  type Initial[E, D[X] <: Domain[X], +S] = SegmentT.Initial[E, D, Boolean, S]

  type Terminal[E, D[X] <: Domain[X], +S] = SegmentT.Terminal[E, D, Boolean, S]

  type Inner[E, D[X] <: Domain[X], +S] = SegmentT.Inner[E, D, Boolean, S]

  type Single[E, D[X] <: Domain[X], +S] = SegmentT.Single[E, D, Boolean, S]
}
