package ordset.core

import ordset.core.domain.Domain

package object set {

  type SetSegment[E, D <: Domain[E]] = Segment[E, D, Boolean]

  type SetSegmentT[E, D <: Domain[E], +S] = SegmentT[E, D, Boolean, S]
  
  
  type OrderedSet[E, D <: Domain[E]] = SegmentSeq[E, D, Boolean]

  type OrderedSetT[E, D <: Domain[E], +S] = SegmentSeqT[E, D, Boolean, S]
}
