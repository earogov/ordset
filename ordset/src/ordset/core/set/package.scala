package ordset.core

import ordset.core.domain.Domain
import ordset.core.map.LazyOrderedMap

package object set {

  // General set -------------------------------------------------------------- //
  type SetSegmentLike[E, D <: Domain[E], V] = SegmentLikeT[E, D, Boolean, Any]

  type SetSegment[E, D <: Domain[E]] = Segment[E, D, Boolean]

  type SetSegmentWithPrev[E, D <: Domain[E], V] = SegmentT.WithPrev[E, D, Boolean, Any]

  type SetSegmentWithNext[E, D <: Domain[E], V] = SegmentT.WithNext[E, D, Boolean, Any]

  type SetSegmentT[E, D <: Domain[E], +S] = SegmentT[E, D, Boolean, S]

  type SetSegmentTWithPrev[E, D <: Domain[E], +S] = SegmentT.WithPrev[E, D, Boolean, S]

  type SetSegmentTWithNext[E, D <: Domain[E], +S] = SegmentT.WithNext[E, D, Boolean, S]

  type SetSegmentTruncationT[E, D <: Domain[E], +S] = SegmentTruncationT[E, D, Boolean, S, SetSegmentT[E, D, S]]

  type SetSegmentTruncation[E, D <: Domain[E]] = SegmentTruncationT[E, D, Boolean, Any, SetSegment[E, D]]

  type OrderedSet[E, D <: Domain[E]] = SegmentSeq[E, D, Boolean]

  type OrderedSetT[E, D <: Domain[E], +S] = SegmentSeqT[E, D, Boolean, S]

  // Uniform set -------------------------------------------------------------- //
  type UniformSetSegment[E, D <: Domain[E]] = UniformSegment[E, D, Boolean]

  type UniformSetTruncation[E, D <: Domain[E]] = UniformTruncation[E, D, Boolean]

  // Indexed set -------------------------------------------------------------- //
  type IndexedSetSegmentBase[E, D <: Domain[E]] = IndexedSegmentBase[E, D, Boolean]

  type IndexedSetSegment[E, D <: Domain[E]] = IndexedSegment[E, D, Boolean]

  type IndexedSetSegmentWithPrev[E, D <: Domain[E]] = IndexedSegmentWithPrev[E, D, Boolean]

  type IndexedSetSegmentWithNext[E, D <: Domain[E]] = IndexedSegmentWithNext[E, D, Boolean]

  type IndexedSetTruncation[E, D <: Domain[E]] = IndexedTruncation[E, D, Boolean]

  type NonuniformIndexedOrderedSet[E, D <: Domain[E]] = NonuniformIndexedSegmentSeq[E, D, Boolean]

  type IndexedOrderedSet[E, D <: Domain[E]] = IndexedSegmentSeq[E, D, Boolean]

  // Array set ---------------------------------------------------------------- //
  type ArrayOrderedSet[E, D <: Domain[E]] = NonuniformArrayOrderedSet[E, D] | UniformOrderedSet[E, D]

  // Treap set ---------------------------------------------------------------- //
  type TreapSetSegmentBase[E, D <: Domain[E]] = TreapSegmentBase[E, D, Boolean]

  type TreapSetSegment[E, D <: Domain[E]] = TreapSegment[E, D, Boolean]

  type TreapSetSegmentWithPrev[E, D <: Domain[E]] = TreapSegmentWithPrev[E, D, Boolean]

  type TreapSetSegmentWithNext[E, D <: Domain[E]] = TreapSegmentWithNext[E, D, Boolean]

  type TreapSetTruncation[E, D <: Domain[E]] = TreapTruncation[E, D, Boolean]

  type TreapOrderedSet[E, D <: Domain[E]] = NonuniformTreapOrderedSet[E, D] | UniformOrderedSet[E, D]

  // Lazy set ----------------------------------------------------------------- //
  type LazySetSegmentBase[E, D <: Domain[E]] = LazySegmentBase[E, D, Boolean]

  type LazySetSegment[E, D <: Domain[E]] = LazySegment[E, D, Boolean]

  type LazySetSegmentWithPrev[E, D <: Domain[E]] = LazySegmentWithPrev[E, D, Boolean]

  type LazySetSegmentWithNext[E, D <: Domain[E]] = LazySegmentWithNext[E, D, Boolean]

  type LazySetTruncation[E, D <: Domain[E]] = LazyTruncation[E, D, Boolean]

  type LazyOrderedSet[E, D <: Domain[E]] = LazyTreapOrderedSet[E, D]

  // Mapped set --------------------------------------------------------------- //
  type MappedSetSegmentBase[E, D <: Domain[E], S] = MappedSegmentBase[E, D, Boolean, Boolean, S]

  type MappedSetSegment[E, D <: Domain[E], S] = MappedSegment[E, D, Boolean, Boolean, S]

  type MappedSetSegmentWithPrev[E, D <: Domain[E], S] = MappedSegmentWithPrev[E, D, Boolean, Boolean, S]

  type MappedSetSegmentWithNext[E, D <: Domain[E], S] = MappedSegmentWithNext[E, D, Boolean, Boolean, S]

  type MappedSetTruncation[E, D <: Domain[E], S] = MappedTruncation[E, D, Boolean, Boolean, S]

  // Zipped set --------------------------------------------------------------- //
  type ZippedSetSegmentBase[E, D <: Domain[E], S1, S2] =
    ZippedSegmentBase[E, D, Boolean, Boolean, Boolean, S1, S2]

  type ZippedSetSegment[E, D <: Domain[E], S1, S2] =
    ZippedSegment[E, D, Boolean, Boolean, Boolean, S1, S2]

  type ZippedSetSegmentWithPrev[E, D <: Domain[E], S1, S2] =
    ZippedSegmentWithPrev[E, D, Boolean, Boolean, Boolean, S1, S2]

  type ZippedSetSegmentWithNext[E, D <: Domain[E], S1, S2] =
    ZippedSegmentWithNext[E, D, Boolean, Boolean, Boolean, S1, S2]

  type ZippedSetTruncation[E, D <: Domain[E], S1, S2] =
    ZippedTruncation[E, D, Boolean, Boolean, Boolean, S1, S2]
}
