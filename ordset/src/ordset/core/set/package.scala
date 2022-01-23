package ordset.core

import ordset.core.domain.Domain
import ordset.core.map.LazyOrderedMap

package object set {

  // General set -------------------------------------------------------------- //
  type SetSegmentLike[E, D[X] <: Domain[X], V] = SegmentLikeT[E, D, Boolean, Any]

  type SetSegment[E, D[X] <: Domain[X]] = Segment[E, D, Boolean]

  type SetSegmentWithPrev[E, D[X] <: Domain[X], V] = SegmentT.WithPrev[E, D, Boolean, Any]

  type SetSegmentWithNext[E, D[X] <: Domain[X], V] = SegmentT.WithNext[E, D, Boolean, Any]

  type SetSegmentT[E, D[X] <: Domain[X], +S] = SegmentT[E, D, Boolean, S]

  type SetSegmentTWithPrev[E, D[X] <: Domain[X], +S] = SegmentT.WithPrev[E, D, Boolean, S]

  type SetSegmentTWithNext[E, D[X] <: Domain[X], +S] = SegmentT.WithNext[E, D, Boolean, S]

  type SetSegmentTruncationT[E, D[X] <: Domain[X], +S] = SegmentTruncationT[E, D, Boolean, S, SetSegmentT[E, D, S]]

  type SetSegmentTruncation[E, D[X] <: Domain[X]] = SegmentTruncationT[E, D, Boolean, Any, SetSegment[E, D]]

  type OrderedSet[E, D[X] <: Domain[X]] = SegmentSeq[E, D, Boolean]

  type OrderedSetT[E, D[X] <: Domain[X], +S] = SegmentSeqT[E, D, Boolean, S]

  // Uniform set -------------------------------------------------------------- //
  type UniformSetSegment[E, D[X] <: Domain[X]] = UniformSegment[E, D, Boolean]

  type UniformSetTruncation[E, D[X] <: Domain[X]] = UniformTruncation[E, D, Boolean]

  // Indexed set -------------------------------------------------------------- //
  type IndexedSetSegmentBase[E, D[X] <: Domain[X]] = IndexedSegmentBase[E, D, Boolean]

  type IndexedSetSegment[E, D[X] <: Domain[X]] = IndexedSegment[E, D, Boolean]

  type IndexedSetSegmentWithPrev[E, D[X] <: Domain[X]] = IndexedSegmentWithPrev[E, D, Boolean]

  type IndexedSetSegmentWithNext[E, D[X] <: Domain[X]] = IndexedSegmentWithNext[E, D, Boolean]

  type IndexedSetTruncation[E, D[X] <: Domain[X]] = IndexedTruncation[E, D, Boolean]

  type NonuniformIndexedOrderedSet[E, D[X] <: Domain[X]] = NonuniformIndexedSegmentSeq[E, D, Boolean]

  type IndexedOrderedSet[E, D[X] <: Domain[X]] = IndexedSegmentSeq[E, D, Boolean]

  // Array set ---------------------------------------------------------------- //
  type ArrayOrderedSet[E, D[X] <: Domain[X]] = NonuniformArrayOrderedSet[E, D] | UniformOrderedSet[E, D]

  // Treap set ---------------------------------------------------------------- //
  type TreapSetSegmentBase[E, D[X] <: Domain[X]] = TreapSegmentBase[E, D, Boolean]

  type TreapSetSegment[E, D[X] <: Domain[X]] = TreapSegment[E, D, Boolean]

  type TreapSetSegmentWithPrev[E, D[X] <: Domain[X]] = TreapSegmentWithPrev[E, D, Boolean]

  type TreapSetSegmentWithNext[E, D[X] <: Domain[X]] = TreapSegmentWithNext[E, D, Boolean]

  type TreapSetTruncation[E, D[X] <: Domain[X]] = TreapTruncation[E, D, Boolean]

  type TreapOrderedSet[E, D[X] <: Domain[X]] = NonuniformTreapOrderedSet[E, D] | UniformOrderedSet[E, D]

  // Lazy set ----------------------------------------------------------------- //
  type LazySetSegmentBase[E, D[X] <: Domain[X]] = LazySegmentBase[E, D, Boolean]

  type LazySetSegment[E, D[X] <: Domain[X]] = LazySegment[E, D, Boolean]

  type LazySetSegmentWithPrev[E, D[X] <: Domain[X]] = LazySegmentWithPrev[E, D, Boolean]

  type LazySetSegmentWithNext[E, D[X] <: Domain[X]] = LazySegmentWithNext[E, D, Boolean]

  type LazySetTruncation[E, D[X] <: Domain[X]] = LazyTruncation[E, D, Boolean]

  type LazyOrderedSet[E, D[X] <: Domain[X]] = LazyTreapOrderedSet[E, D]

  // Set supplier ------------------------------------------------------------- //
  type SetSupplier[E, D[X] <: Domain[X]] = SeqSupplier[E, D, Boolean]

  type SetSupplierSegment[E, D[X] <: Domain[X]] = Segment[E, D, SetSupplier[E, D]]

  type SetSupplierSegmentWithPrev[E, D[X] <: Domain[X]] = Segment.WithPrev[E, D, SetSupplier[E, D]]

  type SetSupplierSegmentWithNext[E, D[X] <: Domain[X]] = Segment.WithNext[E, D, SetSupplier[E, D]]

  type SetSupplierTruncation[E, D[X] <: Domain[X]] = SegmentTruncation[E, D, SetSupplier[E, D]]

  type SetSupplierOrderedMap[E, D[X] <: Domain[X]] = SegmentSeq[E, D, SetSupplier[E, D]]

  // Mapped set --------------------------------------------------------------- //
  type MappedSetSegmentBase[E, D[X] <: Domain[X], S] = MappedSegmentBase[E, D, Boolean, Boolean, S]

  type MappedSetSegment[E, D[X] <: Domain[X], S] = MappedSegment[E, D, Boolean, Boolean, S]

  type MappedSetSegmentWithPrev[E, D[X] <: Domain[X], S] = MappedSegmentWithPrev[E, D, Boolean, Boolean, S]

  type MappedSetSegmentWithNext[E, D[X] <: Domain[X], S] = MappedSegmentWithNext[E, D, Boolean, Boolean, S]

  type MappedSetTruncation[E, D[X] <: Domain[X], S] = MappedTruncation[E, D, Boolean, Boolean, S]

  // Zipped set --------------------------------------------------------------- //
  type ZippedSetSegmentBase[E, D[X] <: Domain[X], S1, S2] =
    ZippedSegmentBase[E, D, Boolean, Boolean, Boolean, S1, S2]

  type ZippedSetSegment[E, D[X] <: Domain[X], S1, S2] =
    ZippedSegment[E, D, Boolean, Boolean, Boolean, S1, S2]

  type ZippedSetSegmentWithPrev[E, D[X] <: Domain[X], S1, S2] =
    ZippedSegmentWithPrev[E, D, Boolean, Boolean, Boolean, S1, S2]

  type ZippedSetSegmentWithNext[E, D[X] <: Domain[X], S1, S2] =
    ZippedSegmentWithNext[E, D, Boolean, Boolean, Boolean, S1, S2]

  type ZippedSetTruncation[E, D[X] <: Domain[X], S1, S2] =
    ZippedTruncation[E, D, Boolean, Boolean, Boolean, S1, S2]
}
