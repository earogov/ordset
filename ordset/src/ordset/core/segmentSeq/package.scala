package ordset.core

import ordset.Order
import ordset.core.domain.Domain

package object segmentSeq {

  type SegmentOrder[E, D[X] <: Domain[X]] = Order[Segment[E, D, Any]]

  // General sequence --------------------------------------------------------- //
  type SegmentLike[E, D[X] <: Domain[X], V] = SegmentLikeT[E, D, V, Any]

  type Segment[E, D[X] <: Domain[X], V] = SegmentT[E, D, V, Any]

  type SegmentWithPrev[E, D[X] <: Domain[X], V] = SegmentT.WithPrev[E, D, V, Any]

  type SegmentWithNext[E, D[X] <: Domain[X], V] = SegmentT.WithNext[E, D, V, Any]

  type SegmentTruncation[E, D[X] <: Domain[X], V] = SegmentTruncationT[E, D, V, Any, Segment[E, D, V]]

  type SegmentSeq[E, D[X] <: Domain[X], V] = SegmentSeqT[E, D, V, Any]

  // Uniform sequence --------------------------------------------------------- //
  type UniformSegment[E, D[X] <: Domain[X], V] = AbstractUniformSegmentSeq.UniformSingleSegment[E, D, V]

  type UniformTruncation[E, D[X] <: Domain[X], V] = AbstractUniformSegmentSeq.UniformTruncation[E, D, V]

  type UniformSegmentSeq[E, D[X] <: Domain[X], V] = AbstractUniformSegmentSeq[E, D, V]

  // Indexed sequence --------------------------------------------------------- //
  type IndexedSegmentBase[E, D[X] <: Domain[X], V] = AbstractIndexedSegmentSeq.IndexedSegmentBase[E, D, V]
  
  type IndexedSegment[E, D[X] <: Domain[X], V] = AbstractIndexedSegmentSeq.IndexedSegment[E, D, V]

  type IndexedSegmentWithPrev[E, D[X] <: Domain[X], V] = AbstractIndexedSegmentSeq.IndexedSegmentWithPrev[E, D, V]

  type IndexedSegmentWithNext[E, D[X] <: Domain[X], V] = AbstractIndexedSegmentSeq.IndexedSegmentWithNext[E, D, V]

  type IndexedTruncation[E, D[X] <: Domain[X], V] = AbstractIndexedSegmentSeq.IndexedTruncation[E, D, V]

  type NonuniformIndexedSegmentSeq[E, D[X] <: Domain[X], V] = AbstractIndexedSegmentSeq[E, D, V]

  type IndexedSegmentSeq[E, D[X] <: Domain[X], V] = NonuniformIndexedSegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  // Array sequence ----------------------------------------------------------- //
  type NonuniformArraySegmentSeq[E, D[X] <: Domain[X], V] = AbstractArraySegmentSeq[E, D, V]

  type ArraySegmentSeq[E, D[X] <: Domain[X], V] = NonuniformArraySegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  // Treap sequence ----------------------------------------------------------- //
  type TreapSegmentBase[E, D[X] <: Domain[X], V] =
    AbstractTreapSegmentSeq.TreapSegmentBase[E, D, V] | UniformSegment[E, D, V]  
  
  type TreapSegment[E, D[X] <: Domain[X], V] =
    AbstractTreapSegmentSeq.TreapSegment[E, D, V] | UniformSegment[E, D, V]

  type TreapSegmentWithPrev[E, D[X] <: Domain[X], V] = 
    AbstractTreapSegmentSeq.TreapSegmentWithPrev[E, D, V]

  type TreapSegmentWithNext[E, D[X] <: Domain[X], V] =
    AbstractTreapSegmentSeq.TreapSegmentWithNext[E, D, V]

  type TreapTruncation[E, D[X] <: Domain[X], V] = 
    AbstractTreapSegmentSeq.TreapTruncation[E, D, V] | UniformTruncation[E, D, V]

  type NonuniformTreapSegmentSeq[E, D[X] <: Domain[X], V] =
    AbstractTreapSegmentSeq[E, D, V]

  type TreapSegmentSeq[E, D[X] <: Domain[X], V] =
    NonuniformTreapSegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  // Lazy sequence ------------------------------------------------------------ //
  type LazySegmentBase[E, D[X] <: Domain[X], V] = AbstractLazyTreapSegmentSeq.LazySegmentBase[E, D, V]
  
  type LazySegment[E, D[X] <: Domain[X], V] = AbstractLazyTreapSegmentSeq.LazySegment[E, D, V]

  type LazySegmentWithPrev[E, D[X] <: Domain[X], V] = AbstractLazyTreapSegmentSeq.LazySegmentWithPrev[E, D, V]

  type LazySegmentWithNext[E, D[X] <: Domain[X], V] = AbstractLazyTreapSegmentSeq.LazySegmentWithNext[E, D, V]

  type LazyTruncation[E, D[X] <: Domain[X], V] = AbstractLazyTreapSegmentSeq.LazyTruncation[E, D, V]

  type LazySegmentSeq[E, D[X] <: Domain[X], V] = AbstractLazyTreapSegmentSeq[E, D, V]

  // Sequence supplier -------------------------------------------------------- //
  type SeqSupplier[E, D[X] <: Domain[X], V] = Option[() => SegmentSeq[E, D, V]]

  type SupplierSegment[E, D[X] <: Domain[X], V] = Segment[E, D, SeqSupplier[E, D, V]]

  type SupplierSegmentWithPrev[E, D[X] <: Domain[X], V] = Segment.WithPrev[E, D, SeqSupplier[E, D, V]]

  type SupplierSegmentWithNext[E, D[X] <: Domain[X], V] = Segment.WithNext[E, D, SeqSupplier[E, D, V]]

  type SupplierTruncation[E, D[X] <: Domain[X], V] = SegmentTruncation[E, D, SeqSupplier[E, D, V]]

  type SupplierSegmentSeq[E, D[X] <: Domain[X], V] = SegmentSeq[E, D, SeqSupplier[E, D, V]]

  type UniformSupplierSegmentSeq[E, D[X] <: Domain[X], V] = UniformSegmentSeq[E, D, SeqSupplier[E, D, V]]

  // Mapped sequence ---------------------------------------------------------- //
  type MappedSegmentBase[E, D[X] <: Domain[X], U, V, S] = AbstractMappedSegmentSeq.MappedSegmentBase[E, D, U, V, S]
  
  type MappedSegment[E, D[X] <: Domain[X], U, V, S] = AbstractMappedSegmentSeq.MappedSegment[E, D, U, V, S]
  
  type MappedSegmentWithPrev[E, D[X] <: Domain[X], U, V, S] = AbstractMappedSegmentSeq.MappedSegmentWithPrev[E, D, U, V, S]
  
  type MappedSegmentWithNext[E, D[X] <: Domain[X], U, V, S] = AbstractMappedSegmentSeq.MappedSegmentWithNext[E, D, U, V, S]

  type MappedTruncation[E, D[X] <: Domain[X], U, V, S] = AbstractMappedSegmentSeq.MappedTruncation[E, D, U, V, S]

  type MappedSegmentSeq[E, D[X] <: Domain[X], U, V, S] = AbstractMappedSegmentSeq[E, D, U, V, S]

  // Zipped sequence ---------------------------------------------------------- //
  type ZippedSegmentBase[E, D[X] <: Domain[X], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentBase[E, D, U1, U2, V, S1, S2]

  type ZippedSegment[E, D[X] <: Domain[X], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegment[E, D, U1, U2, V, S1, S2]

  type ZippedSegmentWithPrev[E, D[X] <: Domain[X], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2]

  type ZippedSegmentWithNext[E, D[X] <: Domain[X], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2]
  
  type ZippedTupleSegment[E, D[X] <: Domain[X], U1, U2, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegment[E, D, U1, U2, (U1, U2), S1, S2]

  type ZippedTupleSegmentWithPrev[E, D[X] <: Domain[X], U1, U2, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentWithPrev[E, D, U1, U2, (U1, U2), S1, S2]

  type ZippedTupleSegmentWithNext[E, D[X] <: Domain[X], U1, U2, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentWithNext[E, D, U1, U2, (U1, U2), S1, S2]
  
  type ZippedTruncation[E, D[X] <: Domain[X], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedTruncation[E, D, U1, U2, V, S1, S2]

  type ZippedSegmentSeq[E, D[X] <: Domain[X], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq[E, D, U1, U2, V, S1, S2]
}
