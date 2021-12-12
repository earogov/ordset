package ordset

import ordset.core.domain.Domain

package object core {

  type SegmentOrder[E, D <: Domain[E]] = Order[Segment[E, D, Any]]

  // General sequence --------------------------------------------------------- //
  type SegmentLike[E, D <: Domain[E], V] = SegmentLikeT[E, D, V, Any]

  type Segment[E, D <: Domain[E], V] = SegmentT[E, D, V, Any]

  type SegmentWithPrev[E, D <: Domain[E], V] = SegmentT.WithPrev[E, D, V, Any]

  type SegmentWithNext[E, D <: Domain[E], V] = SegmentT.WithNext[E, D, V, Any]

  type SegmentTruncation[E, D <: Domain[E], V] = SegmentTruncationT[E, D, V, Any, Segment[E, D, V]]

  type SegmentSeq[E, D <: Domain[E], V] = SegmentSeqT[E, D, V, Any]

  // Uniform sequence --------------------------------------------------------- //
  type UniformSegment[E, D <: Domain[E], V] = AbstractUniformSegmentSeq.UniformSingleSegment[E, D, V]

  type UniformTruncation[E, D <: Domain[E], V] = AbstractUniformSegmentSeq.UniformTruncation[E, D, V]

  type UniformSegmentSeq[E, D <: Domain[E], V] = AbstractUniformSegmentSeq[E, D, V]

  // Indexed sequence --------------------------------------------------------- //
  type IndexedSegmentBase[E, D <: Domain[E], V] = AbstractIndexedSegmentSeq.IndexedSegmentBase[E, D, V]
  
  type IndexedSegment[E, D <: Domain[E], V] = AbstractIndexedSegmentSeq.IndexedSegment[E, D, V]

  type IndexedSegmentWithPrev[E, D <: Domain[E], V] = AbstractIndexedSegmentSeq.IndexedSegmentWithPrev[E, D, V]

  type IndexedSegmentWithNext[E, D <: Domain[E], V] = AbstractIndexedSegmentSeq.IndexedSegmentWithNext[E, D, V]

  type IndexedTruncation[E, D <: Domain[E], V] = AbstractIndexedSegmentSeq.IndexedTruncation[E, D, V]

  type NonuniformIndexedSegmentSeq[E, D <: Domain[E], V] = AbstractIndexedSegmentSeq[E, D, V]

  type IndexedSegmentSeq[E, D <: Domain[E], V] = NonuniformIndexedSegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  // Array sequence ----------------------------------------------------------- //
  type NonuniformArraySegmentSeq[E, D <: Domain[E], V] = AbstractArraySegmentSeq[E, D, V]

  type ArraySegmentSeq[E, D <: Domain[E], V] = NonuniformArraySegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  // Treap sequence ----------------------------------------------------------- //
  type TreapSegmentBase[E, D <: Domain[E], V] =
    AbstractTreapSegmentSeq.TreapSegmentBase[E, D, V] | UniformSegment[E, D, V]  
  
  type TreapSegment[E, D <: Domain[E], V] =
    AbstractTreapSegmentSeq.TreapSegment[E, D, V] | UniformSegment[E, D, V]

  type TreapSegmentWithPrev[E, D <: Domain[E], V] = 
    AbstractTreapSegmentSeq.TreapSegmentWithPrev[E, D, V]

  type TreapSegmentWithNext[E, D <: Domain[E], V] =
    AbstractTreapSegmentSeq.TreapSegmentWithNext[E, D, V]

  type TreapTruncation[E, D <: Domain[E], V] = 
    AbstractTreapSegmentSeq.TreapTruncation[E, D, V] | UniformTruncation[E, D, V]

  type NonuniformTreapSegmentSeq[E, D <: Domain[E], V] =
    AbstractTreapSegmentSeq[E, D, V]

  type TreapSegmentSeq[E, D <: Domain[E], V] =
    NonuniformTreapSegmentSeq[E, D, V] | UniformSegmentSeq[E, D, V]

  // Lazy sequence ------------------------------------------------------------ //
  type LazySegmentBase[E, D <: Domain[E], V] = AbstractLazyTreapSegmentSeq.LazySegmentBase[E, D, V]
  
  type LazySegment[E, D <: Domain[E], V] = AbstractLazyTreapSegmentSeq.LazySegment[E, D, V]

  type LazySegmentWithPrev[E, D <: Domain[E], V] = AbstractLazyTreapSegmentSeq.LazySegmentWithPrev[E, D, V]

  type LazySegmentWithNext[E, D <: Domain[E], V] = AbstractLazyTreapSegmentSeq.LazySegmentWithNext[E, D, V]

  type LazyTruncation[E, D <: Domain[E], V] = AbstractLazyTreapSegmentSeq.LazyTruncation[E, D, V]

  type LazySegmentSeq[E, D <: Domain[E], V] = AbstractLazyTreapSegmentSeq[E, D, V]

  // Sequence supplier -------------------------------------------------------- //
  type SeqSupplier[E, D <: Domain[E], V] = Option[() => SegmentSeq[E, D, V]]

  type SupplierSegment[E, D <: Domain[E], V] = Segment[E, D, SeqSupplier[E, D, V]]

  type SupplierSegmentWithPrev[E, D <: Domain[E], V] = Segment.WithPrev[E, D, SeqSupplier[E, D, V]]

  type SupplierSegmentWithNext[E, D <: Domain[E], V] = Segment.WithNext[E, D, SeqSupplier[E, D, V]]

  type SupplierTruncation[E, D <: Domain[E], V] = SegmentTruncation[E, D, SeqSupplier[E, D, V]]

  type SupplierSegmentSeq[E, D <: Domain[E], V] = SegmentSeq[E, D, SeqSupplier[E, D, V]]

  type UniformSupplierSegmentSeq[E, D <: Domain[E], V] = UniformSegmentSeq[E, D, SeqSupplier[E, D, V]]

  // Mapped sequence ---------------------------------------------------------- //
  type MappedSegmentBase[E, D <: Domain[E], U, V, S] = AbstractMappedSegmentSeq.MappedSegmentBase[E, D, U, V, S]
  
  type MappedSegment[E, D <: Domain[E], U, V, S] = AbstractMappedSegmentSeq.MappedSegment[E, D, U, V, S]
  
  type MappedSegmentWithPrev[E, D <: Domain[E], U, V, S] = AbstractMappedSegmentSeq.MappedSegmentWithPrev[E, D, U, V, S]
  
  type MappedSegmentWithNext[E, D <: Domain[E], U, V, S] = AbstractMappedSegmentSeq.MappedSegmentWithNext[E, D, U, V, S]

  type MappedTruncation[E, D <: Domain[E], U, V, S] = AbstractMappedSegmentSeq.MappedTruncation[E, D, U, V, S]

  type MappedSegmentSeq[E, D <: Domain[E], U, V, S] = AbstractMappedSegmentSeq[E, D, U, V, S]

  // Zipped sequence ---------------------------------------------------------- //
  type ZippedSegmentBase[E, D <: Domain[E], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentBase[E, D, U1, U2, V, S1, S2]

  type ZippedSegment[E, D <: Domain[E], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegment[E, D, U1, U2, V, S1, S2]

  type ZippedSegmentWithPrev[E, D <: Domain[E], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2]

  type ZippedSegmentWithNext[E, D <: Domain[E], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2]
  
  type ZippedTupleSegment[E, D <: Domain[E], U1, U2, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegment[E, D, U1, U2, (U1, U2), S1, S2]

  type ZippedTupleSegmentWithPrev[E, D <: Domain[E], U1, U2, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentWithPrev[E, D, U1, U2, (U1, U2), S1, S2]

  type ZippedTupleSegmentWithNext[E, D <: Domain[E], U1, U2, S1, S2] =
    AbstractZippedSegmentSeq.ZippedSegmentWithNext[E, D, U1, U2, (U1, U2), S1, S2]
  
  type ZippedTruncation[E, D <: Domain[E], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq.ZippedTruncation[E, D, U1, U2, V, S1, S2]

  type ZippedSegmentSeq[E, D <: Domain[E], U1, U2, V, S1, S2] =
    AbstractZippedSegmentSeq[E, D, U1, U2, V, S1, S2]
}
