package ordset.core.internal.lazySeq

import ordset.core.{Bound, Segment, LazySegmentSeq, SeqSupplier, SupplierSegmentSeq, SegmentTruncation, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.map.LazyTreapOrderedMap
import ordset.random.RngManager
import ordset.core.ExtendedBound

protected[ordset] object LazySegmentSeqBuilder {

  /**
   * Returns lazy sequence that after computation of all lazy values is equivalent to the result of appending
   * sequence `rightSegment.sequence` to sequence `leftSegment.sequence` above specified `bound` 
   * (see [[SegmentSeqT.appendAboveBound]]). 
   *
   * Input `leftSegment` must satisfy precondition:
   * {{{
   *   leftSegment.containsBound(bound.provideUpper) == true    (1)
   * }}}
   * Input `rightSegment` must satisfy precondition:
   * {{{
   *   rightSegment.containsBound(bound.provideLower) == true       (2)
   * }}}
   * Having segments as input parameters allows to avoid their repeated search if they are already known before
   * method call.
   *
   * Note, if preconditions 1 and 2 are violated, the behavior of method is undefined.
   * {{{
   *
   *  left sequence:
   *
   *               bound   leftSegment
   *                 ]     /
   *  X-------](--------------)[---------X
   *      A          B              C
   *
   *  right sequence:     
   *                      rightSegment
   *                     /
   *  X---------)[----------](-----------X
   *       D          E            F
   *
   *  output sequence (lazy):
   * 
   *               bound
   *  X--------------](------------------X
   *     () => seq1        () => seq2
   * 
   *  output sequence (after computation of lazy values):
   *
   *  X-------](-----](-----](-----------X
   *      A       B      E         F
   * }}}
   */
  def appendSegment[E, D <: Domain[E], V](
    bound: Bound[E],
    leftSegment: Segment[E, D, V],
    rightSegment: Segment[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): LazySegmentSeq[E, D, V] = {
    val leftSeq = leftSegment.takeBelow
    val rightSeq = rightSegment.takeAbove
    val supplierSeq = consSingleBoundedSupplierSeq(bound.provideUpper, Some(() => leftSeq), Some(() => rightSeq))
    LazyTreapOrderedMap.completelyLazy(supplierSeq)
  }

  /**
   * Returns lazy sequence that after computation of all lazy values is equivalent to the result of appending
   * sequence `leftSeq` to sequence `rightSeq` above specified `bound` (see [[SegmentSeqT.appendAboveBound]]). 
   */
  def appendSeq[E, D <: Domain[E], V](
    bound: Bound[E],
    leftSeq: SegmentSeq[E, D, V],
    rightSeq: SegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): LazySegmentSeq[E, D, V] = {
    val leftSegment = leftSeq.getSegmentForBound(bound.provideUpper)
    val rightSegment = rightSeq.getSegmentForBound(bound.provideLower)
    appendSegment(bound, leftSegment, rightSegment)
  }

  // Private section ---------------------------------------------------------- //
  /**
   * Creates [[SupplierSegmentSeq]] with `value1` below `bound` (including) and `value2` above it.
   */ 
  private final def consSingleBoundedSupplierSeq[E, D <: Domain[E], V](
    bound: Bound.Upper[E],
    value1: SeqSupplier[E, D, V],
    value2: SeqSupplier[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): SupplierSegmentSeq[E, D, V] =
    SeqSupplier.SegmentSeq
      .provideUncheckedFactory(domainOps, rngManager)
      .unsafeBuildSingleBounded(bound, value1, value2)
}
