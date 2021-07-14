package ordset.core

import ordset.core.AbstractIndexedSegmentSeq.IndexedSegment
import ordset.core.domain.Domain

abstract class AbstractSegmentSeq[E, D <: Domain[E], V, +S] extends SegmentSeqT[E, D, V, S] {

  // Transformation ----------------------------------------------------------- //
  override def takeAboveExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] =
    bound match {
      case bound: Bound[E] => takeAboveBound(bound)
      case ExtendedBound.BelowAll => this
      case ExtendedBound.AboveAll => consUniform(lastSegment.value)
    }

  override def takeBelowExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] =
    bound match {
      case bound: Bound[E] => takeBelowBound(bound)
      case ExtendedBound.BelowAll => consUniform(firstSegment.value)
      case ExtendedBound.AboveAll => this
    }

  override def sliceAtExtended(bound: ExtendedBound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    bound match {
      case bound: Bound[E] => sliceAtBound(bound)
      case ExtendedBound.BelowAll => (consUniform(firstSegment.value), this)
      case ExtendedBound.AboveAll => (this, consUniform(lastSegment.value))
    }

  override def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    bound match {
      case bound: Bound[E] => prependBelowBound(bound, other)
      case ExtendedBound.BelowAll => this
      case ExtendedBound.AboveAll => other
    }

  override def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    bound match {
      case bound: Bound[E] => appendAboveBound(bound, other)
      case ExtendedBound.BelowAll => other
      case ExtendedBound.AboveAll => this
    }

  // Protected section -------------------------------------------------------- //
  /**
   * Creates uniform segment sequence with specified `value`.
   */
  protected def consUniform(value: V): SegmentSeq[E, D, V]
  
  /**
   * Utility method to implement [[SegmentSeqT.prependBelowExtended]]:
   * <tr>if input `bound` is instance of [[Bound]] then calls `prependFunc`;</tr>
   * <tr>if `bound` is [[ExtendedBound.BelowAll]] returns current sequence;</tr>
   * <tr>if `bound` is [[ExtendedBound.AboveAll]] returns `other` sequence.</tr>
   * <tr></tr>
   * 
   * `prependFunc` is same as [[SegmentSeqT.prependBelowBound]] but with additional argument `originalBoundSegment`
   * such that:
   * {{{
   *   originalBoundSegment = this.getSegmentForBound(bound.provideLower)    (1)
   * }}}
   * It allows to avoid repeated search of segment if it's already known before method call.
   * 
   * Note if provided segment other then one defined by condition 1, the behaviour of function is undefined.
   */
  protected def prependBelowExtendedInternal[Seg <: Segment[E, D, V]](
    bound: ExtendedBound[E],
    originalBoundSegment: Seg,
    other: SegmentSeq[E, D, V],
    prependFunc: (Bound[E], Seg, SegmentSeq[E, D, V]) => SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    bound match {
      case bound: Bound[E] => prependFunc(bound, originalBoundSegment, other)
      case ExtendedBound.BelowAll => this
      case ExtendedBound.AboveAll => other
    }

  /**
   * Utility method to implement [[SegmentSeqT.appendAboveExtended]]:
   * <tr>if input `bound` is instance of [[Bound]] then calls `appendFunc`;</tr>
   * <tr>if `bound` is [[ExtendedBound.BelowAll]] returns `other` sequence;</tr>
   * <tr>if `bound` is [[ExtendedBound.AboveAll]] returns current sequence.</tr>
   * <tr></tr>
   *
   * `appendFunc` is same as [[SegmentSeqT.appendAboveBound]] but with additional argument `originalBoundSegment` 
   * such that:
   * {{{
   *   originalBoundSegment = this.getSegmentForBound(bound.provideUpper)    (1)
   * }}}
   * It allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note if provided segment other then one defined by condition 1, the behaviour of function is undefined.
   */
  protected def appendAboveExtendedInternal[Seg <: Segment[E, D, V]](
    bound: ExtendedBound[E],
    originalBoundSegment: Seg,
    other: SegmentSeq[E, D, V],
    appendFunc: (Bound[E], Seg, SegmentSeq[E, D, V]) => SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    bound match {
      case bound: Bound[E] => appendFunc(bound, originalBoundSegment, other)
      case ExtendedBound.BelowAll => other
      case ExtendedBound.AboveAll => this
    }
}
