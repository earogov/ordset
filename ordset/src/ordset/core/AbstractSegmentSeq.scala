package ordset.core

import ordset.core.AbstractIndexedSegmentSeq.IndexedSegment
import ordset.core.domain.Domain
import ordset.core.map.LazyTreapOrderedMap

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
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isValueIncluded(value: V): Boolean
  
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
   *   originalBoundSegment.containsBound(bound.provideLower) == true    (1)
   * }}}
   * It allows to avoid repeated search of segment if it's already known before method call.
   * 
   * Note, if provided segment differs from one defined by condition 1, the behaviour of function is undefined.
   */
  protected def prependBelowExtendedInternal[Seg](
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
   *   originalBoundSegment.containsBound(bound.provideUpper) == true    (1)
   * }}}
   * It allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note, if provided segment differs from one defined by condition 1, the behaviour of function is undefined.
   */
  protected def appendAboveExtendedInternal[Seg](
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

  /**
   * Implementation of [[SegmentSeqT.patchLazy]].
   *
   * Current sequence is used as a base sequence of [[LazyTreapOrderedMap]].
   * Note, this may cause immediate conversion of current sequence into treap based one.
   */
  def patchLazyBaseSeqInternal(lazySeq: SegmentSeq[E, D, OptionalSeqSupplier.Type[E, D, V]]): SegmentSeq[E, D, V] =
    LazyTreapOrderedMap.apply(this, lazySeq)(domainOps, valueOps, rngManager)

  /**
   * Implementation of [[SegmentSeqT.patchLazy]].
   *
   * Segments of `lazySeq` that has [[None]] value are replaced with function `() => this`. Then new version of
   * `lazySeq` is used to create [[LazyTreapOrderedMap]]. The result sequence will have completely lazy initial
   * state. So conversion of current sequence into treap based will take place on demand (but with some overhead
   * compared to [[patchLazyBaseSeqInternal]]).
   */
  def patchLazyFlatmapInternal(lazySeq: SegmentSeq[E, D, OptionalSeqSupplier.Type[E, D, V]]): SegmentSeq[E, D, V] = {
    val baseSeq = consUniform(valueOps.unit)
    val currentSeqFunc = () => this
    val newLazySeq = lazySeq.map {
      case x @ Some(_) => x
      case _ => Some(currentSeqFunc)
    }(
      lazySeq.valueOps
    )
    LazyTreapOrderedMap.apply(baseSeq, newLazySeq)(domainOps, valueOps, rngManager)
  }
}
