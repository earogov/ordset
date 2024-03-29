package ordset.core.segmentSeq

import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.Domain
import ordset.core.segmentSeq.map.LazyTreapOrderedMap
import ordset.core.segmentSeq.set.OrderedSet
import ordset.core.segmentSeq.internal.mappedSeq.NonMergingMappedValueOrderedMap
import ordset.util.BooleanUtil

/**
 * Base class for segment sequences.
 * 
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam V type of value assigned to range of elements
 * @tparam S type of range state
 */
abstract class AbstractSegmentSeq[E, D[X] <: Domain[X], V, +S] extends SegmentSeqT[E, D, V, S] {

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
   * Segment type for internal operations. 
   * 
   * Generally it's just a `SegmentT[E, D, V, S] with S`. But also it may be any type representing segment of current
   * sequence.
   */ 
  protected type SegmentInternal

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
   * Same as [[SegmentSeqT.prependBelowBound]] but with additional argument `originalBoundSegment` such that:
   * {{{
   *   originalBoundSegment.containsBound(bound.provideLower) == true     (1)
   * }}}
   * It allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note, if provided segment differs from one defined by condition 1, the behavior of method is undefined.
   */
  protected def prependBelowBoundInternal(
    bound: Bound[E],
    originalBoundSegment: SegmentInternal,
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V]

  /**
   * Adds support of unlimited bounds to [[prependBelowBoundInternal]]:
   * <div>if `bound` is [[ExtendedBound.BelowAll]] returns current sequence;</div>
   * <div>if `bound` is [[ExtendedBound.AboveAll]] returns `other` sequence;</div>
   * <div>otherwise result is the same as for method [[prependBelowBoundInternal]].</div>
   */
  protected def prependBelowExtendedInternal(
    bound: ExtendedBound[E],
    originalBoundSegment: SegmentInternal,
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    bound match {
      case bound: Bound[E] => prependBelowBoundInternal(bound, originalBoundSegment, other)
      case ExtendedBound.BelowAll => this
      case ExtendedBound.AboveAll => other
    }

  /**
   * Same as [[SegmentSeqT.appendAboveBound]] but with additional argument `originalBoundSegment` such that:
   * {{{
   *   originalBoundSegment.containsBound(bound.provideUpper) == true     (1)
   * }}}
   * It allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note, if provided segment differs from one defined by condition 1, the behavior of method is undefined.
   */
  protected def appendAboveBoundInternal(
    bound: Bound[E],
    originalBoundSegment: SegmentInternal,
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V]

  /**
   * Adds support of unlimited bounds to [[appendAboveExtendedInternal]]:
   * <div>if `bound` is [[ExtendedBound.BelowAll]] returns `other` sequence;</div>
   * <div>if `bound` is [[ExtendedBound.AboveAll]] returns current sequence;</div>
   * <div>otherwise result is the same as for method [[appendAboveExtendedInternal]].</div>
   */
  protected def appendAboveExtendedInternal(
    bound: ExtendedBound[E],
    originalBoundSegment: SegmentInternal,
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    bound match {
      case bound: Bound[E] => appendAboveBoundInternal(bound, originalBoundSegment, other)
      case ExtendedBound.BelowAll => other
      case ExtendedBound.AboveAll => this
    }

  /**
   * Implementation of [[SegmentSeqT.patchLazy]].
   *
   * Current sequence is used as a base sequence of [[ordset.core.segmentSeq.map.LazyTreapOrderedMap]].
   * Note, this may cause immediate conversion of current sequence into treap based one.
   */
  def patchLazyDefaultInternal(supplierSeq: SupplierSegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    LazyTreapOrderedMap.apply(this, supplierSeq)(domainOps, valueOps, rngManager)

  /**
   * Implementation of [[SegmentSeqT.patchLazy]].
   *
   * Segments of `supplierSeq` that has [[None]] value are replaced with function `() => this`. Then new version of
   * `supplierSeq` is used to create [[ordset.core.segmentSeq.map.LazyTreapOrderedMap]]. The result sequence will have 
   * completely lazy initial state. So conversion of current sequence into treap based will take place on demand 
   * (but with some overhead compared to [[patchLazyDefaultInternal]]).
   */
  def patchLazyDelayedInternal(supplierSeq: SupplierSegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
    val baseSeq = consUniform(valueOps.unit)
    val currentSeqFunc = () => this
    val newSupplierSeq = supplierSeq.map {
      case x @ Some(_) => x
      case _ => Some(currentSeqFunc)
    }(
      supplierSeq.valueOps
    )
    LazyTreapOrderedMap.apply(baseSeq, newSupplierSeq)(domainOps, valueOps, rngManager)
  }

  // Protected section -------------------------------------------------------- //
  /**
   * Default implementation for [[inverse]] method.
   */
  protected def defaultInverse(implicit ev: V =:= Boolean): OrderedSet[E, D] = {
    type F[X] = X => Boolean
    // Inversion operator preserves the property that all adjacent segments have different values.
    // So we can use non-merging mapped segment sequence to perform some optimization.
    NonMergingMappedValueOrderedMap(this, ev.substituteContra[F](BooleanUtil.inversionOperator1))
  }
}
