package ordset.core.internal.mappedSeq

import ordset.core.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.{AbstractMappedValueSegmentSeq, SegmentSeq, SegmentT, SegmentTruncationT}
import ordset.core.{MappedSegment, MappedTruncation}
import ordset.core.map.{OrderedMapT, OrderedMapCommons, UniformOrderedMap}
import ordset.random.RngManager

/**
 * Ordered map which maps each segment of sequence `originalSeq` with function `valueMapFunc`.
 * <tr></tr>
 * 
 * Unlike [[MappedValueOrderedMap]] sequence doesn't try to merge adjacent segments after mapping.
 * So it can be used only if precondition is provided.
 * <tr></tr>
 * 
 * Precondition:
 * <tr>
 *   1. Adjacent segments after mapping MUST have different values (with respect to `valueOps.valueHash`).
 * </tr>
 */ 
protected[ordset] class NonMergingMappedValueOrderedMap[E, D <: Domain[E], U, V, S] protected (
  override final val originalSeq: OrderedMapT[E, D, U, S],
  override final val valueMapFunc: U => V
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractMappedValueSegmentSeq[E, D, U, V, S]
  with OrderedMapCommons[E, D, V, MappedSegmentBase[E, D, U, V, S]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: V): UniformOrderedMap[E, D, V] = UniformOrderedMap.default(value)

  @inline
  protected final override def cons(original: SegmentSeq[E, D, U]): NonMergingMappedValueOrderedMap[E, D, U, V, Any] =
    new NonMergingMappedValueOrderedMap(original, valueMapFunc)

  protected final override def mapSegment(segment: SegmentT[E, D, U, S]): MappedSegment[E, D, U, V, S] =
    super.mapSegment(segment)
  
  protected final override def mapTruncation(
    truncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]]
  ): MappedTruncation[E, D, U, V, S] =
    super.mapTruncation(truncation)

  @inline
  protected final override def searchFrontMapper[Seg >: SegmentT.WithPrev[E, D, U, S] <: SegmentT[E, D, U, S], R](
    mapper: Seg => R,
    original: Seg
  ): R =
    mapper(original)
}

object NonMergingMappedValueOrderedMap {

  def apply[E, D <: Domain[E], U, V, S](
    originalSeq: OrderedMapT[E, D, U, S],
    valueMapFunc: U => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): NonMergingMappedValueOrderedMap[E, D, U, V, S] =
    new NonMergingMappedValueOrderedMap(originalSeq, valueMapFunc)

  def mapSegment[E, D <: Domain[E], U, V, S](
    originalSegment: SegmentT[E, D, U, S],
    valueMapFunc: U => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedSegment[E, D, U, V, S] =
    apply(originalSegment.sequence, valueMapFunc).mapSegment(originalSegment)
  
  def mapTruncation[E, D <: Domain[E], U, V, S](
    originalTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]],
    valueMapFunc: U => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedTruncation[E, D, U, V, S] =
    apply(originalTruncation.sequence, valueMapFunc).mapTruncation(originalTruncation)
}




