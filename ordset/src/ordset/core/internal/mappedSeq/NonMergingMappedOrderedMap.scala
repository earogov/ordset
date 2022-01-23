package ordset.core.internal.mappedSeq

import ordset.core.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.{AbstractMappedSegmentSeq, Segment, SegmentSeq, SegmentT, SegmentTruncationT}
import ordset.core.{MappedSegment, MappedTruncation}
import ordset.core.map.{OrderedMapT, OrderedMapCommons, UniformOrderedMap}
import ordset.random.RngManager

/**
 * Ordered map which maps each segment of sequence `originalSeq` with function `segmentMapFunc`.
 * <tr></tr>
 * 
 * Unlike [[MappedOrderedMap]] sequence doesn't try to merge adjacent segments after mapping.
 * So it can be used only if precondition is provided.
 * <tr></tr>
 * 
 * Precondition:
 * <tr>
 *   1. Adjacent segments after mapping MUST have different values (with respect to `valueOps.valueHash`).
 * </tr>
 */ 
protected[ordset] class NonMergingMappedOrderedMap[E, D[X] <: Domain[X], U, V, S] protected (
  override final val originalSeq: OrderedMapT[E, D, U, S],
  override final val segmentMapFunc: Segment[E, D, U] => V
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractMappedSegmentSeq[E, D, U, V, S]
  with OrderedMapCommons[E, D, V, MappedSegmentBase[E, D, U, V, S]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: V): UniformOrderedMap[E, D, V] = UniformOrderedMap.default(value)

  @inline
  protected final override def cons(original: SegmentSeq[E, D, U]): NonMergingMappedOrderedMap[E, D, U, V, Any] =
    new NonMergingMappedOrderedMap(original, segmentMapFunc)

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

object NonMergingMappedOrderedMap {

  def apply[E, D[X] <: Domain[X], U, V, S](
    originalSeq: OrderedMapT[E, D, U, S],
    segmentMapFunc: Segment[E, D, U] => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): NonMergingMappedOrderedMap[E, D, U, V, S] =
    new NonMergingMappedOrderedMap(originalSeq, segmentMapFunc)

  def mapSegment[E, D[X] <: Domain[X], U, V, S](
    originalSegment: SegmentT[E, D, U, S],
    segmentMapFunc: Segment[E, D, U] => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedSegment[E, D, U, V, S] =
    apply(originalSegment.sequence, segmentMapFunc).mapSegment(originalSegment)
  
  def mapTruncation[E, D[X] <: Domain[X], U, V, S](
    originalTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]],
    segmentMapFunc: Segment[E, D, U] => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedTruncation[E, D, U, V, S] =
    apply(originalTruncation.sequence, segmentMapFunc).mapTruncation(originalTruncation)
}

