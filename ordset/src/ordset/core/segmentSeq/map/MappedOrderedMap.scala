package ordset.core.segmentSeq.map

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.segmentSeq.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.segmentSeq.{AbstractMappedSegmentSeq, Segment, SegmentSeq, SegmentT, SegmentTruncationT}
import ordset.core.segmentSeq.{MappedSegment, MappedTruncation}
import ordset.random.RngManager

/**
 * Ordered map which maps each segment of sequence `originalSeq` with function `segmentMapFunc`.
 * <tr></tr>
 * 
 * Adjacent segments with the same values after mapping are merged.
 */ 
class MappedOrderedMap[E, D[X] <: Domain[X], U, V, S] protected (
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
  protected final override def cons(original: SegmentSeq[E, D, U]): MappedOrderedMap[E, D, U, V, Any]  =
    new MappedOrderedMap(original, segmentMapFunc)

  protected final override def mapSegment(segment: SegmentT[E, D, U, S]): MappedSegment[E, D, U, V, S] =
    super.mapSegment(segment)
  
  protected final override def mapTruncation(
    truncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]]
  ): MappedTruncation[E, D, U, V, S] =
    super.mapTruncation(truncation)
}

object MappedOrderedMap {

  def apply[E, D[X] <: Domain[X], U, V, S](
    originalSeq: OrderedMapT[E, D, U, S],
    segmentMapFunc: Segment[E, D, U] => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedOrderedMap[E, D, U, V, S] =
    new MappedOrderedMap(originalSeq, segmentMapFunc)

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

