package ordset.core.map

import ordset.core.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.{AbstractMappedSegmentSeq, MappedTruncation, Segment, SegmentSeq, SegmentT, SegmentTruncationT}
import ordset.random.RngManager

class MappedOrderedMap[E, D <: Domain[E], U, V, S] protected (
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
  protected final override def cons(original: SegmentSeq[E, D, U]): SegmentSeq[E, D, V] =
    new MappedOrderedMap(original, segmentMapFunc)

  protected final override def mapOriginalSeqTruncation(
    originalTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]]
  ): MappedTruncation[E, D, U, V, S] =
    super.mapOriginalSeqTruncation(originalTruncation)
}

object MappedOrderedMap {

  def apply[E, D <: Domain[E], U, V, S](
    originalSeq: OrderedMapT[E, D, U, S],
    segmentMapFunc: Segment[E, D, U] => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedOrderedMap[E, D, U, V, S] =
    new MappedOrderedMap(originalSeq, segmentMapFunc)
    
  def mapOriginalTruncation[E, D <: Domain[E], U, V, S](
    originalTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]],
    segmentMapFunc: Segment[E, D, U] => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedTruncation[E, D, U, V, S] = {
    val mappedSeq = apply(
      originalTruncation.sequence,
      segmentMapFunc
    )
    mappedSeq.mapOriginalSeqTruncation(originalTruncation)
  }
}

