package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.core.{AbstractMappedSegmentSeq, MappedTruncation, Segment, SegmentT, SegmentTruncationT}
import ordset.core.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.map.MappedOrderedMap
import ordset.core.value.ValueOps
import ordset.util.BooleanUtil

class MappedOrderedSet[E, D <: Domain[E], S] protected (
  override final val originalSeq: OrderedSetT[E, D, S],
  override final val segmentMapFunc: SetSegment[E, D] => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractMappedSegmentSeq[E, D, Boolean, Boolean, S]
  with OrderedSetCommons[E, D, MappedSegmentBase[E, D, Boolean, Boolean, S]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] = UniformOrderedSet.default(value)

  @inline
  protected final override def cons(original: OrderedSet[E, D]): OrderedSet[E, D] =
    new MappedOrderedSet(original, segmentMapFunc)

  protected final override def mapOriginalSeqTruncation(
    originalTruncation: SegmentTruncationT[E, D, Boolean, S, SetSegmentT[E, D, S]]
  ): MappedTruncation[E, D, Boolean, Boolean, S] =
    super.mapOriginalSeqTruncation(originalTruncation)
}

object MappedOrderedSet {

  def apply[E, D <: Domain[E], S](
    original: OrderedSetT[E, D, S],
    segmentMapFunc: SetSegment[E, D] => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedOrderedSet[E, D, S] =
    new MappedOrderedSet(original, segmentMapFunc)

  def mapOriginalTruncation[E, D <: Domain[E], S](
    originalTruncation: SegmentTruncationT[E, D, Boolean, S, SetSegmentT[E, D, S]],
    segmentMapFunc: SetSegment[E, D] => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedTruncation[E, D, Boolean, Boolean, S] = {
    val mappedSeq = apply(
      originalTruncation.sequence,
      segmentMapFunc
    )
    mappedSeq.mapOriginalSeqTruncation(originalTruncation)
  }
}
