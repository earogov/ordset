package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.core.{AbstractMappedSegmentSeq, Segment, SegmentT, SegmentTruncationT}
import ordset.core.{MappedSegment, MappedTruncation}
import ordset.core.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.map.MappedOrderedMap
import ordset.core.value.ValueOps
import ordset.util.BooleanUtil

class MappedOrderedSet[E, D[X] <: Domain[X], S] protected (
  override final val originalSeq: OrderedSetT[E, D, S],
  override final val segmentMapFunc: SetSegment[E, D] => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractMappedSegmentSeq[E, D, Boolean, Boolean, S]
  with OrderedSetCommons[E, D, MappedSetSegmentBase[E, D, S]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] = UniformOrderedSet.default(value)

  @inline
  protected final override def cons(original: OrderedSet[E, D]): MappedOrderedSet[E, D, Any] =
    new MappedOrderedSet(original, segmentMapFunc)

  protected final override def mapSegment(segment: SetSegmentT[E, D, S]): MappedSetSegment[E, D, S] =
    super.mapSegment(segment)
  
  protected final override def mapTruncation(truncation: SetSegmentTruncationT[E, D, S]): MappedSetTruncation[E, D, S] =
    super.mapTruncation(truncation)
}

object MappedOrderedSet {

  def apply[E, D[X] <: Domain[X], S](
    original: OrderedSetT[E, D, S],
    segmentMapFunc: SetSegment[E, D] => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedOrderedSet[E, D, S] =
    new MappedOrderedSet(original, segmentMapFunc)

  def mapSegment[E, D[X] <: Domain[X], S](
    originalSegment: SetSegmentT[E, D, S],
    segmentMapFunc: SetSegment[E, D] => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedSetSegment[E, D, S] =
    apply(originalSegment.sequence, segmentMapFunc).mapSegment(originalSegment)
  
  def mapTruncation[E, D[X] <: Domain[X], S](
    originalTruncation: SegmentTruncationT[E, D, Boolean, S, SetSegmentT[E, D, S]],
    segmentMapFunc: SetSegment[E, D] => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedSetTruncation[E, D, S] =
    apply(originalTruncation.sequence, segmentMapFunc).mapTruncation(originalTruncation)
}
