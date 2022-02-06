package ordset.core.segmentSeq.map

import ordset.core.segmentSeq.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.segmentSeq.{AbstractMappedValueSegmentSeq, SegmentSeq, SegmentT, SegmentTruncationT}
import ordset.core.segmentSeq.{MappedSegment, MappedTruncation}
import ordset.random.RngManager

/**
 * Ordered map which maps each segment of sequence `originalSeq` with function `valueMapFunc`.
 * <tr></tr>
 * 
 * Adjacent segments with the same values after mapping are merged.
 */ 
class MappedValueOrderedMap[E, D[X] <: Domain[X], U, V, S] protected (
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
  protected final override def cons(original: SegmentSeq[E, D, U]): MappedValueOrderedMap[E, D, U, V, Any] =
    new MappedValueOrderedMap(original, valueMapFunc)

  protected final override def mapSegment(segment: SegmentT[E, D, U, S]): MappedSegment[E, D, U, V, S] =
    super.mapSegment(segment)
  
  protected final override def mapTruncation(
    truncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]]
  ): MappedTruncation[E, D, U, V, S] =
    super.mapTruncation(truncation)
}

object MappedValueOrderedMap {

  def apply[E, D[X] <: Domain[X], U, V, S](
    originalSeq: OrderedMapT[E, D, U, S],
    valueMapFunc: U => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedValueOrderedMap[E, D, U, V, S] =
    new MappedValueOrderedMap(originalSeq, valueMapFunc)

  def mapSegment[E, D[X] <: Domain[X], U, V, S](
    originalSegment: SegmentT[E, D, U, S],
    valueMapFunc: U => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedSegment[E, D, U, V, S] =
    apply(originalSegment.sequence, valueMapFunc).mapSegment(originalSegment)
  
  def mapTruncation[E, D[X] <: Domain[X], U, V, S](
    originalTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]],
    valueMapFunc: U => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedTruncation[E, D, U, V, S] =
    apply(originalTruncation.sequence, valueMapFunc).mapTruncation(originalTruncation)

  def identity[E, D[X] <: Domain[X], U, S](
      originalSeq: OrderedMapT[E, D, U, S]
  ): MappedValueOrderedMap[E, D, U, U, S] =
    apply(originalSeq, scala.Predef.identity)(originalSeq.domainOps, originalSeq.valueOps, originalSeq.rngManager)
}




