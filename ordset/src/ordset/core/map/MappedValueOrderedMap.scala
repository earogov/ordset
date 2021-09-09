package ordset.core.map

import ordset.core.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.{AbstractMappedValueSegmentSeq, MappedTruncation, SegmentSeq, SegmentT, SegmentTruncationT}
import ordset.random.RngManager

class MappedValueOrderedMap[E, D <: Domain[E], U, V, S] protected (
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
  protected final override def cons(original: SegmentSeq[E, D, U]): SegmentSeq[E, D, V] =
    new MappedValueOrderedMap(original, valueMapFunc)

  protected final override def mapOriginalSeqTruncation(
    originalTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]]
  ): MappedTruncation[E, D, U, V, S] =
    super.mapOriginalSeqTruncation(originalTruncation)
}

object MappedValueOrderedMap {

  def apply[E, D <: Domain[E], U, V, S](
    originalSeq: OrderedMapT[E, D, U, S],
    valueMapFunc: U => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedValueOrderedMap[E, D, U, V, S] =
    new MappedValueOrderedMap(originalSeq, valueMapFunc)

  def mapOriginalTruncation[E, D <: Domain[E], U, V, S](
    originalTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]],
    valueMapFunc: U => V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): MappedTruncation[E, D, U, V, S] = {
    val mappedSeq = apply(
      originalTruncation.sequence,
      valueMapFunc
    )
    mappedSeq.mapOriginalSeqTruncation(originalTruncation)
  }
}




