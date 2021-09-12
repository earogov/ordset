package ordset.core.set

import ordset.core.{AbstractMappedValueSegmentSeq, MappedSegment, MappedTruncation, SegmentT, SegmentTruncationT}
import ordset.core.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.util.BooleanUtil

class MappedValueOrderedSet[E, D <: Domain[E], S] protected (
  override final val originalSeq: OrderedSetT[E, D, S],
  override final val valueMapFunc: Boolean => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractMappedValueSegmentSeq[E, D, Boolean, Boolean, S]
  with OrderedSetCommons[E, D, MappedSetSegmentBase[E, D, S]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] = UniformOrderedSet.default(value)

  @inline
  protected final override def cons(original: OrderedSet[E, D]): OrderedSet[E, D] =
    new MappedValueOrderedSet(original, valueMapFunc)

  protected final override def mapSegment(segment: SetSegmentT[E, D, S]): MappedSetSegment[E, D, S] =
    super.mapSegment(segment)

  protected final override def mapTruncation(truncation: SetSegmentTruncationT[E, D, S]): MappedSetTruncation[E, D, S] =
    super.mapTruncation(truncation)
}

object MappedValueOrderedSet {

  def apply[E, D <: Domain[E], S](
    original: OrderedSetT[E, D, S],
    valueMapFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedValueOrderedSet[E, D, S] =
    new MappedValueOrderedSet(original, valueMapFunc)

  def mapSegment[E, D <: Domain[E], S](
    originalSegment: SetSegmentT[E, D, S],
    valueMapFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedSetSegment[E, D, S] =
    apply(originalSegment.sequence, valueMapFunc).mapSegment(originalSegment)

  def mapTruncation[E, D <: Domain[E], S](
    originalTruncation: SetSegmentTruncationT[E, D, S],
    valueMapFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedSetTruncation[E, D, S] =
    apply(originalTruncation.sequence, valueMapFunc).mapTruncation(originalTruncation)

  def identity[E, D <: Domain[E], S](
    original: OrderedSetT[E, D, S]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedValueOrderedSet[E, D, S] =
    new MappedValueOrderedSet(original, BooleanUtil.identityOperator1)

  def inversion[E, D <: Domain[E], S](
    original: OrderedSetT[E, D, S]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedValueOrderedSet[E, D, S] =
    new MappedValueOrderedSet(original, BooleanUtil.inversionOperator1)
}


