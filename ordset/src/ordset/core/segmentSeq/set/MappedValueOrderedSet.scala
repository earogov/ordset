package ordset.core.segmentSeq.set

import ordset.core.segmentSeq.{AbstractMappedValueSegmentSeq, SegmentT, SegmentTruncationT}
import ordset.core.segmentSeq.{MappedSegment, MappedTruncation}
import ordset.core.segmentSeq.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.util.BooleanUtil

class MappedValueOrderedSet[E, D[X] <: Domain[X], S] protected (
  override final val originalSeq: OrderedSetT[E, D, S],
  override final val valueMapFunc: Boolean => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractMappedValueSegmentSeq[E, D, Boolean, Boolean, S]
  with OrderedSetCommons[E, D, MappedSetSegmentBase[E, D, S]] {

  // Transformation ----------------------------------------------------------- //
  override def strict: StrictOrderedSet[E, D] = defaultStrict

  // Set transformation ------------------------------------------------------- //
  override def inverse(implicit ev: Boolean =:= Boolean): OrderedSet[E, D] = defaultInverse

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] = UniformOrderedSet.default(value)

  @inline
  protected final override def cons(original: OrderedSet[E, D]): MappedValueOrderedSet[E, D, Any] =
    new MappedValueOrderedSet(original, valueMapFunc)

  protected final override def mapSegment(segment: SetSegmentT[E, D, S]): MappedSetSegment[E, D, S] =
    super.mapSegment(segment)

  protected final override def mapTruncation(truncation: SetSegmentTruncationT[E, D, S]): MappedSetTruncation[E, D, S] =
    super.mapTruncation(truncation)
}

object MappedValueOrderedSet {

  def apply[E, D[X] <: Domain[X], S](
    original: OrderedSetT[E, D, S],
    valueMapFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedValueOrderedSet[E, D, S] =
    new MappedValueOrderedSet(original, valueMapFunc)

  def mapSegment[E, D[X] <: Domain[X], S](
    originalSegment: SetSegmentT[E, D, S],
    valueMapFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedSetSegment[E, D, S] =
    apply(originalSegment.sequence, valueMapFunc).mapSegment(originalSegment)

  def mapTruncation[E, D[X] <: Domain[X], S](
    originalTruncation: SetSegmentTruncationT[E, D, S],
    valueMapFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedSetTruncation[E, D, S] =
    apply(originalTruncation.sequence, valueMapFunc).mapTruncation(originalTruncation)

  def identity[E, D[X] <: Domain[X], S](
    original: OrderedSetT[E, D, S]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedValueOrderedSet[E, D, S] =
    new MappedValueOrderedSet(original, BooleanUtil.identityOperator1)

  def inversion[E, D[X] <: Domain[X], S](
    original: OrderedSetT[E, D, S]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedValueOrderedSet[E, D, S] =
    new MappedValueOrderedSet(original, BooleanUtil.inversionOperator1)
}


