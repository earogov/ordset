package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{AbstractZippedSegmentSeq, SegmentT, SegmentTruncationT, ZippedTruncation}
import ordset.core.AbstractZippedSegmentSeq.{ZippedSegmentBase, ZippedTruncation}
import ordset.random.RngManager
import ordset.util.BooleanUtil

class ZippedOrderedSet[E, D[X] <: Domain[X], S1, S2] protected (
  override final val firstSeq: OrderedSetT[E, D, S1],
  override final val secondSeq: OrderedSetT[E, D, S2],
  final val operatorFunc: (Boolean, Boolean) => Boolean,
  final val invariantFunc: Boolean => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractZippedSegmentSeq[E, D, Boolean, Boolean, Boolean, S1, S2]
  with OrderedSetCommons[E, D, ZippedSegmentBase[E, D, Boolean, Boolean, Boolean, S1, S2]] {

  // Inspection --------------------------------------------------------------- //
  @inline
  final override def operator(first: Boolean, second: Boolean): Boolean = operatorFunc(first, second)

  @inline
  final override def firstInvariant(value: Boolean): Boolean = invariantFunc(value)

  @inline
  final override def secondInvariant(value: Boolean): Boolean = invariantFunc(value)

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] = UniformOrderedSet.default(value)

  @inline
  protected final override def cons(
    first: OrderedSet[E, D], 
    second: OrderedSet[E, D]
  ): ZippedOrderedSet[E, D, Any, Any] =
    new ZippedOrderedSet(first, second, operatorFunc, invariantFunc)

  protected final override def zipFirstSeqTruncation(
    firstTruncation: SegmentTruncationT[E, D, Boolean, S1, SegmentT[E, D, Boolean, S1]]
  ): ZippedTruncation[E, D, Boolean, Boolean, Boolean, S1, S2] =
    super.zipFirstSeqTruncation(firstTruncation)

  protected final override def zipSecondSeqTruncation(
    secondTruncation: SegmentTruncationT[E, D, Boolean, S2, SegmentT[E, D, Boolean, S2]]
  ): ZippedTruncation[E, D, Boolean, Boolean, Boolean, S1, S2] =
    super.zipSecondSeqTruncation(secondTruncation)
}

object ZippedOrderedSet {

  def apply[E, D[X] <: Domain[X], S1, S2](
    first: OrderedSetT[E, D, S1],
    second: OrderedSetT[E, D, S2],
    operatorFunc: (Boolean, Boolean) => Boolean,
    invariantFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D, S1, S2] =
    new ZippedOrderedSet(first, second, operatorFunc, invariantFunc)

  def union[E, D[X] <: Domain[X], S1, S2](
    first: OrderedSetT[E, D, S1],
    second: OrderedSetT[E, D, S2]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D, S1, S2] =
    new ZippedOrderedSet(first, second, BooleanUtil.orOperator2, BooleanUtil.identityOperator1)

  def intersection[E, D[X] <: Domain[X], S1, S2](
    first: OrderedSetT[E, D, S1],
    second: OrderedSetT[E, D, S2]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D, S1, S2] =
    new ZippedOrderedSet(first, second, BooleanUtil.andOperator2, BooleanUtil.inversionOperator1)

  def zipFirstSetTruncation[E, D[X] <: Domain[X], S1, S2](
    firstTruncation:  SegmentTruncationT[E, D, Boolean, S1, SegmentT[E, D, Boolean, S1]],
    secondSet: OrderedSetT[E, D, S2],
    operatorFunc: (Boolean, Boolean) => Boolean,
    invariantFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedTruncation[E, D, Boolean, Boolean, Boolean, S1, S2] = {
    val zippedSeq = apply(
      firstTruncation.sequence,
      secondSet,
      operatorFunc,
      invariantFunc
    )
    zippedSeq.zipFirstSeqTruncation(firstTruncation)
  }

  def zipSecondSetTruncation[E, D[X] <: Domain[X], S1, S2](
    secondTruncation:  SegmentTruncationT[E, D, Boolean, S2, SegmentT[E, D, Boolean, S2]],
    firstSet: OrderedSetT[E, D, S1],
    operatorFunc: (Boolean, Boolean) => Boolean,
    invariantFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedTruncation[E, D, Boolean, Boolean, Boolean, S1, S2] = {
    val zippedSeq = apply(
      firstSet,
      secondTruncation.sequence,
      operatorFunc,
      invariantFunc
    )
    zippedSeq.zipSecondSeqTruncation(secondTruncation)
  }
}