package ordset.core.segmentSeq.map

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.segmentSeq.{AbstractZippedSegmentSeq, SegmentT, SegmentSeq, SegmentTruncationT, ZippedTruncation}
import ordset.core.segmentSeq.AbstractZippedSegmentSeq.ZippedSegmentBase
import ordset.core.segmentSeq.set.OrderedSet
import ordset.random.RngManager

class ZippedOrderedMap[E, D[X] <: Domain[X], U1, U2, V, S1, S2] protected (
  override final val firstSeq: OrderedMapT[E, D, U1, S1],
  override final val secondSeq: OrderedMapT[E, D, U2, S2],
  final val operatorFunc: (U1, U2) => V,
  final val firstInvariantFunc: U1 => Boolean,
  final val secondInvariantFunc: U2 => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractZippedSegmentSeq[E, D, U1, U2, V, S1, S2]
  with OrderedMapCommons[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]] {

  // Inspection --------------------------------------------------------------- //
  @inline
  final override def operator(first: U1, second: U2): V = operatorFunc(first, second)

  @inline
  final override def firstInvariant(value: U1): Boolean = firstInvariantFunc(value)

  @inline
  final override def secondInvariant(value: U2): Boolean = secondInvariantFunc(value)

  // Transformation ----------------------------------------------------------- //
  override def strict: StrictOrderedMap[E, D, V] = defaultStrict

  // Set transformation ------------------------------------------------------- //
  override def inverse(implicit ev: V =:= Boolean): OrderedSet[E, D] = defaultInverse

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: V): UniformOrderedMap[E, D, V] = UniformOrderedMap.default(value)
  
  @inline
  protected final override def cons(first: SegmentSeq[E, D, U1], second: SegmentSeq[E, D, U2]): SegmentSeq[E, D, V] =
    new ZippedOrderedMap(first, second, operatorFunc, firstInvariantFunc, secondInvariantFunc)

  protected final override def zipFirstSeqTruncation(
    firstTruncation: SegmentTruncationT[E, D, U1, S1, SegmentT[E, D, U1, S1]]
  ): ZippedTruncation[E, D, U1, U2, V, S1, S2] =
    super.zipFirstSeqTruncation(firstTruncation)

  protected final override def zipSecondSeqTruncation(
    secondTruncation: SegmentTruncationT[E, D, U2, S2, SegmentT[E, D, U2, S2]]
  ): ZippedTruncation[E, D, U1, U2, V, S1, S2] =
    super.zipSecondSeqTruncation(secondTruncation)
}

object ZippedOrderedMap {

  def apply[E, D[X] <: Domain[X], U1, U2, V, S1, S2](
    first: OrderedMapT[E, D, U1, S1],
    second: OrderedMapT[E, D, U2, S2],
    operatorFunc: (U1, U2) => V,
    firstInvariantFunc: U1 => Boolean,
    secondInvariantFunc: U2 => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZippedOrderedMap[E, D, U1, U2, V, S1, S2] =
    new ZippedOrderedMap(first, second, operatorFunc, firstInvariantFunc, secondInvariantFunc)

  def zipFirstMapTruncation[E, D[X] <: Domain[X], U1, U2, V, S1, S2](
    firstTruncation: SegmentTruncationT[E, D, U1, S1, SegmentT[E, D, U1, S1]],
    secondMap: OrderedMapT[E, D, U2, S2],
    operatorFunc: (U1, U2) => V,
    firstInvariantFunc: U1 => Boolean,
    secondInvariantFunc: U2 => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZippedTruncation[E, D, U1, U2, V, S1, S2] = {
    val zippedSeq = apply(
      firstTruncation.sequence,
      secondMap,
      operatorFunc,
      firstInvariantFunc,
      secondInvariantFunc
    )
    zippedSeq.zipFirstSeqTruncation(firstTruncation)
  }

  def zipSecondMapTruncation[E, D[X] <: Domain[X], U1, U2, V, S1, S2](
    secondTruncation: SegmentTruncationT[E, D, U2, S2, SegmentT[E, D, U2, S2]],
    firstMap: OrderedMapT[E, D, U1, S1],
    operatorFunc: (U1, U2) => V,
    firstInvariantFunc: U1 => Boolean,
    secondInvariantFunc: U2 => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZippedTruncation[E, D, U1, U2, V, S1, S2] = {
    val zippedSeq = apply(
      firstMap,
      secondTruncation.sequence,
      operatorFunc,
      firstInvariantFunc,
      secondInvariantFunc
    )
    zippedSeq.zipSecondSeqTruncation(secondTruncation)
  }
}
