package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.core.AbstractMappedSegmentSeq
import ordset.core.AbstractMappedSegmentSeq.MappedSegmentBase
import ordset.util.BooleanUtil

class MappedOrderedSet[E, D <: Domain[E], S] protected (
  override final val originalSeq: OrderedSetT[E, D, S],
  final val mapFunc: Boolean => Boolean
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
    new MappedOrderedSet(original, mapFunc)
}

object MappedOrderedSet {

  def apply[E, D <: Domain[E], S](
    original: OrderedSetT[E, D, S],
    mapFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedOrderedSet[E, D, S] =
    new MappedOrderedSet(original, mapFunc)

  def identity[E, D <: Domain[E], S](
    original: OrderedSetT[E, D, S]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedOrderedSet[E, D, S] =
    new MappedOrderedSet(original, BooleanUtil.identityOperator1)

  def inversion[E, D <: Domain[E], S](
    original: OrderedSetT[E, D, S]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): MappedOrderedSet[E, D, S] =
    new MappedOrderedSet(original, BooleanUtil.inversionOperator1)
}
