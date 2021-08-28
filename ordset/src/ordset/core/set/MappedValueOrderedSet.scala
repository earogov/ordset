package ordset.core.set

import ordset.core.AbstractMappedValueSegmentSeq
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
  with OrderedSetCommons[E, D, MappedSegmentBase[E, D, Boolean, Boolean, S]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] = UniformOrderedSet.default(value)

  @inline
  protected final override def cons(original: OrderedSet[E, D]): OrderedSet[E, D] =
    new MappedValueOrderedSet(original, valueMapFunc)
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


