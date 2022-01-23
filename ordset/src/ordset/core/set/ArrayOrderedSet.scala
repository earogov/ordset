package ordset.core.set

import ordset.core.{AbstractIndexedSegmentSeq, Bound, SegmentSeqException, SeqValidationPredicate}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

import scala.collection.immutable.ArraySeq

object ArrayOrderedSet {

  /**
   * Creates ordered set from array sequence of bounds.
   *
   * Validation of key order is not applied.
   *
   * @param bounds array sequence of upper bounds.
   * @param complementary equals to value of the first segment. If `true` segment is included in set and vise versa.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def unchecked[E, D[X] <: Domain[X]](
    bounds: ArraySeq[Bound.Upper[E]],
    complementary: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ArrayOrderedSet[E, D] =
    if (bounds.isEmpty) UniformOrderedSet.apply(complementary, getFactory)
    else NonuniformArrayOrderedSet.unsafeUnchecked(bounds, complementary)

  /**
   * Returns ordered set factory.
   */
  def getFactory[E, D[X] <: Domain[X]]: OrderedSetFactory[E, D, ArrayOrderedSet[E, D]] =
    factoryInstance.asInstanceOf[OrderedSetFactory[E, D, ArrayOrderedSet[E, D]]]

  // Private section ---------------------------------------------------------- //
  private lazy val factoryInstance: Factory[Any, Domain] = new Factory()

  private class Factory[E, D[X] <: Domain[X]] extends OrderedSetFactory[E, D, ArrayOrderedSet[E, D]] {

    @throws[SegmentSeqException]("if preconditions are violated")
    override def unsafeBuildAsc(
      bounds: IterableOnce[Bound.Upper[E]],
      complementary: Boolean,
      domainOps: DomainOps[E, D]
    )(
      boundsValidationFunc: SeqValidationPredicate[Bound.Upper[E]] = domainOps.validation.boundsSeq
    )(
      implicit rngManager: RngManager
    ): ArrayOrderedSet[E, D] =
      bounds match {
        case bounds: ArraySeq[Bound.Upper[E]] =>
          SeqValidationPredicate.validateIterable(bounds, boundsValidationFunc)
          unchecked(bounds, complementary)(domainOps, rngManager)
        case _ =>
          val boundsArraySeq =
            SeqValidationPredicate.foldIterableAfter[Bound.Upper[E], ArraySeq[Bound.Upper[E]]](
              bounds,
              boundsValidationFunc,
              ArraySeq.empty[Bound.Upper[E]],
              (seq, bnd) => seq.appended(bnd)
            )
          unchecked(boundsArraySeq, complementary)(domainOps, rngManager)
      }

    override def convertSet(set: OrderedSet[E, D]): ArrayOrderedSet[E, D] =
      set match {
        case set: ArrayOrderedSet[E, D] => set
        case _ => convertSetInternal(set)
      }
  }
}
