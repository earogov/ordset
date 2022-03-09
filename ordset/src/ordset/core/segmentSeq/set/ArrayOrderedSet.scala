package ordset.core.segmentSeq.set

import ordset.core.Bound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.{AbstractIndexedSegmentSeq, SegmentSeqException}
import ordset.core.segmentSeq.validation.ValidatingIterable
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
   * Returns ordered set factory (see [[OrderedSetFactory]]).
   */
  def getFactory[E, D[X] <: Domain[X]]: OrderedSetFactory[E, D, ArrayOrderedSet[E, D]] =
    factoryInstance.asInstanceOf[OrderedSetFactory[E, D, ArrayOrderedSet[E, D]]]

  /**
   * Returns ordered set builder (see [[OrderedSetBuilder]]).
   */
  def getBuilder[E, D[X] <: Domain[X]]: OrderedSetBuilder[E, D, ArrayOrderedSet[E, D]] =
    builderInstance.asInstanceOf[OrderedSetBuilder[E, D, ArrayOrderedSet[E, D]]]

  // Private section ---------------------------------------------------------- //
  private lazy val factoryInstance: Factory[Any, Domain] = new Factory()

  private lazy val builderInstance: OrderedSetBuilder[Any, Domain, ArrayOrderedSet[Any, Domain]] = 
    OrderedSetBuilder.default(factoryInstance)

  private class Factory[E, D[X] <: Domain[X]] extends OrderedSetFactory[E, D, ArrayOrderedSet[E, D]] {

    @throws[SegmentSeqException]("if preconditions are violated")
    override def unsafeBuild(
      bounds: ValidatingIterable[Bound.Upper[E]],
      complementary: Boolean
    )(
      implicit 
      domainOps: DomainOps[E, D],
      rngManager: RngManager
    ): ArrayOrderedSet[E, D] =
      bounds.originalIterable match {
        case boundsArraySeq: ArraySeq[Bound.Upper[E]] =>
          bounds.validateAll()
          unchecked(boundsArraySeq, complementary)(domainOps, rngManager)
        case _ =>
          val boundsArraySeq = 
            bounds.foldLeftValidated(ArraySeq.empty[Bound.Upper[E]]) { (seq, bnd) => 
              seq.appended(bnd) 
            }
          unchecked(boundsArraySeq, complementary)(domainOps, rngManager)
      }

    override def convertSet(set: OrderedSet[E, D]): ArrayOrderedSet[E, D] =
      set match {
        case set: ArrayOrderedSet[E, D] => set
        case _ => convertSetInternal(set)
      }
  }
}
