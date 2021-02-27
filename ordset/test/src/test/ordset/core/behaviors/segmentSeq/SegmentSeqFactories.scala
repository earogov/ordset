package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{ArrayOrderedSet, OrderedSetFactory, TreapOrderedSet}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels

object SegmentSeqFactories {

  def getOrderedSetFactories[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): List[(Set[Label], OrderedSetFactory[E, D])] =
    List(
      (Set(Labels.arrayOrderedSet), ArrayOrderedSet.getFactory(domainOps)()),
      (Set(Labels.treapOrderedSet), TreapOrderedSet.getFactory(domainOps)())
    )
}
