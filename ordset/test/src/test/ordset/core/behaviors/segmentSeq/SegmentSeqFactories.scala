package test.ordset.core.behaviors.segmentSeq

import ordset.core.{ArrayOrderedSet, OrderedSetFactory, TreapOrderedSet}
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label
import test.ordset.core.{Labels, TestRngUtil}

object SegmentSeqFactories {

  def getOrderedSetFactories[E, D <: Domain[E]](
    implicit domainOps: DomainOps[E, D]
  ): List[(Set[Label], OrderedSetFactory[E, D])] =
    List(
      (Set(Labels.arrayOrderedSet), ArrayOrderedSet.getFactory(domainOps)()),
      (Set(Labels.treapOrderedSet), TreapOrderedSet.getFactory(TestRngUtil.defaultRng(), domainOps)())
    )
}
