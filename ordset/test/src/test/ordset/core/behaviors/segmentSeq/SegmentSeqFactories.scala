package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, OrderedSetFactory, TreapOrderedSet}
import ordset.util.label.Label
import test.ordset.core.{Labels, TestRngUtil}

object SegmentSeqFactories {

  def getOrderedSetFactories[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D]
  ): List[(Set[Label], OrderedSetFactory[E, D])] =
    List(
      (
        Set(Labels.arrayOrderedSet),
        ArrayOrderedSet.getFactory(domainOps)()(TestRngUtil.Implicits.defaultRngManager)
      )
    ).appendedAll(
      Range(1, 6).map { seed =>
        (
          Set(Labels.treapOrderedSet, Labels.seed(seed)),
          TreapOrderedSet.getFactory(domainOps)()(TestRngUtil.defaultRngManager(seed))
        )
      }
    )
}
