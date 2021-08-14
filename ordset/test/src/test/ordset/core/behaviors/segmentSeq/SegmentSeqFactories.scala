package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, NonuniformArrayOrderedSet, NonuniformTreapOrderedSet, OrderedSet, OrderedSetFactory, TreapOrderedSet}
import ordset.core.value.ValueOps
import ordset.util.label.Label
import test.ordset.core.implementations.domain.BoundSelector
import test.ordset.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq
import test.ordset.core.{Labels, TestRngUtil}

object SegmentSeqFactories {

  def getOrderedSetFactories[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    boundSelector: BoundSelector[E]
  ): List[(Set[Label], OrderedSetFactory[E, D, OrderedSet[E, D]]#Partial)] =
    List(
      List(
        (
          Set(Labels.arrayOrderedSet),
          ArrayOrderedSet.getFactory.provided(domainOps)()(TestRngUtil.Implicits.defaultRngManager)
        )
      ),
      (1 to 5).map { seed =>
        (
          Set(Labels.treapOrderedSet, Labels.seed(seed)),
          TreapOrderedSet.getFactory.provided(domainOps)()(TestRngUtil.defaultRngManager(seed))
        )
      },
      (1 to 5).map { seed =>
        (
          Set(Labels.lazyTreapOrderedSet, Labels.seed(seed)),
          OrderedSetFactory.fromMapFactory(LazyTreapSegmentSeq.getRandomMapFactory)
            .provided(domainOps)()(TestRngUtil.defaultRngManager(seed))
        )
      }
    ).flatten
}
