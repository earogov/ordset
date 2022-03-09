package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.{ArrayOrderedSet, NonuniformArrayOrderedSet, NonuniformTreapOrderedSet}
import ordset.core.segmentSeq.set.{OrderedSet, OrderedSetFactory, TreapOrderedSet}
import ordset.core.value.ValueOps
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq
import ordset.test.core.{SegmentSeqLabels, TestRngUtil}

object SegmentSeqFactories {

  def getOrderedSetFactories[E, D[X] <: Domain[X]](
    implicit
    domainOps: DomainOps[E, D],
    boundSelector: BoundSelector[E]
  ): List[(Set[Label], OrderedSetFactory[E, D, OrderedSet[E, D]]#Provided)] =
    List(
      List(
        (
          Set(SegmentSeqLabels.arrayOrderedSet),
          ArrayOrderedSet.getFactory.provided(domainOps, TestRngUtil.Givens.defaultRngManager)
        )
      ),
      (1 to 5).map { seed =>
        (
          Set(SegmentSeqLabels.treapOrderedSet, intSeedLabel(seed)),
          TreapOrderedSet.getFactory.provided(domainOps, TestRngUtil.defaultRngManager(seed))
        )
      },
      (1 to 5).map { seed =>
        (
          Set(SegmentSeqLabels.lazyTreapOrderedSet, intSeedLabel(seed)),
          OrderedSetFactory.fromMapFactory(LazyTreapSegmentSeq.getRandomMapFactory)
            .provided(domainOps, TestRngUtil.defaultRngManager(seed))
        )
      }
    ).flatten
}
