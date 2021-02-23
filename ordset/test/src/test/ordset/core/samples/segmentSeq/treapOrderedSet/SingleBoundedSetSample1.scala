package test.ordset.core.samples.segmentSeq.treapOrderedSet

import ordset.core.TreapOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RandomUtil
import ordset.util.label.Label
import test.ordset.core.Labels

import scala.language.postfixOps

class SingleBoundedSetSample1[D <: Domain[Int]](
  seed: Int
)(
  implicit override val domainOps: DomainOps[Int, D]
) extends TreapSegmentSeqSample[Int, D, Boolean](seed)
  with test.ordset.core.behaviors.segmentSeq.singleBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  override def sequence: GenSegmentSeq =
    TreapOrderedSet.fromIterableUnsafe[Int, D](bounds, RandomUtil.intLazyList(seed), complementary, domainOps)()
}