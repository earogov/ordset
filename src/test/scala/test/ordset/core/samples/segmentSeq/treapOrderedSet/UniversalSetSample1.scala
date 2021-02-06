package test.ordset.core.samples.segmentSeq.treapOrderedSet

import ordset.core.TreapOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.RandomUtil
import ordset.util.label.Label
import test.ordset.core.Labels

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class UniversalSetSample1[D <: Domain[Int]](
  seed: Int
)(
  implicit override val domainOps: DomainOps[Int, D]
) extends TreapSegmentSeqSample[Int, D, Boolean](seed)
  with test.ordset.core.behaviors.segmentSeq.universalSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.universalSet

  override def sequence: GenSegmentSeq =
    TreapOrderedSet.fromIterableUnsafe[Int, D](bounds, RandomUtil.intLazyList(seed), complementary, domainOps)()
}
