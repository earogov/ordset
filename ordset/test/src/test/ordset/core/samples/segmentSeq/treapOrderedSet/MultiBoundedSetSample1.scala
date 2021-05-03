package test.ordset.core.samples.segmentSeq.treapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.TreapOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.{Labels, TestRngUtil}

import scala.language.postfixOps

class MultiBoundedSetSample1[D <: Domain[Int]](
  seed: Long
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends TreapSegmentSeqSample[Int, D, Boolean](seed)
  with test.ordset.core.behaviors.segmentSeq.multiBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  override def sequence: GenSegmentSeq = {
    TreapOrderedSet.fromIterableUnsafe[Int, D](bounds, complementary, domainOps)()(TestRngUtil.defaultRngManager(seed))
  }
}
