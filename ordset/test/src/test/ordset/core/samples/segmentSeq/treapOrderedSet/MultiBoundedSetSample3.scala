package test.ordset.core.samples.segmentSeq.treapOrderedSet

import ordset.core.TreapOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.commons.ApacheCommonsRng
import ordset.util.label.Label
import test.ordset.core.Labels

import scala.language.postfixOps

class MultiBoundedSetSample3[D <: Domain[Int]](
  seed: Long
)(
  implicit override val domainOps: DomainOps[Int, D]
) extends TreapSegmentSeqSample[Int, D, Boolean](seed)
  with test.ordset.core.behaviors.segmentSeq.multiBoundedSet.Sample3[D] {

  override def labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  override def sequence: GenSegmentSeq = {
    TreapOrderedSet.fromIterableUnsafe[Int, D](bounds, ApacheCommonsRng.default(seed), complementary, domainOps)()
  }
}
