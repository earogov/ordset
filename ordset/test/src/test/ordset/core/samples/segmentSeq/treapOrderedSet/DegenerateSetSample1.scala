package test.ordset.core.samples.segmentSeq.treapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.TreapOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.{Labels, TestRngUtil}

import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  seed: Long
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends TreapSegmentSeqSample[Int, D, Boolean](seed)
  with test.ordset.core.behaviors.segmentSeq.degenerateSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.degenerateSeq

  override def sequence: GenSegmentSeq =
    TreapOrderedSet.getFactory.unsafeBuildAsc(
      bounds, complementary, domainOps
    )(
      domainOps.boundOrd.strictValidation
    )(
      TestRngUtil.defaultRngManager(seed)
    )
}
