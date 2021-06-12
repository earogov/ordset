package test.ordset.core.samples.segmentSeq.treapOrderedSet

import ordset.core.TreapSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.TreapOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.samples.segmentSeq.TreapSeqSample
import test.ordset.core.{Labels, TestRngUtil}

import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  seed: Long
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends TreapSeqSample[Int, D, Boolean](seed)
  with test.ordset.core.behaviors.segmentSeq.degenerateSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.degenerateSeq

  override val sequence: TreapSegmentSeq[Int, D, Boolean] =
    TreapOrderedSet.getFactory.unsafeBuildAsc(
      bounds, complementary, domainOps
    )(
      domainOps.boundOrd.strictValidation
    )(
      TestRngUtil.defaultRngManager(seed)
    )
}
