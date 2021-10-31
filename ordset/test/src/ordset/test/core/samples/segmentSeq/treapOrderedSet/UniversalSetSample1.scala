package ordset.test.core.samples.segmentSeq.treapOrderedSet

import ordset.core.TreapSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.TreapOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.TreapSeqSample
import ordset.test.core.{Labels, TestRngUtil}

import scala.language.postfixOps

class UniversalSetSample1[D <: Domain[Int]](
  seed: Long
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends TreapSeqSample[Int, D, Boolean](seed)
  with ordset.test.core.behaviors.segmentSeq.universalSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.universalSet

  override val sequence: TreapSegmentSeq[Int, D, Boolean] =
    TreapOrderedSet.getFactory.unsafeBuildAsc(
      bounds, complementary, domainOps
    )(
      domainOps.boundOrd.strictValidation
    )(
      TestRngUtil.defaultRngManager(seed)
    )
}
