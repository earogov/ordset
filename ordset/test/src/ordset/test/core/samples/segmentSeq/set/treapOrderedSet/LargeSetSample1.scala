package ordset.test.core.samples.segmentSeq.set.treapOrderedSet

import ordset.core.segmentSeq.TreapSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.TreapOrderedSet
import ordset.core.syntax.SetBuilderNotation.*
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.TreapSeqSample
import ordset.test.core.{SegmentSeqLabels, TestRngUtil}

import scala.language.postfixOps

class LargeSetSample1[D[X] <: Domain[X]](
  seed: Long
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends TreapSeqSample[Int, D, Boolean](seed)
  with ordset.test.core.behaviors.segmentSeq.set.multiBoundedSet.LargeSample1[D] {

  override val labels: Set[Label] = super.labels + SegmentSeqLabels.multiBoundedSeq

  override val sequence: TreapSegmentSeq[Int, D, Boolean] =
    TreapOrderedSet.getFactory.unsafeBuild(
      bounds, 
      complementary
    )(
      domainOps,
      TestRngUtil.defaultRngManager(seed)
    )
}
