package ordset.test.core.samples.segmentSeq.set.treapOrderedSet

import ordset.core.segmentSeq.TreapSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.TreapOrderedSet
import ordset.core.syntax.SetBuilderNotation.*
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.TreapSeqSample
import ordset.test.core.{Labels, TestRngUtil}

import scala.language.postfixOps

class UniversalSetSample1[D[X] <: Domain[X]](
  seed: Long
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends TreapSeqSample[Int, D, Boolean](seed)
  with ordset.test.core.behaviors.segmentSeq.set.universalSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.universalSet

  override val sequence: TreapSegmentSeq[Int, D, Boolean] =
    TreapOrderedSet.getFactory.unsafeBuildAsc(
      bounds, 
      complementary
    )(
      domainOps,
      TestRngUtil.defaultRngManager(seed)
    )
}
