package test.ordset.core.samples.segmentSeq.lazyTreapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.implementations.domain.BoundSelector
import test.ordset.core.implementations.segmentSeq.lazyTreap.{LazyTreapSegmentSeq, LazyTreapSeqUtil}
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample
import test.ordset.core.samples.segmentSeq.treapOrderedSet.DegenerateSetSample1 as TreapDegenerateSetSample1
import test.ordset.core.{Labels, TestRngUtil}

import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  seed: Long,
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with test.ordset.core.behaviors.segmentSeq.degenerateSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.seed(seed) + Labels.degenerateSeq

  // Protected section -------------------------------------------------------- //
  override protected def initializeSequence: LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seqSample = new TreapDegenerateSetSample1[D](seed)
    LazyTreapSeqUtil.makeRandomLazySeq(
      seqSample.sequence
    )(
      boundSelector,
      TestRngUtil.defaultRngManager(seed)
    )
  }
}
