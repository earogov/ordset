package ordset.test.core.samples.segmentSeq.set.lazyTreapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.SegmentSeqLabels
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.implementations.segmentSeq.lazyTreap.{LazyTreapSegmentSeq, LazyTreapSeqUtil}
import ordset.test.core.samples.segmentSeq.set.treapOrderedSet.{EmptySetSample1 => TreapEmptySetSample1}
import ordset.test.core.samples.segmentSeq.LazyTreapSeqSample
import ordset.test.core.{SegmentSeqLabels, TestRngUtil}

import scala.language.postfixOps

class EmptySetSample1[D[X] <: Domain[X]](
  seed: Long,
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with ordset.test.core.behaviors.segmentSeq.set.emptySet.Sample1[D] {

  override val labels: Set[Label] = super.labels + longSeedLabel(seed) + SegmentSeqLabels.emptySet

  // Protected section -------------------------------------------------------- //
  override protected def initializeSequence: LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seqSample = new TreapEmptySetSample1[D](seed)
    LazyTreapSeqUtil.makeRandomLazySeq(
      seqSample.sequence
    )(
      boundSelector,
      TestRngUtil.defaultRngManager(seed)
    )
  }
}
