package ordset.test.core.samples.segmentSeq.set.lazyTreapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.implementations.segmentSeq.lazyTreap.{LazyTreapSegmentSeq, LazyTreapSeqUtil}
import ordset.test.core.{Labels, TestRngUtil}
import ordset.test.core.samples.segmentSeq.LazyTreapSeqSample
import ordset.test.core.samples.segmentSeq.set.treapOrderedSet.{MultiBoundedSetSample3 => TreapMultiBoundedSetSample3}

import scala.language.postfixOps

class MultiBoundedSetSample3[D[X] <: Domain[X]](
  seed: Long,
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with ordset.test.core.behaviors.segmentSeq.set.multiBoundedSet.Sample3[D] {

  override val labels: Set[Label] = super.labels + Labels.seed(seed) + Labels.multiBoundedSeq

  // Protected section -------------------------------------------------------- //
  override protected def initializeSequence: LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seqSample = new TreapMultiBoundedSetSample3[D](seed)
    LazyTreapSeqUtil.makeRandomLazySeq(
      seqSample.sequence
    )(
      boundSelector,
      TestRngUtil.defaultRngManager(seed)
    )
  }
}
