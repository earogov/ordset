package test.ordset.core.samples.segmentSeq.lazyTreapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.domain.BoundSelector
import test.ordset.core.{Labels, TestRngUtil}
import test.ordset.core.samples.segmentSeq.{LazyTreapSeqSample, LazyTreapSeqUtil}
import test.ordset.core.samples.segmentSeq.treapOrderedSet.{MultiBoundedSetSample3 => TreapMultiBoundedSetSample3}

import scala.language.postfixOps

class MultiBoundedSetSample3[D <: Domain[Int]](
  seed: Long,
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with test.ordset.core.behaviors.segmentSeq.multiBoundedSet.Sample3[D] {

  override val labels: Set[Label] = super.labels + Labels.seed(seed) + Labels.multiBoundedSeq

  // Protected section -------------------------------------------------------- //
  override protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seqSample = new TreapMultiBoundedSetSample3[D](seed)
    LazyTreapSeqUtil.makeRandomLazySeq(
      seqSample.sequence
    )(
      BoundSelector.intBoundSelector,
      TestRngUtil.defaultRngManager(seed)
    )
  }
}
