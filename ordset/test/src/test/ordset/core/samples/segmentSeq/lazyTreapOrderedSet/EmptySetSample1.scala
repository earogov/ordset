package test.ordset.core.samples.segmentSeq.lazyTreapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.domain.BoundSelector
import test.ordset.core.samples.segmentSeq.treapOrderedSet.{EmptySetSample1 => TreapEmptySetSample1}
import test.ordset.core.samples.segmentSeq.{LazyTreapSeqSample, LazyTreapSeqUtil}
import test.ordset.core.{Labels, TestRngUtil}

import scala.language.postfixOps

class EmptySetSample1[D <: Domain[Int]](
  seed: Long,
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with test.ordset.core.behaviors.segmentSeq.emptySet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.seed(seed) + Labels.emptySet

  // Protected section -------------------------------------------------------- //
  override protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seqSample = new TreapEmptySetSample1[D](seed)
    LazyTreapSeqUtil.makeRandomLazySeq(
      seqSample.sequence
    )(
      BoundSelector.intBoundSelector,
      TestRngUtil.defaultRngManager(seed)
    )
  }
}
