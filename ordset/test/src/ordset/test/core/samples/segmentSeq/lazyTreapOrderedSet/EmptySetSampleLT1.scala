package ordset.test.core.samples.segmentSeq.lazyTreapOrderedSet

import ordset.core.ExtendedBound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.TreapOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.test.core.Labels
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq
import ordset.test.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSampleLT1[D <: Domain[Int]](
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with ordset.test.core.behaviors.lazyTreapSeq.emptySet.SampleLT1[D] {

  override val labels: Set[Label] = super.labels + Labels.emptySet

  // Protected section -------------------------------------------------------- //

  // X-------------------------------------false--------------------------------------X  seq1
  //
  // X-------------------------------------false--------------------------------------X  seq2
  //
  // X-------------------------------------false--------------------------------------X  seq3
  //
  // X-------------------------------------false--------------------------------------X  seq4
  //
  //        seq1                 seq2                  seq3                seq4
  // X-----------------)[--------------------](--------------------](-----------------X
  //                   0                     10                    20
  override protected def initializeSequence: LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seq = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq.empty, complementary = false, domainOps)()

    LazyTreapSegmentSeq.totallyLazy(
      List(
        (0 `)`, () => seq),
        (10 `]`, () => seq),
        (20 `](`, () => seq),
        (ExtendedBound.AboveAll, () => seq)
      )
    )
  }
}
