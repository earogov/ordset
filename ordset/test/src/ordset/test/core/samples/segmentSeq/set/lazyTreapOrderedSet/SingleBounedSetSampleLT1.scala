package ordset.test.core.samples.segmentSeq.set.lazyTreapOrderedSet

import ordset.core.ExtendedBound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.TreapOrderedSet
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

class SingleBoundedSetSampleLT1[D[X] <: Domain[X]](
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with ordset.test.core.behaviors.lazyTreapSeq.set.singleBoundedSet.SampleLT1[D] {

  override val labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  // Protected section -------------------------------------------------------- //

  // X----------------------------f--------------------------------X  seq1
  //
  // X---------------f------------)[---------------t---------------X  seq2
  //                               0
  //
  // X----------------------------t--------------------------------X  seq3
  //
  //        seq1                 seq2                  seq3
  // X-----------------)[--------------------](--------------------X
  //                  -10                    10
  override protected def initializeSequence: LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seq1 = TreapOrderedSet.getFactory[Int, D].buildUniform(false)
    val seq2 = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq(0 `)[`), complementary = false)
    val seq3 = TreapOrderedSet.getFactory[Int, D].buildUniform(true)

    LazyTreapSegmentSeq.totallyLazy(
      List(
        (-10 `)`, () => seq1),
        ( 10 `]`, () => seq2),
        (ExtendedBound.AboveAll, () => seq3)
      )
    )
  }
}
