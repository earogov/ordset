package test.ordset.core.samples.segmentSeq.lazyTreapOrderedSet

import ordset.core.ExtendedBound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.TreapOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class SingleBounedSetSampleLT1[D <: Domain[Int]](
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with test.ordset.core.behaviors.lazyTreapSeq.singleBoundedSet.SampleLT1[D] {

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
  override protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seq1 = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq.empty, complementary = false, domainOps)()
    val seq2 = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq(0 `)[`), complementary = false, domainOps)()
    val seq3 = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq.empty, complementary = true, domainOps)()

    LazyTreapSeqSample.LazyTreapSegmentSeq.totallyLazy(
      List(
        (-10 `)`, () => seq1),
        ( 10 `]`, () => seq2),
        (ExtendedBound.AboveAll, () => seq3)
      )
    )
  }
}
