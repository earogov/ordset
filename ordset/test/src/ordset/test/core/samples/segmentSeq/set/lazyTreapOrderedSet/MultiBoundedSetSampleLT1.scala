package ordset.test.core.samples.segmentSeq.set.lazyTreapOrderedSet

import ordset.core.ExtendedBound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.TreapOrderedSet
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.core.SegmentSeqLabels
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq
import ordset.test.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class MultiBoundedSetSampleLT1[D[X] <: Domain[X]](
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with ordset.test.core.behaviors.lazyTreapSeq.set.multiBoundedSet.SampleLT1[D] {

  override val labels: Set[Label] = super.labels + SegmentSeqLabels.multiBoundedSeq

  // Protected section -------------------------------------------------------- //

  // X-t--)[---f--](-----true-----)[------------false--------------X  seq1
  //      -10     -5              5
  //
  // X----------false--------)[----t---](-------f------)[-----t----X  seq2
  //                         2         8               17
  //
  // X-----------------------false----------------)[--t---](---f---X  seq3
  //                                              15      20
  //
  //        seq1                 seq2                  seq3
  // X-----------------)[--------------------](--------------------X
  //                   0                     10
  override protected def initializeSequence: LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seq1 = TreapOrderedSet.getFactory.unsafeBuild(
      ArraySeq(-10 `)[`, -5 `](`, 5 `)[`),
      complementary = true
    )

    val seq2 = TreapOrderedSet.getFactory.unsafeBuild(
      ArraySeq(2 `)[`, 8 `](`, 17 `)[`),
      complementary = false
    )

    val seq3 = TreapOrderedSet.getFactory.unsafeBuild(
      ArraySeq(15 `)[`, 20 `](`),
      complementary = false
    )

    LazyTreapSegmentSeq.totallyLazy(
      List(
        (0 `)`, () => seq1),
        (10 `]`, () => seq2),
        (ExtendedBound.AboveAll, () => seq3)
      )
    )
  }
}
