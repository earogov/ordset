package test.ordset.core.samples.segmentSeq.lazyTreapOrderedSet

import ordset.core.ExtendedBound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.TreapOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import test.ordset.core.Labels
import test.ordset.core.implementations.domain.BoundSelector
import test.ordset.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class MultiBoundedSetSampleLT1[D <: Domain[Int]](
  shuffled: Boolean
)(
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends LazyTreapSeqSample.Fixed[Int, D, Boolean](shuffled)
  with test.ordset.core.behaviors.lazyTreapSeq.multiBoundedSet.SampleLT1[D] {

  override val labels: Set[Label] = super.labels + Labels.multiBoundedSeq

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
    val seq1 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      ArraySeq(-10 `)[`, -5 `](`, 5 `)[`),
      complementary = true,
      domainOps
    )()

    val seq2 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      ArraySeq(2 `)[`, 8 `](`, 17 `)[`),
      complementary = false,
      domainOps
    )()

    val seq3 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      ArraySeq(15 `)[`, 20 `](`),
      complementary = false,
      domainOps
    )()

    LazyTreapSegmentSeq.totallyLazy(
      List(
        (0 `)`, () => seq1),
        (10 `]`, () => seq2),
        (ExtendedBound.AboveAll, () => seq3)
      )
    )
  }
}
