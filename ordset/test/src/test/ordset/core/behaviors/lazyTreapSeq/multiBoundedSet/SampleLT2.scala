package test.ordset.core.behaviors.lazyTreapSeq.multiBoundedSet

import ordset.core.AbstractLazyTreapSegmentSeq.EagerValue
import ordset.core.ExtendedBound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.lazyTreapSeq.LazyTreapSeqCacheTest
import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchTest
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait SampleLT2[D <: Domain[Int]]
  extends LazyTreapSeqCacheTest[Int, D, Boolean] {
  self: LazyTreapSeqSample[Int, D, Boolean] =>

  override val sample: String = "LT2"

  // Protected section -------------------------------------------------------- //

  // X-f--)[---t--](------f------)[---------------t------------------------------------X  seq1
  //      -10     -5              5
  //
  // X--------t--------)[---------f----------](----------t-----------------------------X  seq2
  //                   0                     10
  //
  // X-----------------------f-------------------)[-t-](-f-)[------t------)[-----f-----X  seq3
  //                                             15   20   25             35
  //
  // X--------t--------)[--------------------------------f-----------------------------X  seq4
  //                   0
  //
  //        seq1                 seq2                  seq3                  seq4
  // X-----------------)[--------------------](--------------------)[------------------X
  //                   0                     10                    30
  override protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seq1 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      ArraySeq(-10 `)[`, -5 `](`, 5 `)[`),
      complementary = false,
      domainOps
    )()

    val seq2 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      ArraySeq(0 `)[`, 10 `](`),
      complementary = true,
      domainOps
    )()

    val seq3 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      ArraySeq(15 `)[`, 20 `](`, 25 `)[`, 35 `)[`),
      complementary = false,
      domainOps
    )()

    val seq4 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      ArraySeq(0 `)[`),
      complementary = true,
      domainOps
    )()

    new LazyTreapSeqSample.LazyTreapSegmentSeq(
      List(
        (0 `)`, () => seq1),
        (10 `]`, () => seq2),
        (30 `)`, () => seq3),
        (AboveAll, () => seq4)
      )
    )
  }

  override def lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[Int, D, Boolean]] = List(
    LazyTreapSeqCacheTest.TestPackage(
      Set(Label("A")),
      List(
        LazyTreapSeqCacheTest.TestCase(
          5 `(`,
          false forAll (x > -5 & x < 15),
          List(
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x < -10),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        ),
        LazyTreapSeqCacheTest.TestCase(
          27 `(`,
          true forAll (x >= 25 & x < 30),
          List(
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x < -10),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x >= 30)
          )
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(Label("B")),
      List(
        LazyTreapSeqCacheTest.TestCase(
          23 `(`,
          false forAll (x > 20 & x < 25),
          List(
            someLazyZvalue forAll (x < 0),
            someLazyZvalue forAll (x >= 0 & x <= 10),
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x > 10 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        ),
        LazyTreapSeqCacheTest.TestCase(
          -2 `(`,
          false forAll (x > -5 & x < 15),
          List(
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x < -10),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        ),
        LazyTreapSeqCacheTest.TestCase(
          40 `(`,
          false forAll (x >= 30),
          List(
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x < -10),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x >= 30)
          )
        )
      )
    )
  )
}
