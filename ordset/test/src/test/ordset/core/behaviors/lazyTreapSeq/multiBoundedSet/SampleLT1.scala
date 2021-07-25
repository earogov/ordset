package test.ordset.core.behaviors.lazyTreapSeq.multiBoundedSet

import ordset.core.AbstractLazyTreapSegmentSeq.EagerValue
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.core.ExtendedBound
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.lazyTreapSeq.LazyTreapSeqCacheTest
import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchTest
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait SampleLT1[D <: Domain[Int]]
  extends LazyTreapSeqCacheTest[Int, D, Boolean] {
  self: LazyTreapSeqSample[Int, D, Boolean] =>

  override val sample: String = "LT1"

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
  override protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[Int, D, Boolean] = {
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
  
    new LazyTreapSeqSample.LazyTreapSegmentSeq(
      List(
        (0 `)`, () => seq1),
        (10 `]`, () => seq2),
        (ExtendedBound.AboveAll, () => seq3)
      )
    )
  }

  override def lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[Int, D, Boolean]] = List(
    LazyTreapSeqCacheTest.TestPackage(
      Set(Label("A")),
      List(
        LazyTreapSeqCacheTest.TestCase(
          5`(`,
          true forAll (x >= 2 & x <= 8),
          List(
            someLazyZvalue forAll (x < 0),
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x > 8 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        ),
        LazyTreapSeqCacheTest.TestCase(
          17`(`,
          true forAll (x >= 15 & x <= 20),
          List(
            someLazyZvalue forAll (x < 0),
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 8 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20)
          )
        ),
        LazyTreapSeqCacheTest.TestCase(
          -5`(`,
          true forAll (x > -5 & x < 0),
          List(
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x < -10),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 0),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 8 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20)
          )
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(Label("B")),
      List(
        LazyTreapSeqCacheTest.TestCase(
          12`(`,
          false forAll (x > 8 & x < 15),
          List(
            someLazyZvalue forAll (x < 0),
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 8 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20)
          )
        ),
        LazyTreapSeqCacheTest.TestCase(
          -30`(`,
          true forAll (x < -10),
          List(
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x < -10),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 0),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 8 & x < 15),
            (true, EagerValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x > 20)
          )
        )
      )
    )
  )
}
