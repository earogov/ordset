package test.ordset.core.behaviors.lazyTreapSeq.emptySet

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

trait SampleLT1[D <: Domain[Int]]
  extends LazyTreapSeqCacheTest[Int, D, Boolean] {
  self: LazyTreapSeqSample[Int, D, Boolean] =>

  override val sample: String = "LT1"

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
  override protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[Int, D, Boolean] = {
    val seq = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq.empty, complementary = false, domainOps)()

    new LazyTreapSeqSample.LazyTreapSegmentSeq(
      List(
        (0 `)`, () => seq),
        (10 `]`, () => seq),
        (20 `](`, () => seq),
        (ExtendedBound.AboveAll, () => seq)
      )
    )
  }

  override def lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[Int, D, Boolean]] = List(
//    LazyTreapSeqCacheTest.TestPackage(
//      Set(Label("A")),
//      List(
//        LazyTreapSeqCacheTest.SegmentTestCase(
//          ExtendedBound.BelowAll,
//          false forAll x,
//          List(
//            (false, EagerValue.stable[Int, D, Boolean]) forAll x
//          )
//        ),
//        LazyTreapSeqCacheTest.SegmentTestCase(
//          15`]`,
//          false forAll x,
//          List(
//            (false, EagerValue.stable[Int, D, Boolean]) forAll x
//          )
//        )
//      )
//    ),
//    LazyTreapSeqCacheTest.TestPackage(
//      Set(Label("B")),
//      List(
//        LazyTreapSeqCacheTest.SegmentTestCase(
//          5`(`,
//          false forAll x,
//          List(
//            (false, EagerValue.stable[Int, D, Boolean]) forAll x
//          )
//        ),
//        LazyTreapSeqCacheTest.SegmentTestCase(
//          ExtendedBound.AboveAll,
//          false forAll x,
//          List(
//            (false, EagerValue.stable[Int, D, Boolean]) forAll x
//          )
//        )
//      )
//    ),
//    LazyTreapSeqCacheTest.TestPackage(
//      Set(Label("C")),
//      List(
//        LazyTreapSeqCacheTest.SegmentTestCase(
//          ExtendedBound.AboveAll,
//          false forAll x,
//          List(
//            (false, EagerValue.stable[Int, D, Boolean]) forAll x
//          )
//        ),
//        LazyTreapSeqCacheTest.SegmentTestCase(
//          ExtendedBound.BelowAll,
//          false forAll x,
//          List(
//            (false, EagerValue.stable[Int, D, Boolean]) forAll x
//          )
//        )
//      )
//    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(Label("D")),
      List(
        LazyTreapSeqCacheTest.ValueTestCase(
          5 `)`,
          false,
          List(
            someLazyZvalue forAll (x < 0),
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 10),
            someLazyZvalue forAll (x > 10 & x <= 20),
            someLazyZvalue forAll (x > 20)
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          15 `)`,
          false,
          List(
            someLazyZvalue forAll (x < 0),
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 20),
            someLazyZvalue forAll (x > 20)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          -5 `)`,
          false forAll (x < 0),
          List(
            (false, EagerValue.stable[Int, D, Boolean]) forAll x
          )
        )
      )
    )
  )
}
