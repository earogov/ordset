package test.ordset.core.behaviors.segmentSeq.multiBoundedSet

import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.core.{Bound, ExtendedBound, SegmentSeq}
import ordset.util.label.Label
import test.ordset.core.behaviors.TestTuple
import test.ordset.core.behaviors.segmentSeq._
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample1[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
    with SegmentContainsTest[Int, D, Boolean]
    with SegmentRestrictBoundTest[Int, D, Boolean]
    with SegmentSeqPrependedTest[Int, D, Boolean]
    with SegmentSeqAppendedTest[Int, D, Boolean]
    with SegmentSeqSlicedTest[Int, D, Boolean]
    with SegmentPatchedTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override def sample: String = "1"

  override def bounds: IterableOnce[Bound.Upper[Int]] =
    ArraySeq(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`)

  override def complementary: Boolean = false

  override def reference: Seq[GenIntervalRelation] =
    (false forAll x <  0) ::
    (true  forAll x >= 0  & x < 10) ::
    (false forAll x >= 10 & x < 20) ::
    (true  forAll x >= 20 & x < 30) ::
    (false forAll x >= 30 & x < 40) ::
    (true  forAll x >= 40) ::
    Nil

  override def moveToBoundCases: Seq[(ExtendedBound[Int], GenIntervalRelation)] =
    (ExtendedBound.BelowAll, false forAll x <  0) ::
    (10`)`, true  forAll x >= 0  & x < 10) ::
    (10`)`, true  forAll x >= 0  & x < 10) ::
    (ExtendedBound.AboveAll, true  forAll x >= 40) ::
    (30`[`, false forAll x >= 30 & x < 40) ::
    (40`)`, false forAll x >= 30 & x < 40) ::
    (40`[`, true  forAll x >= 40) ::
    (45`[`, true  forAll x >= 40) ::
    (ExtendedBound.BelowAll, false forAll x <  0) ::
    (25`[`, true  forAll x >= 20 & x < 30) ::
    (-5`[`, false forAll x <  0) ::
    Nil

  override def containsCases: Seq[SegmentContainsTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentContainsTest.TestCase(
        bound = -10`]`,
        includedBounds = List(ExtendedBound.BelowAll, -10`[`, 0`)`),
        excludedBounds = List(ExtendedBound.AboveAll, 10`(`, 0`[`)
      ),
      SegmentContainsTest.TestCase(
        bound = 25`]`,
        includedBounds = List(20`]`, 20`[`, 20`(`, 25`)`, 30`)`),
        excludedBounds = List(ExtendedBound.BelowAll, ExtendedBound.AboveAll, 20`)`, 30`]`, 30`[`, 30`(`)
      ),
      SegmentContainsTest.TestCase(
        bound = 40`]`,
        includedBounds = List(ExtendedBound.AboveAll, 40`]`, 40`[`, 40`(`, 50`(`),
        excludedBounds = List(ExtendedBound.BelowAll, 40`)`, 0`(`)
      )
    )

  override def restrictCases: Seq[SegmentRestrictBoundTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentRestrictBoundTest.TestCase(
        bound = -10`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, ExtendedBound.BelowAll),
          TestTuple(ExtendedBound.AboveAll, 0`)`),
          TestTuple(-10`]`, -10`]`),
          TestTuple(0`]`, 0`)`),
          TestTuple(10`(`, 0`)`)
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 15`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 10`[`),
          TestTuple(ExtendedBound.AboveAll, 20`)`),
          TestTuple(15`]`, 15`]`),
          TestTuple(10`)`, 10`[`),
          TestTuple(0`(`, 10`[`),
          TestTuple(20`]`, 20`)`),
          TestTuple(25`]`, 20`)`)
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 50`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 40`[`),
          TestTuple(ExtendedBound.AboveAll, ExtendedBound.AboveAll),
          TestTuple(15`]`, 40`[`),
          TestTuple(40`)`, 40`[`),
          TestTuple(40`(`, 40`(`),
          TestTuple(50`]`, 50`]`)
        )
      )
    )

  override def prependedCases: Seq[SegmentSeqPrependedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        // prepended:
        // X------------------false-----------------------X
        //
        // result:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        // prepended:
        // X-------------------true-----------------------X
        //
        // result:
        // X------t-------)[ ............Seq1............ X
        //                10
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          (true  forAll x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        // prepended:
        // X-f-](-----true-----)[-----false-----](--true--X
        //     -5              25               30
        // result:
        // X-f-](----t----)[ ............Seq1............ X
        //     -5         10
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5 `](`, 30 `)[`, 50 `](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5 & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        // prepended:
        // X-t-](-----false----)[------true-----](---f----X
        //     -5              25               30
        // result:
        // X-t-](f)[--t---)[ ............Seq1............ X
        //     -5  0      10
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5 `](`, 30 `)[`, 50 `](`), complementary = true),
          (true  forAll x <= -5) ::
          (false forAll x >  -5 & x < 0) ::
          (true  forAll x >=  0 & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        // prepended:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        // result:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("E"),
          factoryTuple._2.unsafeBuildAsc(bounds, complementary = complementary),
          reference
        ),
        // current:
        // X---f--)[---t--)[ ............Seq1............ X
        //        0       10
        // prepended:
        // X---t--)[---------------false------------------X
        //        0
        // result:
        // X------t-------)[ ............Seq1............ X
        //                10
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("F"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0 `](`), complementary = true),
          (true  forAll x <  10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        )
      )
    }
  }

  override def prependedWithBoundCases: Seq[SegmentSeqPrependedTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //   bound
        //     )
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // prepended:
        // X------------------false-----------------------X
        //
        // result:
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          -10 `)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        //      bound
        //        )
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // prepended:
        // X------------------false-----------------------X
        //
        // result:
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A2"),
          0`)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        //           bound
        //             [
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // prepended:
        // X------------------false-----------------------X
        //
        // result:
        // X-----f----)[t-)[---f--)[---t--)[---f--)[---t--X
        //            5   10      20      30      40
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A3"),
          5`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x <  5) ::
          (true  forAll x >= 5  & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        //                       bound
        //                         ]
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // prepended:
        // X------------------false-----------------------X
        //
        // result:
        //
        // X---------false---------](--t--)[---f--)[---t--X
        //                         20     30      40
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A4"),
          20`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x <= 20) ::
          (true  forAll x >  20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        //                                           bound
        //                                             (
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // prepended:
        // X------------------false-----------------------X
        //
        // result:
        // X----------------false---------------------](t-X
        //                                            45
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A5"),
          45`(`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x <= 45) ::
          (true  forAll x > 45) ::
          Nil
        ),
        // current:
        //            bound
        //             (
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // prepended:
        // X--true----](---false--)[---true---](---false--X
        //            5           20          35
        //
        // result:
        // X-----true-----)[---f--)[---t--)[---f--)[---t--X
        //                10      20      30      40
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          5`(`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(5 `](`, 20 `)[`, 35 `](`), complementary = true),
          (true  forAll x <  10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        //                   bound
        //                     )
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // prepended:
        // X--true----](---false--)[---true---](---false--X
        //            5           20          35
        //
        // result:
        // X--true----](---false--)[---t--)[---f--)[---t--X
        //            5           20      30      40
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B2"),
          15`)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(5 `](`, 20 `)[`, 35 `](`), complementary = true),
          (true  forAll x <= 5) ::
          (false forAll x >  5  & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        //                      bound
        //                        )
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // prepended:
        // X--true----](---false--)[---true---](---false--X
        //            5           20          35
        //
        // result:
        // X--true----](---false--)[---t--)[---f--)[---t--X
        //            5           20      30      40
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B3"),
          20`)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(5 `](`, 20 `)[`, 35 `](`), complementary = true),
          (true  forAll x <= 5) ::
          (false forAll x >  5  & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        )
      )
    }
  }

  override def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        // appended:
        // X------------------false-----------------------X
        //
        // result:
        // X ......Seq1....... )[----------false----------X
        //                     30
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30) ::
          Nil
        ),
        // current:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        // appended:
        // X------------------true------------------------X
        //
        // result:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        // appended:
        // X--false--](--true--)[-----false-----](--true--X
        //           25        30               50
        // result:
        // X ......Seq1....... )[-----false-----](--true--X
        //                     30               50
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25 `](`, 30 `)[`, 50 `](`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <= 50) ::
          (true  forAll x >  50) ::
          Nil
        ),
        // current:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        // appended:
        // X--true--](--false--)[-----true-----](--false--X
        //          25         30              50
        // result:
        // X ......Seq1....... )[--false--)[-t-](--false--X
        //                     30         40   50
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25 `](`, 30 `)[`, 50 `](`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50) ::
          Nil
        ),
        // current:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        // appended:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        // result:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("E"),
          factoryTuple._2.unsafeBuildAsc(bounds, complementary),
          reference
        ),
        // current:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        // appended:
        // X--------------true------------)[----false-----X
        //                                40
        // result:
        // X ......Seq1....... )[----------false----------X
        //                     30
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("F"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(40 `)[`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30) ::
          Nil
        )
      )
    }
  }
  
  override def appendedWithBoundCases: Seq[SegmentSeqAppendedTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //   bound
        //     )
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // appended:
        // X------------------false-----------------------X
        //
        // result:
        // X------------------false-----------------------X
        //                     
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          -10`)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x) ::
          Nil
        ),
        // current:
        //      bound
        //        )
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // appended:
        // X------------------false-----------------------X
        //
        // result:
        // X------------------false-----------------------X
        //                     
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A2"),
          0`)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x) :: 
          Nil
        ),
        // current:
        //           bound
        //             [
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // appended:
        // X------------------false-----------------------X
        //
        // result:
        // X---f--)[t-)[------false-----------------------X
        //        0   5
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A3"),
          5`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x < 0) ::
          (true  forAll x >= 0 & x < 5) ::
          (false forAll x >= 5) ::
          Nil
        ),
        // current:
        //                        bound
        //                          (
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // appended:
        // X------------------false-----------------------X
        //
        // result:
        //                         t
        // X---f--)[---t--)[---f--)|(--------false--------X
        //        0       10      20
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A4"),
          20`(`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x < 0) ::
          (true  forAll x >= 0 & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x <= 20) ::
          (false forAll x > 20) ::
          Nil
        ),
        // current:
        //                                           bound
        //                                             (
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // appended:
        // X------------------false-----------------------X
        //
        // result:
        // X---f--)[---t--)[---f--)[---t--)[---f--)[t-](f-X
        //        0       10      20      30      40 45
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A5"),
          45`(`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x < 0) ::
          (true  forAll x >= 0 & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40 & x <= 45) ::
          (false forAll x > 45) ::
          Nil
        ),
        // current:
        //            bound
        //             (
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // appended:
        // X--true----](---false--)[---true---](---false--X
        //            5           20          35
        //
        // result:
        // X---f--)[t-](---false--)[---true---](---false--X
        //        0   5           20          35
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          5`(`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(5 `](`, 20 `)[`, 35 `](`), complementary = true),
          (false forAll x < 0) ::
          (true  forAll x >= 0 & x <= 5) ::
          (false forAll x > 5 & x < 20) ::
          (true  forAll x >= 20 & x <= 35) ::
          (false forAll x > 35) ::
          Nil
        ),
        // current:
        //                   bound
        //                     )
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // appended:
        // X--true----](---false--)[---true---](---false--X
        //            5           20          35
        //
        // result:
        // X---f--)[---t--)[---f--)[---true---](---false--X
        //        0       10      20          35
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B2"),
          15`)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(5 `](`, 20 `)[`, 35 `](`), complementary = true),
          (false forAll x < 0) ::
          (true  forAll x >= 0 & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x <= 35) ::
          (false forAll x > 35) ::
          Nil
        ),
        // current:
        //                      bound
        //                        )
        // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
        //        0       10      20      30      40
        // appended:
        // X--true----](---false--)[---true---](---false--X
        //            5           20          35
        //
        // result:
        // X---f--)[---t--)[---f--)[---true---](---false--X
        //        0       10      20          35
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B3"),
          20`)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(5 `](`, 20 `)[`, 35 `](`), complementary = true),
          (false forAll x < 0) ::
          (true  forAll x >= 0 & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x <= 35) ::
          (false forAll x > 35) ::
          Nil
        )
      )
    }
  }

  override def slicedCases: Seq[SegmentSeqSlicedTest.TestCase[Int, D, Boolean]] =
    List(
      // current:
      //   bound
      //     )
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenBelow:
      // X------------------false-----------------------X
      //
      // takenAbove:
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      SegmentSeqSlicedTest.TestCase(
        -10`(`,
        (false forAll x) ::
        Nil,
        reference
      ),
      // current:
      //      bound
      //        )
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenBelow:
      // X------------------false-----------------------X
      //
      // takenAbove:
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      SegmentSeqSlicedTest.TestCase(
        0`)`,
        (false forAll x) ::
        Nil,
        reference
      ),
      // current:
      //                   bound
      //                     [
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenBelow:
      // X---f--)[---t--)[-----------false--------------X
      //        0       10
      // takenAbove:
      // X---------false--------)[---t--)[---f--)[---t--X
      //                        20      30      40
      SegmentSeqSlicedTest.TestCase(
        15`[`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x < 10) ::
        (false forAll x >= 10 ) ::
        Nil,
        (false forAll x < 20) ::
        (true  forAll x >= 20 & x < 30) ::
        (false forAll x >= 30 & x < 40) ::
        (true  forAll x >= 40) ::
        Nil
      ),
      // current:
      //                      bound
      //                        )
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenBelow:
      // X---f--)[---t--)[-----------false--------------X
      //        0       10
      // takenAbove:
      // X---------false--------)[---t--)[---f--)[---t--X
      //                        20      30      40
      SegmentSeqSlicedTest.TestCase(
        20`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x < 10) ::
        (false forAll x >= 10 ) ::
        Nil,
        (false forAll x < 20) ::
        (true  forAll x >= 20 & x < 30) ::
        (false forAll x >= 30 & x < 40) ::
        (true  forAll x >= 40) ::
        Nil
      ),
      // current:
      //                       bound
      //                         ]
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenBelow:
      // X---f--)[---t--)[---f--)[---------true---------X
      //        0       10      20
      // takenAbove:
      // X--------------true------------)[---f--)[---t--X
      //                                30      40
      SegmentSeqSlicedTest.TestCase(
        20`]`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x < 10) ::
        (false forAll x >= 10 & x < 20) ::
        (true  forAll x >= 20 ) ::
        Nil,
        (true  forAll x <  30) ::
        (false forAll x >= 30 & x < 40) ::
        (true  forAll x >= 40) ::
        Nil
      ),
      // current:
      //                        bound
      //                          (
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenBelow:
      // X---f--)[---t--)[---f--)[---------true---------X
      //        0       10      20
      // takenAbove:
      // X---------------true-----------)[---f--)[---t--X
      //                                30      40
      SegmentSeqSlicedTest.TestCase(
        20`(`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x < 10) ::
        (false forAll x >= 10 & x < 20) ::
        (true  forAll x >= 20 ) ::
        Nil,
        (true  forAll x <  30) ::
        (false forAll x >= 30 & x < 40) ::
        (true  forAll x >= 40) ::
        Nil
      ),
      // current:
      //                                      bound
      //                                        )
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenBelow:
      // X---f--)[---t--)[---f--)[---t--)[----false-----X
      //        0       10      20      30
      // takenAbove:
      // X-----------------false----------------)[---t--X
      //                                        40
      SegmentSeqSlicedTest.TestCase(
        40`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x < 10) ::
        (false forAll x >= 10 & x < 20) ::
        (true  forAll x >= 20 & x < 30) ::
        (false forAll x >= 30 ) ::
        Nil,
        (false forAll x <  40) ::
        (true  forAll x >= 40) ::
        Nil
      ),
      // current:
      //                                           bound
      //                                             [
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenBelow:
      // X---f--)[---t--)[---f--)[---t--)[---f--)[---t--X
      //        0       10      20      30      40
      // takenAbove:
      // X---------------------true---------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        50`[`,
        reference,
        (true  forAll x) ::
        Nil
      ),
    )
    
  override def patchedCases: Seq[SegmentPatchedTest.TestCase[Int, D, Boolean]] =
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // patched segment
        // X-----f----)[-----t----)[-----f----)[-----t----)[-----f----)[-----t----X
        //            0           10          20          30          40
        // patch:
        // X-f--)[---true---)[-----------------------false------------------------X
        //      -5          5
        // result:
        // X-f--)[------true------)[-----f----)[-----t----)[-----f----)[-----t----X
        //      -5                10          20          30          40
        SegmentPatchedTest.TestCase(
          factoryTuple._1 + Label("A1"),
          -5`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5 `)[`, 5 `)[`), complementary = false),
          (false forAll x <  -5) ::
          (true  forAll x >= -5 & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        // patched segment
        // X-----f----)[-----t----)[-----f----)[-----t----)[-----f----)[-----t----X
        //            0           10          20          30          40
        // patch:
        // X-t--)[--false---)[-----------------------true-------------------------X
        //      -5          5
        // result:
        // X-t--)[-f--)[-----t----)[-----f----)[-----t----)[-----f----)[-----t----X
        //      -5    0           10          20          30          40
        SegmentPatchedTest.TestCase(
          factoryTuple._1 + Label("A2"),
          -5`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5 `)[`, 5 `)[`), complementary = true),
          (true  forAll x <  -5) ::
          (false forAll x >= -5 & x < 0) ::
          (true  forAll x >=  0 & x < 10) ::
          (false forAll x >= 10 & x < 20) ::
          (true  forAll x >= 20 & x < 30) ::
          (false forAll x >= 30 & x < 40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        //                                               patched segment
        // X-----f----)[-----t----)[-----f----)[-----t----)[-----f----)[-----t----X
        //            0           10          20          30          40
        // patch:
        // X---------------false-----------------)[---true---)[-f-](---true---](f-X
        //                                       22          32   35          50
        // result:
        // X-----f----)[-----t----)[-----f----)[------t------)[-f-](---true---](f-X
        //            0           10          20             32   35          50
        SegmentPatchedTest.TestCase(
          factoryTuple._1 + Label("B1"),
          35`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(22 `)[`, 32 `)[`, 35 `](`, 50 `](`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  32) ::
          (false forAll x >= 32 & x <= 35) ::
          (true  forAll x >  35) ::
          Nil
        ),
        // current:
        //                                               patched segment
        // X-----f----)[-----t----)[-----f----)[-----t----)[-----f----)[-----t----X
        //            0           10          20          30          40
        // patch:
        // X---------------true------------------)[---false--)[-t-](---false--](t-X
        //                                       22          32   35          50
        // result:
        // X-----f----)[-----t----)[-----f----)[-----t----)[f)[-t-](f-)[-----t----X
        //            0           10          20          30 32   35  40
        SegmentPatchedTest.TestCase(
          factoryTuple._1 + Label("B2"),
          35`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(22 `)[`, 32 `)[`, 35 `](`, 50 `](`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  32) ::
          (true  forAll x >= 32 & x <= 35) ::
          (false forAll x >  35 & x <  40) ::
          (true  forAll x >= 40) ::
          Nil
        ),
        // current:
        //                                                            patched segment
        // X-----f----)[-----t----)[-----f----)[-----t----)[-----f----)[-----t----X
        //            0           10          20          30          40
        // patch:
        // X---------------true------------------)[----------false--------)[-true--X
        //                                       22                       42
        // result:
        // X-----f----)[-----t----)[-----f----)[-----t----)[-----false----)[-true--X
        //            0           10          20          30              42
        SegmentPatchedTest.TestCase(
          factoryTuple._1 + Label("C1"),
          100`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(22 `)[`, 42 `)[`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  42) ::
          (true  forAll x >= 42) ::
          Nil
        ),
        // current:
        //                                                            patched segment
        // X-----f----)[-----t----)[-----f----)[-----t----)[-----f----)[-----t----X
        //            0           10          20          30          40
        // patch:
        // X---------------false-----------------)[----------true---------)[-false-X
        //                                       22                       42
        // result:
        // X-----f----)[-----t----)[-----f----)[-----t----)[--false---)[t-)[-false-X
        //            0           10          20          30              42
        SegmentPatchedTest.TestCase(
          factoryTuple._1 + Label("C2"),
          100`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(22 `)[`, 42 `)[`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <  42) ::
          (false forAll x >= 42) ::
          Nil
        )
      )
    }
}
