package test.ordset.core.behaviors.segmentSeq.degenerateSet

import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.core.{Bound, ExtendedBound, IntervalRelation, SegmentSeq}
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
    with SegmentSeqPrependTest[Int, D, Boolean]
    with SegmentSeqAppendTest[Int, D, Boolean]
    with SegmentSeqSliceTest[Int, D, Boolean]
    with SegmentPatchTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override val sample: String = "1"
  
  override val complementary: Boolean = false

  override val reference: Seq[GenIntervalRelation] =
    (false forAll x <  0) ::
    (true  forAll x >= 0  & x <= 0) ::
    (false forAll x >  0  & x <  10) ::
    (true  forAll x >= 10 & x <  20) ::
    (false forAll x >= 20 & x <= 20) ::
    (true  forAll x >  20 & x <  30) ::
    (false forAll x >= 30 & x <= 30) ::
    (true  forAll x >  30) ::
    Nil

  override lazy val moveToBoundCases: Seq[(ExtendedBound[Int], GenIntervalRelation)] = {
    (20`[`, false forAll x >= 20 & x <= 20) ::
    (20`]`, false forAll x >= 20 & x <= 20) ::
    (20`)`, true  forAll x >= 10 & x <  20) ::
    (ExtendedBound.BelowAll, false forAll x <  0) ::
    (ExtendedBound.AboveAll, true  forAll x >  30) ::
    (30`(`, true  forAll x >  30) ::
    (40`)`, true  forAll x >  30) ::
    (-1`[`, false forAll x <  0 ) ::
    ( 0`]`, true  forAll x >= 0  & x <= 0 ) ::
    Nil
  }

  override lazy val containsCases: Seq[SegmentContainsTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentContainsTest.TestCase(
        bound = -10`[`,
        includedBounds = List(ExtendedBound.BelowAll, -10`[`, 0`)`),
        excludedBounds = List(ExtendedBound.AboveAll, 50`)`, 0`]`, 0`(`)
      ),
      SegmentContainsTest.TestCase(
        bound = 0`[`,
        includedBounds = List(0`]`, 0`[`),
        excludedBounds = List(ExtendedBound.BelowAll, ExtendedBound.AboveAll, -50`)`, 50`[`, 0`)`, 0`(`)
      ),
      SegmentContainsTest.TestCase(
        bound = 50`[`,
        includedBounds = List(ExtendedBound.AboveAll, 30`(`, 40`)`),
        excludedBounds = List(ExtendedBound.BelowAll, 0`)`, 30`]`, 30`)`)
      )
    )

  override lazy val restrictCases: Seq[SegmentRestrictBoundTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentRestrictBoundTest.TestCase(
        bound = -10`]`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, ExtendedBound.BelowAll),
          TestTuple(ExtendedBound.AboveAll, 0`)`),
          TestTuple(0`)`, 0`)`),
          TestTuple(0`[`, 0`)`),
          TestTuple(-10`)`, -10`)`)
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 0`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 0`[`),
          TestTuple(ExtendedBound.AboveAll, 0`]`),
          TestTuple(0`]`, 0`]`),
          TestTuple(0`[`, 0`[`),
          TestTuple(0`)`, 0`[`),
          TestTuple(0`(`, 0`]`)
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 50`(`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 30`(`),
          TestTuple(ExtendedBound.AboveAll, ExtendedBound.AboveAll),
          TestTuple(25`(`, 30`(`),
          TestTuple(30`(`, 30`(`),
          TestTuple(30`[`, 30`(`),
          TestTuple(30`]`, 30`(`),
          TestTuple(30`)`, 30`(`),
          TestTuple(50`)`, 50`)`)
        )
      )
    )

  override lazy val prependCases: Seq[SegmentSeqPrependTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //        true
        // X---f---)|( ..............Seq1................ X
        //          0
        //
        // other:
        //
        // X------------------false-----------------------X
        //
        // result:
        //
        //        true
        // X---f---)|( ..............Seq1................ X
        //          0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        //        true
        // X---f---)|( ..............Seq1................ X
        //          0
        //
        // other:
        //
        // X-------------------true-----------------------X
        //
        // result:
        //
        //        true
        // X----t---]( ..............Seq1................ X
        //          0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0  & x <  10) ::
          (true  forAll x >= 10 & x <  20) ::
          (false forAll x >= 20 & x <= 20) ::
          (true  forAll x >  20 & x <  30) ::
          (false forAll x >= 30 & x <= 30) ::
          (true  forAll x >  30) ::
          Nil
        ),
        // current:
        //        true
        // X---f---)|( ..............Seq1................ X
        //          0
        //
        // other:
        //
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        //
        // X-f-](-t-]( ..............Seq1................ X
        //     -5   0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5 & x <= 0) ::
          (false forAll x >   0 & x <  10) ::
          (true  forAll x >= 10 & x <  20) ::
          (false forAll x >= 20 & x <= 20) ::
          (true  forAll x >  20 & x <  30) ::
          (false forAll x >= 30 & x <= 30) ::
          (true  forAll x >  30) ::
          Nil
        ),
        // current:
        //        true
        // X---f---)|( ..............Seq1................ X
        //          0
        //
        // other:
        //        true
        // X---f---)|( ..............Seq1................ X
        //          0
        //
        // result:
        //        true
        // X---f---)|( ..............Seq1................ X
        //          0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(bounds, complementary),
          reference
        )
      )
    }
  }

  override lazy val prependBelowBoundCases: Seq[SegmentSeqPrependTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //        bound
        //          ]
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        // result:
        //
        // X---f---)|(---t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          0 `]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0 `)[`, 10 `)[`, 35 `)[`), complementary = false),
          reference
        ),
        // current:
        //                           bound
        //                            )
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        // result:
        //                                     false
        // X---f---)[----t---)[--------](---t---)|(---t---X
        //          0        10        20        30
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A2"),
          20 `)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0 `)[`, 10 `)[`, 35 `)[`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <= 20) ::
          (true  forAll x >  20 & x <  30) ::
          (false forAll x >= 30 & x <= 30) ::
          (true  forAll x >  30) ::
          Nil
        ),
        // current:
        //                            bound
        //                              (
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        // result:
        //                                     false
        // X---f---)[----t---)[--------](---t---)|(---t---X
        //          0        10        20        30
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A3"),
          20 `(`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0 `)[`, 10 `)[`, 35 `)[`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <= 20) ::
          (true  forAll x >  20 & x <  30) ::
          (false forAll x >= 30 & x <= 30) ::
          (true  forAll x >  30) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        //                                     false
        // X---f---)[----t---)[--------](---t---)|(---t---X
        //          0        10        20        30
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A4"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          reference
        ),
        // current:
        //                                            bound
        //                                                X
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        //
        // X-f-](----------------true---------------------X
        //     -5
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A5"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5) ::
          Nil
        )
      )
    }
  }

  override lazy val appendCases: Seq[SegmentSeqAppendTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:           false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        // other:
        // X------------------false-----------------------X
        //
        // result:            false
        // X ......Seq1....... )|(--true--)[-----false----X
        //                     20         30
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <= 0) ::
          (false forAll x >  0  & x <  10) ::
          (true  forAll x >= 10 & x <  20) ::
          (false forAll x >= 20 & x <= 20) ::
          (true  forAll x >  20 & x <  30) ::
          (false forAll x >= 30 ) ::
          Nil
        ),
        // current:           false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        // other:
        // X-------------------true-----------------------X
        //
        // result:            false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:           false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        // other:
        // X-------------------true--------------](-false-X
        //                                       40
        // result:            false      false
        // X ......Seq1....... )|(--true--)|(-tr-](-false-X
        //                     20         30
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(40 `](`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <= 0) ::
          (false forAll x >  0  & x <  10) ::
          (true  forAll x >= 10 & x <  20) ::
          (false forAll x >= 20 & x <= 20) ::
          (true  forAll x >  20 & x <  30) ::
          (false forAll x >= 30 & x <= 30) ::
          (true  forAll x >  30 & x <= 40) ::
          (false forAll x >  40) ::
          Nil
        ),
        // current:           false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        //
        // other:          false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        //
        // result:            false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(bounds, complementary),
          reference
        )
      )
    }
  }

  override lazy val appendAboveBoundCases: Seq[SegmentSeqAppendTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //       bound
        //         )
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        // result:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          0`)`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`)[`, 10`)[`, 35`)[`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  35) ::
          (true  forAll x >= 35) ::
          Nil
        ),
        // current:
        //          bound
        //           (
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        // result:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A2"),
          0`(`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`)[`, 10`)[`, 35`)[`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  35) ::
          (true  forAll x >= 35) ::
          Nil
        ),
        // current:
        //                           bound
        //                             ]
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        // result:
        //        true
        // X---f---)|(---f---)[---t---)[----false----)[-t-X
        //          0        10       20             35
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A3"),
          20`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`)[`, 10`)[`, 35`)[`), complementary = false),
          (false forAll x < 0) ::
          (true  forAll x >= 0  & x <= 0) ::
          (false forAll x >  0  & x <  10) ::
          (true  forAll x >= 10 & x <  20) ::
          (false forAll x >= 20 & x <  35) ::
          (true  forAll x >= 35) ::
          Nil
        ),
        // current:
        //                           bound
        //                             [
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X---f---)[----t---)[----------false-------)[-t-X
        //          0        10                      35
        //
        // result:
        //        true
        // X---f---)|(---f---)[---t---)[----false----)[-t-X
        //          0        10       20             35
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A4"),
          20`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`)[`, 10`)[`, 35`)[`), complementary = false),
          (false forAll x < 0) ::
          (true  forAll x >= 0  & x <= 0) ::
          (false forAll x >  0  & x <  10) ::
          (true  forAll x >= 10 & x <  20) ::
          (false forAll x >= 20 & x <  35) ::
          (true  forAll x >= 35) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        //
        // X-f-](----------------true---------------------X
        //     -5
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A5"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5) ::
          Nil
        ),
        // current:
        //                                            bound
        //                                                X
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // other:
        //
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A6"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          reference
        )
      )
    }
  }

  override lazy val sliceCases: Seq[SegmentSeqSliceTest.TestCase[Int, D, Boolean]] =
    List(
      // current:      
      //                      bound
      //                        ]
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      // takeBelow:
      //
      //        true        
      // X---f---)|(---f---)[----------true-------------X
      //          0        10
      //
      // takeAbove:
      //                           false     false 
      // X-----------true-----------)|(---t---)|(---t---X
      //                             20        30
      //
      SegmentSeqSliceTest.TestCase(
        15`]`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <= 0) ::
        (false forAll x >  0  & x <  10) ::
        (true  forAll x >= 10) ::
        Nil,
        (true  forAll x <  20) ::
        (false forAll x >= 20 & x <= 20) ::
        (true  forAll x >  20 & x <  30) ::
        (false forAll x >= 30 & x <= 30) ::
        (true  forAll x >  30) ::
        Nil
      ),
      // current:      
      //                          bound
      //                            )
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      // takeBelow:
      //
      //        true        
      // X---f---)|(---f---)[----------true-------------X
      //          0        10
      //
      // takeAbove:
      //                           false     false 
      // X-----------true-----------)|(---t---)|(---t---X
      //                             20        30
      //
      SegmentSeqSliceTest.TestCase(
        20`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <= 0) ::
        (false forAll x >  0  & x <  10) ::
        (true  forAll x >= 10) ::
        Nil,
        (true  forAll x <  20) ::
        (false forAll x >= 20 & x <= 20) ::
        (true  forAll x >  20 & x <  30) ::
        (false forAll x >= 30 & x <= 30) ::
        (true  forAll x >  30) ::
        Nil
      ),
      // current:      
      //                           bound
      //                             ]
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      // takeBelow:
      //
      //        true        
      // X---f---)|(---f---)[---t---)[------false-------X
      //          0        10       20
      //
      // takeAbove:
      //                                     false 
      // X-----------false-----------](---t---)|(---t---X
      //                             20        30
      //
      SegmentSeqSliceTest.TestCase(
        20`]`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <= 0) ::
        (false forAll x >  0  & x <  10) ::
        (true  forAll x >= 10 & x <  20) ::
        (false forAll x >= 20) ::
        Nil,
        (false forAll x <= 20) ::
        (true  forAll x >  20 & x <  30) ::
        (false forAll x >= 30 & x <= 30) ::
        (true  forAll x >  30) ::
        Nil
      ),
      // current:      
      //                           bound
      //                             [
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      // takeBelow:
      //
      //        true        
      // X---f---)|(---f---)[---t---)[------false-------X
      //          0        10       20
      //
      // takeAbove:
      //                                     false 
      // X-----------false-----------](---t---)|(---t---X
      //                             20        30
      //
      SegmentSeqSliceTest.TestCase(
        20`[`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <= 0) ::
        (false forAll x >  0  & x <  10) ::
        (true  forAll x >= 10 & x <  20) ::
        (false forAll x >= 20) ::
        Nil,
        (false forAll x <= 20) ::
        (true  forAll x >  20 & x <  30) ::
        (false forAll x >= 30 & x <= 30) ::
        (true  forAll x >  30) ::
        Nil
      ),
      // current:      
      //                            bound
      //                              (
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      // takeBelow:
      //
      //        true               false
      // X---f---)|(---f---)[---t---)|(------true-------X
      //          0        10       20
      //
      // takeAbove:
      //                                     false 
      // X------------true--------------------)|(---t---X
      //                                       30
      //
      SegmentSeqSliceTest.TestCase(
        20`(`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <= 0) ::
        (false forAll x >  0  & x <  10) ::
        (true  forAll x >= 10 & x <  20) ::
        (false forAll x >= 20 & x <= 20) ::
        (true  forAll x >  20) ::
        Nil,
        (true  forAll x <  30) ::
        (false forAll x >= 30 & x <= 30) ::
        (true  forAll x >  30) ::
        Nil
      ),
      // current:
      //
      // bound
      // X
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      // takeBelow:
      //
      // X-------------------false----------------------X
      //
      // takeAbove:
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.BelowAll,
        (false forAll x ) ::
        Nil,
        reference
      ),
      // current:
      //                                             bound
      //                                                X
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      // takeBelow:
      //
      //        true               false     false
      // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
      //          0        10        20        30
      //
      // takeAbove:
      //
      // X-------------------true-----------------------X
      //
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.AboveAll,
        reference,
        (true forAll x ) ::
        Nil
      )
    )

  override lazy val patchCases: Seq[SegmentPatchTest.TestCase[Int, D, Boolean]] =
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //    patched segment
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // patch:
        // X---t---)[-------------false-------------------X
        //          0
        //
        // result:
        //                           false     false
        // X------false------)[---t---)|(---t---)|(---t---X
        //                   10        20        30
        //
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("A1"),
          0 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0 `)[`), complementary = true),
          (false forAll x <  10) ::
          (true  forAll x >= 10 & x <  20) ::
          (false forAll x >= 20 & x <= 20) ::
          (true  forAll x >  20 & x <  30) ::
          (false forAll x >= 30 & x <= 30) ::
          (true  forAll x >  30) ::
          Nil
        ),
        // current:
        //    patched segment
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // patch:
        // X---f---)[--------------true-------------------X
        //          0
        //
        // result:
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("A2"),
          0 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0 `)[`), complementary = false),
          reference
        ),
        // current:
        //                           patched segment
        //        true               false     false
        // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
        //          0        10        20        30
        //
        // patch:
        // X----------------true------------)[----false---X
        //                                  25
        //
        // result:
        //        true               false
        // X---f---)|(---f---)[---t---)|(-t-)[-f-](---t---X
        //          0        10        20   25   30
        //
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("B1"),
          25 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25 `)[`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <= 0) ::
          (false forAll x >  0  & x <  10) ::
          (true  forAll x >= 10 & x <  20) ::
          (false forAll x >= 20 & x <= 20) ::
          (true  forAll x >  20 & x <  25) ::
          (false forAll x >= 25 & x <= 30) ::
          (true  forAll x >  30) ::
          Nil
        )
      )
    }
}
