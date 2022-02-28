package ordset.test.core.behaviors.segmentSeq.set.singleBoundedSet

import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.*
import ordset.test.Label.*
import ordset.test.core.behaviors.TestTuple
import ordset.test.core.behaviors.segmentSeq._
import ordset.test.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample1[D[X] <: Domain[X]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
    with SegmentContainsTest[Int, D, Boolean]
    with SegmentRestrictBoundTest[Int, D, Boolean]
    with SegmentSeqPrependTest[Int, D, Boolean]
    with SegmentSeqAppendTest[Int, D, Boolean]
    with SegmentSeqSliceTest[Int, D, Boolean]
    with SegmentPatchTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override val sample: String = "1"

  override val complementary: Boolean = true

  override val reference: Seq[GenIntervalRelation] =
    (true  forAll x <= 0) ::
    (false forAll x >  0) ::
    Nil

  override lazy val moveToBoundCases: Seq[(ExtendedBound[Int], GenIntervalRelation)] =
    (ExtendedBound.BelowAll, true  forAll x <= 0) ::
    (ExtendedBound.AboveAll, false forAll x >  0) ::
    ( 10`)`, false forAll x >  0) ::
    ( 15`[`, false forAll x >  0) ::
    (-10`)`, true  forAll x <= 0) ::
    (-15`[`, true  forAll x <= 0) ::
    (  0`(`, false forAll x >  0) ::
    (  0`]`, true  forAll x <= 0) ::
    Nil

  override lazy val containsCases: Seq[SegmentContainsTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentContainsTest.TestCase(
        bound = 0`]`,
        includedBounds = List(ExtendedBound.BelowAll, -10`[`, 0`]`),
        excludedBounds = List(ExtendedBound.AboveAll, 10`(`, 0`(`)
      ),
      SegmentContainsTest.TestCase(
        bound = 0`(`,
        includedBounds = List(ExtendedBound.AboveAll, 10`(`, 0`(`),
        excludedBounds = List(ExtendedBound.BelowAll, -10`[`, 0`]`)
      )
    )

  override lazy val restrictCases: Seq[SegmentRestrictBoundTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentRestrictBoundTest.TestCase(
        bound = -10`]`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, ExtendedBound.BelowAll),
          TestTuple(ExtendedBound.AboveAll, 0`]`),
          TestTuple(-10`]`, -10`]`),
          TestTuple(0`]`, 0`]`),
          TestTuple(0`[`, 0`[`),
          TestTuple(0`(`, 0`]`),
          TestTuple(0`)`, 0`)`),
          TestTuple(10`)`, 0`]`)
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 10`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 0`(`),
          TestTuple(ExtendedBound.AboveAll, ExtendedBound.AboveAll),
          TestTuple(-10`]`, 0`(`),
          TestTuple(0`(`, 0`(`),
          TestTuple(0`]`, 0`(`),
          TestTuple(0`[`, 0`(`),
          TestTuple(0`)`, 0`(`),
          TestTuple(10`(`, 10`(`),
        )
      )
    )

  override lazy val prependCases: Seq[SegmentSeqPrependTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + label("A"),
          factoryTuple._2.buildUniform(false),
          (false forAll x) ::
          Nil
        ),
        // current:
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-------------------true-----------------------X
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + label("B"),
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        // X--------true---------](--------false----------X
        //                       0
        // other:                     true
        // X----true---](-------false-----)|(----false----X
        //            -10                 10
        // result:
        // X----true---](-------------false---------------X
        //            -10
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-10 `](`, 10 `)[`, 10 `](`), complementary = true),
          (true  forAll x <= -10) ::
          (false forAll x >  -10) ::
          Nil
        ),
        // current:
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + label("D"),
          factoryTuple._2.unsafeBuildAsc(bounds, !complementary),
          (false forAll x) ::
          Nil
        )
      )
    }
  }

  override lazy val prependBelowBoundCases: Seq[SegmentSeqPrependTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //          bound
        //             )
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X------------------false-----------------------X
        //
        // result:
        // X---false---)[--true--](--------false----------X
        //            -10         0
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("A1"),
          -10 `)`,
          factoryTuple._2.buildUniform(false),
          (false forAll x < -10) ::
          (true  forAll x >= -10 & x <= 0) ::
          (false forAll x > 0) ::
          Nil
        ),
        // current:
        //                      bound
        //                        (
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X------------------false-----------------------X
        //
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("A2"),
          0`(`,
          factoryTuple._2.buildUniform(false),
          (false forAll x) ::
          Nil
        ),
        // current:
        //             bound
        //               (
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-------------------true-----------------------X
        //
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("B1"),
          -10`(`,
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        //                     bound
        //                       ]
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-------------------true-----------------------X
        //
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("B2"),
          0`]`,
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        //                          bound
        //                            ]
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X--false---](--------true------)[----false-----X
        //            -10                 10
        // result:
        // X--false---](-----true-----](------false-------X
        //            -10             5
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("C1"),
          5`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-10`](`, 10`)[`), complementary = false),
          (false forAll x <= -10) ::
          (true  forAll x > -10 & x <= 5) ::
          (false forAll x > 5) ::
          Nil
        ),
        // current:
        //                     bound
        //                       ]
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("D1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = false),
          (false forAll x) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("E1"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          reference
        ),
        // current:
        //                                            bound
        //                                                X
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X-f-](----------------true---------------------X
        //     -5
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("E2"),
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
        // current:
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X------------------false-----------------------X
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + label("A"),
          factoryTuple._2.buildUniform(false),
          reference
        ),
        // current:
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-------------------true-----------------------X
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + label("B"),
          factoryTuple._2.buildUniform(true),
          (true  forAll x) ::
          Nil
        ),
        // current:
        // X--------true---------](--------false----------X
        //                       0
        // other:                     true
        // X----true---](-------false-----)|(----false----X
        //            -10                 10
        //
        // result:                       true
        // X--------true---------](-false-)|(----false----X
        //                       0        10
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-10 `](`, 10 `)[`, 10 `](`), complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0  & x <  10) ::
          (true  forAll x >= 10 & x <= 10) ::
          (false forAll x >  10) ::
          Nil
        ),
        // current:
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + label("D"),
          factoryTuple._2.unsafeBuildAsc(bounds, !complementary),
          (true  forAll x) ::
          Nil
        )
      )
    }
  }

  override lazy val appendAboveBoundCases: Seq[SegmentSeqAppendTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //          bound
        //             )
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X------------------false-----------------------X
        //
        // result:
        // X----true---)[------------false----------------X
        //             -10
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("A1"),
          -10`)`,
          factoryTuple._2.buildUniform(false),
          (true forAll x < -10) ::
          (false forAll x >= -10)  ::
          Nil
        ),
        // current:
        //                      bound
        //                        (
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X------------------false-----------------------X
        //
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("A2"),
          0`(`,
          factoryTuple._2.buildUniform(false),
          reference
        ),
        // current:
        //             bound
        //               (
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-------------------true-----------------------X
        //
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("B1"),
          -10`(`,
          factoryTuple._2.buildUniform(true),
          (true forAll x) ::
          Nil
        ),
        // current:
        //                     bound
        //                       ]
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-------------------true-----------------------X
        //
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("B2"),
          0`]`,
          factoryTuple._2.buildUniform(true),
          (true forAll x) ::
          Nil
        ),
        // current:
        //                          bound
        //                            ]
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X---true---](-------false------)[-----true-----X
        //            -10                 10
        // result:
        // X--------true---------](-false-)[-----true-----X
        //                       0        10
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("C1"),
          5`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-10`](`, 10`)[`), complementary = true),
          (true forAll x <= 0) ::
          (false forAll x > 0 & x < 10) ::
          (true forAll x >= 10) ::
          Nil
        ),
        // current:
        //                     bound
        //                       ]
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("D1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = false),
          (true forAll x) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X-f-](----------------true---------------------X
        //     -5
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("E1"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5) ::
          Nil
        ),
        // current:
        //                                            bound
        //                                                X
        // X--------true---------](--------false----------X
        //                       0
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("E2"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          reference
        )
      )
    }
  }

  override lazy val sliceCases: Seq[SegmentSeqSliceTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentSeqSliceTest.TestCase(
        // current:
        //         bound
        //           (
        // X--------true---------](--------false----------X
        //                       0
        // takeBelow:
        // X-------------------true-----------------------X
        //
        // takeAbove:
        // X--------true---------](--------false----------X
        //                       0
        -10`(`,
        (true  forAll x) ::
        Nil,
        reference
      ),
      // current:
      //                    bound
      //                      )
      // X--------true---------](--------false----------X
      //                       0
      // takeBelow:
      // X-------------------true-----------------------X
      //
      // takeAbove:
      // X--------true---------](--------false----------X
      //                       0
      SegmentSeqSliceTest.TestCase(
        0`)`,
        (true  forAll x) ::
        Nil,
        reference
      ),
      // current:
      //                     bound
      //                       ]
      // X--------true---------](--------false----------X
      //                       0
      // takeBelow:
      // X-------------------true-----------------------X
      //
      // takeAbove:
      // X--------true---------](--------false----------X
      //                       0
      SegmentSeqSliceTest.TestCase(
        0`]`,
        (true  forAll x) ::
        Nil,
        reference
      ),
      // current:
      //                     bound
      //                       [
      // X--------true---------](--------false----------X
      //                       0
      // takeBelow:
      // X-------------------true-----------------------X
      //
      // takeAbove:
      // X--------true---------](--------false----------X
      //                       0
      SegmentSeqSliceTest.TestCase(
        0`[`,
        (true  forAll x) ::
        Nil,
        reference
      ),
      // current:
      //                      bound
      //                        (
      // X--------true---------](--------false----------X
      //                       0
      // takeBelow:
      // X--------true---------](--------false----------X
      //                       0
      // takeAbove:
      // X-------------------false----------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`(`,
        reference,
        (false forAll x) ::
        Nil
      ),
      // current:
      //                              bound
      //                                )
      // X--------true---------](--------false----------X
      //                       0
      // takeBelow:
      // X--------true---------](--------false----------X
      //                       0
      // takeAbove:
      // X-------------------false----------------------X
      //
      SegmentSeqSliceTest.TestCase(
        10`)`,
        reference,
        (false forAll x) ::
        Nil
      ),
      // current:
      //
      // bound
      // X
      // X--------true---------](--------false----------X
      //                       0
      // takeBelow:
      // X-----------------true------------------------X
      //
      // takeAbove:
      // X--------true---------](--------false----------X
      //                       0
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.BelowAll,
        (true forAll x) :: Nil,
        reference
      ),
      // current:
      //                                            bound
      //                                                X
      // X--------true---------](--------false----------X
      //                       0
      // takeBelow:
      // X--------true---------](--------false----------X
      //                       0
      // takeAbove:
      // X-----------------false------------------------X
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.AboveAll,
        reference,
        (false forAll x) :: Nil
      )
    )

  override lazy val patchCases: Seq[SegmentPatchTest.TestCase[Int, D, Boolean]] =
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //     patched segment
        // X--------true---------](--------false----------X
        //                       0
        // patch:
        // X-------------------true-----------------------X
        //
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentPatchTest.TestCase(
          factoryTuple._1 + label("A1"),
          0 `[`,
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        //     patched segment
        // X--------true---------](--------false----------X
        //                       0
        // patch:
        // X-------------------false----------------------X
        //
        // result:
        // X-------------------false----------------------X
        //
        SegmentPatchTest.TestCase(
          factoryTuple._1 + label("A2"),
          0 `[`,
          factoryTuple._2.buildUniform(false),
          (false forAll x) ::
          Nil
        ),
        // current:
        //     patched segment
        // X--------true---------](--------false----------X
        //                       0
        // patch:
        // X--f--)[--t--](---f---](-----------t-----------X
        //       -20    -10      0
        // result:
        // X--f--)[--t--](-------------false--------------X
        //       -20    -10
        SegmentPatchTest.TestCase(
          factoryTuple._1 + label("B1"),
          0 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-20 `)[`, -10 `](`, 0 `](`), complementary = false),
          (false forAll x <  -20) ::
          (true  forAll x >= -20 & x <= -10) ::
          (false forAll x >  -10) ::
          Nil
        )
      )
    }
}
