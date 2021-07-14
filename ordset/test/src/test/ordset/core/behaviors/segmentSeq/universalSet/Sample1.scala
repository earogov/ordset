package test.ordset.core.behaviors.segmentSeq.universalSet

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
    with SegmentSeqPrependTest[Int, D, Boolean]
    with SegmentSeqAppendTest[Int, D, Boolean]
    with SegmentSeqSliceTest[Int, D, Boolean]
    with SegmentPatchTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override def sample: String = "1"

  override def bounds: IterableOnce[Bound.Upper[Int]] =
    ArraySeq.empty

  override def complementary: Boolean = true

  override def reference: Seq[GenIntervalRelation] =
    (true forAll x) ::
    Nil

  override def moveToBoundCases: Seq[(ExtendedBound[Int], GenIntervalRelation)] =
    (ExtendedBound.BelowAll, true forAll x) ::
    (ExtendedBound.AboveAll, true forAll x) ::
    ( 10`)`, true forAll x) ::
    ( 15`[`, true forAll x) ::
    (-10`)`, true forAll x) ::
    (-15`[`, true forAll x) ::
    Nil

  override def containsCases: Seq[SegmentContainsTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentContainsTest.TestCase(
        bound = 0`[`,
        includedBounds = List(ExtendedBound.BelowAll, ExtendedBound.AboveAll, -10`[`, 10`)`),
        excludedBounds = List()
      )
    )

  override def restrictCases: Seq[SegmentRestrictBoundTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentRestrictBoundTest.TestCase(
        bound = 0`]`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, ExtendedBound.BelowAll),
          TestTuple(ExtendedBound.AboveAll, ExtendedBound.AboveAll),
          TestTuple(0`)`, 0`)`),
          TestTuple(0`[`, 0`[`),
          TestTuple(-10`(`, -10`(`),
          TestTuple(-10`]`, -10`]`)
        )
      )
    )
  
  override def prependCases: Seq[SegmentSeqPrependTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------true------------------------X
        // other:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X--------true---------](--------false----------X
        //                       0
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X--------false--------](---------true----------X
        //                       0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        )
      )
    }
  }

  override def prependBelowBoundCases: Seq[SegmentSeqPrependTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // other:
        // X-------------------true-----------------------X
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          0 `]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // other:
        // X-------------------false----------------------X
        // result:
        // X----------false---------](---------true-------X
        //                           0
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        // X------------------true------------------------X
        //
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X------------------true------------------------X
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("C1"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          reference
        ),
        // current:
        //                                            bound
        //                                                X
        // X------------------true------------------------X
        //
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X-f-](----------------true---------------------X
        //     -5
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("C2"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5) ::
          Nil
        )
      )
    }
  }

  override def appendCases: Seq[SegmentSeqAppendTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------true------------------------X
        // other:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X--------true---------](--------false----------X
        //                       0
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X--------false--------](---------true----------X
        //                       0
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        )
      )
    }
  }

  override def appendAboveBoundCases: Seq[SegmentSeqAppendTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // other:
        // X-------------------true-----------------------X
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // other:
        // X-------------------false----------------------X
        // result:
        // X-----------true----------](--------false-------X
        //                           0
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        // X------------------true------------------------X
        //
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X-f-](----------------true---------------------X
        //     -5
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("C1"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5) ::
          Nil
        ),
        // current:
        //
        //                                            bound
        //                                                X
        // X------------------true------------------------X
        //
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X------------------false-----------------------X
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("C2"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          reference
        )
      )
    }
  }
  
  override def sliceCases: Seq[SegmentSeqSliceTest.TestCase[Int, D, Boolean]] =
    List(
      // current: 
      //                     bound
      //                       )
      // X------------------true------------------------X
      //
      // takeBelow:
      // X------------------true------------------------X
      // 
      // takeAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`)`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       ]
      // X------------------true------------------------X
      //
      // takeBelow:
      // X------------------true------------------------X
      // 
      // takeAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`]`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       [
      // X------------------true------------------------X
      //
      // takeBelow:
      // X------------------true------------------------X
      // 
      // takeAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`[`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       (
      // X------------------true------------------------X
      //
      // takeBelow:
      // X------------------true------------------------X
      // 
      // takeAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`(`,
        reference,
        reference
      ),
      // current:
      //
      // bound
      // X
      // X-----------------true-------------------------X
      //
      // takeBelow:
      // X-----------------true-------------------------X
      //
      // takeAbove:
      // X-----------------true-------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.BelowAll,
        reference,
        reference
      ),
      // current:
      //                                            bound
      //                                                X
      // X-----------------true-------------------------X
      //
      // takeBelow:
      // X-----------------true-------------------------X
      //
      // takeAbove:
      // X-----------------true-------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.AboveAll,
        reference,
        reference
      )
    )

  override def patchCases: Seq[SegmentPatchTest.TestCase[Int, D, Boolean]] =
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //                patched segment
        // X------------------true------------------------X
        // patch:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("A1"),
          0 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x) ::
          Nil
        ),
        // current:
        //                patched segment
        // X------------------true------------------------X
        // patch:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("A2"),
          0 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        //                patched segment
        // X-------------------true-----------------------X
        // patch:
        // X---------false------)[----------true----------X
        //                      0
        // result:
        // X---------false------)[----------true----------X
        //                      0
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("B1"),
          0 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0 `)[`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0) ::
          Nil
        )
      )
    }
}
