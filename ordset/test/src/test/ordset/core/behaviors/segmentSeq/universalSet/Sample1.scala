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
    with SegmentSeqPrependedTest[Int, D, Boolean]
    with SegmentSeqAppendedTest[Int, D, Boolean]
    with SegmentSeqSlicedTest[Int, D, Boolean]
    with SegmentPatchedTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override def sample: String = "1"

  override def bounds: IterableOnce[GenUpperBound] =
    ArraySeq.empty

  override def complementary: Boolean = true

  override def reference: Seq[GenIntervalRelation] =
    (true forAll x) ::
    Nil

  override def moveToBoundCases: Seq[(GenBound, GenIntervalRelation)] =
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
  
  override def prependedCases: Seq[SegmentSeqPrependedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------true------------------------X
        // prepended:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // prepended:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        // X------------------true------------------------X
        // prepended:
        // X--------true---------](--------false----------X
        //                       0
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // prepended:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X--------false--------](---------true----------X
        //                       0
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        )
      )
    }
  }

  override def prependedWithBoundCases: Seq[SegmentSeqPrependedTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // prepended:
        // X-------------------true-----------------------X
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          0 `]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // prepended:
        // X-------------------false----------------------X
        // result:
        // X----------false---------](---------true-------X
        //                           0
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        )
      )
    }
  }

  override def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------true------------------------X
        // appended:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (false forAll x) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // appended:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        // X------------------true------------------------X
        // appended:
        // X--------true---------](--------false----------X
        //                       0
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // appended:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X--------false--------](---------true----------X
        //                       0
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(0`](`), complementary = false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        )
      )
    }
  }

  override def appendedWithBoundCases: Seq[SegmentSeqAppendedTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // appended:
        // X-------------------true-----------------------X
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // appended:
        // X-------------------false----------------------X
        // result:
        // X-----------true----------](--------false-------X
        //                           0
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        )
      )
    }
  }
  
  override def slicedCases: Seq[SegmentSeqSlicedTest.TestCase[Int, D, Boolean]] =
    List(
      // current: 
      //                     bound
      //                       )
      // X------------------true------------------------X
      //
      // takenBelow:
      // X------------------true------------------------X
      // 
      // takenAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        0`)`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       ]
      // X------------------true------------------------X
      //
      // takenBelow:
      // X------------------true------------------------X
      // 
      // takenAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        0`]`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       [
      // X------------------true------------------------X
      //
      // takenBelow:
      // X------------------true------------------------X
      // 
      // takenAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        0`[`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       (
      // X------------------true------------------------X
      //
      // takenBelow:
      // X------------------true------------------------X
      // 
      // takenAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        0`(`,
        reference,
        reference
      )
    )

  override def patchedCases: Seq[SegmentPatchedTest.TestCase[Int, D, Boolean]] =
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
        SegmentPatchedTest.TestCase(
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
        SegmentPatchedTest.TestCase(
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
        SegmentPatchedTest.TestCase(
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
