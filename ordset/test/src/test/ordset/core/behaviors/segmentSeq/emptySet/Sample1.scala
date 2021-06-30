package test.ordset.core.behaviors.segmentSeq.emptySet

import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.core.{Bound, SegmentSeq}
import ordset.util.label.Label
import test.ordset.core.behaviors.segmentSeq._
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample1[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
    with SegmentSeqPrependedTest[Int, D, Boolean]
    with SegmentSeqAppendedTest[Int, D, Boolean]
    with SegmentSeqSlicedTest[Int, D, Boolean]
    with SegmentPatchedTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override def sample: String = "1"

  override def bounds: IterableOnce[GenUpperBound] =
    ArraySeq.empty

  override def complementary: Boolean = false

  override def reference: Seq[GenIntervalRelation] =
    (false forAll x) ::
    Nil

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    ( 10`)`, false forAll x) ::
    ( 15`[`, false forAll x) ::
    (-10`)`, false forAll x) ::
    (-15`[`, false forAll x) ::
    Nil

  override def prependedCases: Seq[SegmentSeqPrependedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------false-----------------------X
        // prepended:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        // X------------------false-----------------------X
        // prepended:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentSeqPrependedTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          (true forAll x) ::
          Nil
        ),
        // current:
        // X------------------false-----------------------X
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
        // X------------------false-----------------------X
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
        // X------------------false-----------------------X
        // prepended:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          0 `]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        //                         bound
        //                           ]
        // X------------------false-----------------------X
        // prepended:
        // X------------------true------------------------X
        // result:
        // X-----------true----------](-------false-------X
        //                           0
        SegmentSeqPrependedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        )
      )
    }
  }

  override def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------false-----------------------X
        // appended:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        // X------------------false-----------------------X
        // appended:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          (true forAll x) ::
          Nil
        ),
        // current:
        // X------------------false-----------------------X
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
        // X------------------false-----------------------X
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
        // X------------------false-----------------------X
        // appended:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        //                         bound
        //                           ]
        // X------------------false-----------------------X
        // appended:
        // X------------------true------------------------X
        // result:
        // X----------false----------](--------true-------X
        //                           0
        SegmentSeqAppendedTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          0`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
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
      // X-----------------false------------------------X
      //
      // takenBelow:
      // X-----------------false------------------------X
      // 
      // takenAbove:
      // X-----------------false------------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        0`)`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       ]
      // X-----------------false------------------------X
      //
      // takenBelow:
      // X-----------------false------------------------X
      // 
      // takenAbove:
      // X-----------------false------------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        0`]`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       [
      // X-----------------false------------------------X
      //
      // takenBelow:
      // X-----------------false------------------------X
      // 
      // takenAbove:
      // X-----------------false------------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        0`[`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       (
      // X-----------------false------------------------X
      //
      // takenBelow:
      // X-----------------false------------------------X
      // 
      // takenAbove:
      // X-----------------false------------------------X
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
        // X------------------false-----------------------X
        // patch:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentPatchedTest.TestCase(
          factoryTuple._1 + Label("A1"),
          0 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        //                patched segment
        // X------------------false-----------------------X
        // patch:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentPatchedTest.TestCase(
          factoryTuple._1 + Label("A2"),
          0 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq.empty, complementary = true),
          (true forAll x) ::
          Nil
        ),
        // current:
        //                patched segment
        // X------------------false-----------------------X
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
