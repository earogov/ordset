package test.ordset.core.behaviors.segmentSeq.universalSet

import ordset.core.Bound
import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.util.label.Label
import test.ordset.core.behaviors.segmentSeq.{SegmentMoveToBoundTest, SegmentSeqAppendedV0Test, SegmentSeqFactories, SegmentSeqSlicedTest}
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample1[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
    with SegmentSeqAppendedV0Test[Int, D, Boolean]
    with SegmentSeqSlicedTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean] =>

  override def sample: String = "1"

  override def bounds: IterableOnce[Bound.Upper[Int]] =
    ArraySeq.empty

  override def complementary: Boolean = true

  override def reference: Seq[GenIntervalRelation] =
    (true forAll x) ::
    Nil

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    ( 10`)`, true forAll x) ::
    ( 15`[`, true forAll x) ::
    (-10`)`, true forAll x) ::
    (-15`[`, true forAll x) ::
    Nil

  override def appendedV0Cases: Seq[SegmentSeqAppendedV0Test.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------true------------------------X
        // appended:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = false),
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
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = true),
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
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.buildUnsafe(ArraySeq(0`](`), complementary = true),
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
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.buildUnsafe(ArraySeq(0`](`), complementary = false),
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
}
