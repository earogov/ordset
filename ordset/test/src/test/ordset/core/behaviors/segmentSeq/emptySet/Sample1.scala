package test.ordset.core.behaviors.segmentSeq.emptySet

import ordset.core.Bound
import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.util.label.Label
import test.ordset.core.behaviors.segmentSeq.{SegmentMoveToBoundTest, SegmentSeqAppendedTest, SegmentSeqFactories, SegmentSeqSlicedTest}
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample1[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
    with SegmentSeqAppendedTest[Int, D, Boolean]
    with SegmentSeqSlicedTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean] =>

  override def sample: String = "1"

  override def bounds: IterableOnce[Bound.Upper[Int]] =
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

  override def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[Int, D, Boolean]] = {
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
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("A1"),
          0`]`,
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = false),
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
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("B1"),
          0`]`,
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = true),
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
}
