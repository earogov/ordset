package test.ordset.core.behaviors.segmentSeq.singleBoundedSet

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
    ArraySeq(0 `](`)

  override def complementary: Boolean = true

  override def reference: Seq[GenIntervalRelation] =
    (true  forAll x <= 0) ::
    (false forAll x >  0) ::
    Nil

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    ( 10`)`, false forAll x >  0) ::
    ( 15`[`, false forAll x >  0) ::
    (-10`)`, true  forAll x <= 0) ::
    (-15`[`, true  forAll x <= 0) ::
    (  0`(`, false forAll x >  0) ::
    (  0`]`, true  forAll x <= 0) ::
    Nil

  override def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //          bound
        //             )
        // X--------true---------](--------false----------X
        //                       0
        // appended:
        // X------------------false-----------------------X
        //
        // result:
        // X----true---)[------------false----------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("A1"),
          -10`)`,
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = false),
          (true forAll x < -10) ::
          (false forAll x >= -10)  ::
          Nil
        ),
        // current:
        //                      bound
        //                        (
        // X--------true---------](--------false----------X
        //                       0
        // appended:
        // X------------------false-----------------------X
        //
        // result:
        // X--------true---------](--------false----------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("A2"),
          0`(`,
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = false),
          reference
        ),
        // current:
        //             bound
        //               (
        // X--------true---------](--------false----------X
        //                       0
        // appended:
        // X-------------------true-----------------------X
        //
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("B1"),
          -10`(`,
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = true),
          (true forAll x) ::
          Nil
        ),
        // current:
        //                     bound
        //                       ]
        // X--------true---------](--------false----------X
        //                       0
        // appended:
        // X-------------------true-----------------------X
        //
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("B2"),
          0`]`,
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = true),
          (true forAll x) ::
          Nil
        ),
        // current:
        //                          bound
        //                            ]
        // X--------true---------](--------false----------X
        //                       0
        // appended:
        // X---true---](-------false------)[-----true-----X
        //            -10                 10
        // result:
        // X--------true---------](-false-)[-----true-----X
        //                       0        10
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("C1"),
          5`]`,
          factoryTuple._2.buildUnsafe(ArraySeq(-10`](`, 10`)[`), complementary = true),
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
        // appended:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("D1"),
          0`]`,
          factoryTuple._2.buildUnsafe(ArraySeq(0`](`), complementary = false),
          (true forAll x) ::
          Nil
        ),
        // current:
        //                      bound
        //                        (
        // X--------true---------](--------false----------X
        //                       0
        // appended:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("D2"),
          0`(`,
          factoryTuple._2.buildUnsafe(ArraySeq(0`](`), complementary = false),
          (true forAll x) ::
          Nil
        )
      )
    }
  }

  override def slicedCases: Seq[SegmentSeqSlicedTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentSeqSlicedTest.TestCase(
        // current:
        //         bound
        //           (
        // X--------true---------](--------false----------X
        //                       0
        // takenBelow:
        // X-------------------true-----------------------X
        //
        // takenAbove:
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
      // takenBelow:
      // X-------------------true-----------------------X
      //
      // takenAbove:
      // X--------true---------](--------false----------X
      //                       0
      SegmentSeqSlicedTest.TestCase(
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
      // takenBelow:
      // X-------------------true-----------------------X
      //
      // takenAbove:
      // X--------true---------](--------false----------X
      //                       0
      SegmentSeqSlicedTest.TestCase(
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
      // takenBelow:
      // X-------------------true-----------------------X
      //
      // takenAbove:
      // X--------true---------](--------false----------X
      //                       0
      SegmentSeqSlicedTest.TestCase(
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
      // takenBelow:
      // X--------true---------](--------false----------X
      //                       0
      // takenAbove:
      // X-------------------false----------------------X
      //
      SegmentSeqSlicedTest.TestCase(
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
      // takenBelow:
      // X--------true---------](--------false----------X
      //                       0
      // takenAbove:
      // X-------------------false----------------------X
      //
      SegmentSeqSlicedTest.TestCase(
        10`)`,
        reference,
        (false forAll x) ::
        Nil
      )
    )
}
