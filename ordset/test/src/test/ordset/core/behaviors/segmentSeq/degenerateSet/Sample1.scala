package test.ordset.core.behaviors.segmentSeq.degenerateSet

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
    ArraySeq(0 `)[`, 0 `](`, 10 `)[`, 20 `)[`, 20 `](`, 30 `)[`, 30 `](`)

  override def complementary: Boolean = false

  override def reference: Seq[GenIntervalRelation] =
    (false forAll x <  0) ::
    (true  forAll x >= 0  & x <= 0) ::
    (false forAll x >  0  & x <  10) ::
    (true  forAll x >= 10 & x <  20) ::
    (false forAll x >= 20 & x <= 20) ::
    (true  forAll x >  20 & x <  30) ::
    (false forAll x >= 30 & x <= 30) ::
    (true  forAll x >  30) ::
    Nil

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    (20`[`, false forAll x >= 20 & x <= 20) ::
    (20`]`, false forAll x >= 20 & x <= 20) ::
    (20`)`, true  forAll x >= 10 & x <  20) ::
    (30`(`, true  forAll x >  30) ::
    (40`)`, true  forAll x >  30) ::
    (-1`[`, false forAll x <  0 ) ::
    ( 0`]`, true  forAll x >= 0  & x <= 0 ) ::
    Nil

  override def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories(domainOps).flatMap { factoryTuple =>
      List(
        // current:           false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        // appended:
        // X------------------false-----------------------X
        //
        // result:            false
        // X ......Seq1....... )|(--true--)[-----false----X
        //                     20         30
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = false),
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
        // appended:
        // X-------------------true-----------------------X
        //
        // result:            false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:           false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        // appended:
        // X-------------------true--------------](-false-X
        //                                       40
        // result:            false      false
        // X ......Seq1....... )|(--true--)|(-tr-](-false-X
        //                     20         30
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.buildUnsafe(ArraySeq(40 `](`), complementary = true),
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
        // appended:          false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        //
        // result:            false      false
        // X ......Seq1....... )|(--true--)|(-----true----X
        //                     20         30
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.buildUnsafe(bounds, complementary),
          reference
        )
      )
    }
  }

  override def slicedCases: Seq[SegmentSeqSlicedTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentSeqSlicedTest.TestCase(
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
      SegmentSeqSlicedTest.TestCase(
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
      SegmentSeqSlicedTest.TestCase(
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
      SegmentSeqSlicedTest.TestCase(
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
      SegmentSeqSlicedTest.TestCase(
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
      )
    )
}