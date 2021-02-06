package test.ordset.core.behaviors.segmentSeq.multiBoundedSet

import ordset.core.Bound
import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.util.label.Label
import test.ordset.core.behaviors.segmentSeq.{SegmentMoveToBoundTest, SegmentSeqAppendedTest, SegmentSeqFactories}
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample1[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
  with SegmentSeqAppendedTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean] =>

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

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    (10`)`, true  forAll x >= 0  & x < 10) ::
    (10`)`, true  forAll x >= 0  & x < 10) ::
    (30`[`, false forAll x >= 30 & x < 40) ::
    (40`)`, false forAll x >= 30 & x < 40) ::
    (40`[`, true  forAll x >= 40) ::
    (45`[`, true  forAll x >= 40) ::
    (25`[`, true  forAll x >= 20 & x < 30) ::
    (-5`[`, false forAll x <  0) ::
    Nil

  override def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories(domainOps).flatMap { factoryTuple =>
      List(
        // current:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        // appended:
        // X------------------false-----------------------X
        // result:
        // X ......Seq1....... )[----------false----------X
        //                     30
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = false),
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
        // result:
        // X ......Seq1....... )[--false--)[-----true-----X
        //                     30         40
        SegmentSeqAppendedTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = true),
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
          factoryTuple._2.buildUnsafe(ArraySeq(25 `](`, 30 `)[`, 50 `](`), complementary = false),
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
          factoryTuple._2.buildUnsafe(ArraySeq(25 `](`, 30 `)[`, 50 `](`), complementary = true),
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
          factoryTuple._2.buildUnsafe(bounds, complementary),
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
          factoryTuple._2.buildUnsafe(ArraySeq(40 `)[`), complementary = true),
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
}
