package test.ordset.core.behaviors.segmentSeq.multiBoundedSet

import ordset.core.Bound
import ordset.core.domain.Domain
import ordset.core.set.TreapOrderedSet
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.segmentSeq.{SegmentMoveToBoundTest, SegmentSeqAppendedV0Test, SegmentSeqFactories, SegmentSeqSlicedTest}
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample3[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
  with SegmentSeqAppendedV0Test[Int, D, Boolean]
  with SegmentSeqSlicedTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean] =>

  override def sample: String = "3"

  override def bounds: IterableOnce[Bound.Upper[Int]] =
    ArraySeq(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`, 50 `](`, 60 `](`, 70 `)[`, 80 `)[`)

  override def complementary: Boolean = false

  override def reference: Seq[GenIntervalRelation] =
    (false forAll x <  0) ::
    (true  forAll x >= 0  & x <  10) ::
    (false forAll x >= 10 & x <  20) ::
    (true  forAll x >= 20 & x <  30) ::
    (false forAll x >= 30 & x <  40) ::
    (true  forAll x >= 40 & x <= 50) ::
    (false forAll x >  50 & x <= 60) ::
    (true  forAll x >  60 & x <  70) ::
    (false forAll x >= 70 & x <  80) ::
    (true  forAll x >= 80) ::
    Nil

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    (10`)`, true  forAll x >= 0  & x <  10) ::
    (10`)`, true  forAll x >= 0  & x <  10) ::
    (30`[`, false forAll x >= 30 & x <  40) ::
    (40`)`, false forAll x >= 30 & x <  40) ::
    (40`[`, true  forAll x >= 40 & x <= 50) ::
    (45`[`, true  forAll x >= 40 & x <= 50) ::
    (25`[`, true  forAll x >= 20 & x <  30) ::
    (-5`[`, false forAll x <  0) ::
    (80`[`, true  forAll x >= 80) ::
    (70`)`, true  forAll x >  60 & x <  70) ::
    (75`[`, false forAll x >= 70 & x <  80) ::
    (70`]`, false forAll x >= 70 & x <  80) ::
    (70`[`, false forAll x >= 70 & x <  80) ::
    Nil

  override def appendedV0Cases: Seq[SegmentSeqAppendedV0Test.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // appended:
        // X--------------------false---------------------X
        //
        // result:
        // X ..Seq1.. )[--------------false---------------X
        //            70
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70) ::
          Nil
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // appended:
        // X--------------------true----------------------X
        //
        // result:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.buildUnsafe(ArraySeq.empty, complementary = true),
          reference
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // appended:
        // X------true-----)[-----------false-------------X
        //                 75
        // result:
        // X ..Seq1.. )[-------------false----------------X
        //            70
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.buildUnsafe(ArraySeq(75 `)[`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70) ::
          Nil
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // appended:
        // X-----false-----)[------------true-------------X
        //                 75
        // result:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.buildUnsafe(ArraySeq(75 `)[`), complementary = false),
          reference
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // appended:
        //                               true
        // X--false--](--true--)[--false--)|(----false----X
        //           25        80         90
        // result:
        //                               true
        // X ..Seq1.. )[------false-------)|(----false----X
        //            70                  90
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("E"),
          factoryTuple._2.buildUnsafe(ArraySeq(25 `](`, 80 `)[`, 90 `)[`, 90 `](`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  90) ::
          (true  forAll x >= 90 & x <= 90) ::
          (false forAll x >  90) ::
          Nil
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // appended:
        //                               false
        // X---true--](-false--)[---true--)|(----true-----X
        //           25        80         90
        // result:
        //                               false
        // X ..Seq1.. )[-false-)[---true--)|(----true-----X
        //            70       80         90
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("F"),
          factoryTuple._2.buildUnsafe(ArraySeq(25 `](`, 80 `)[`, 90 `)[`, 90 `](`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80 & x <  90) ::
          (false forAll x >= 90 & x <= 90) ::
          (true  forAll x >  90) ::
          Nil
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // appended:
        //                        false false false
        // X----------true---------)|(-t-)|(-t-)|(--true--X
        //                         82    84    86
        // result:
        //                        false false false
        // X ..Seq1.. )[-false-)[--)|(-t-)|(-t-)|(--true--X
        //            70       80  82    84    86
        SegmentSeqAppendedV0Test.TestCase(
          factoryTuple._1 + Label("G"),
          factoryTuple._2.buildUnsafe(
            ArraySeq(82 `)[`, 82 `](`, 84 `)[`, 84 `](`, 86 `)[`, 86 `](`),
            complementary = true
          ),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80 & x <  82) ::
          (false forAll x >= 82 & x <= 82) ::
          (true  forAll x >  82 & x <  84) ::
          (false forAll x >= 84 & x <= 84) ::
          (true  forAll x >  84 & x <  86) ::
          (false forAll x >= 86 & x <= 86) ::
          (true  forAll x >  86) ::
          Nil
        )
      )
    }
  }

  override def slicedCases: Seq[SegmentSeqSlicedTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentSeqSlicedTest.TestCase(
        -10`]`,
        (false forAll x) ::
        Nil,
        reference
      ),
      SegmentSeqSlicedTest.TestCase(
        0`)`,
        (false forAll x) ::
        Nil,
        reference
      ),
      SegmentSeqSlicedTest.TestCase(
        5`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0) ::
        Nil,
        (true  forAll x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      SegmentSeqSlicedTest.TestCase(
        20`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 ) ::
        Nil,
        (false forAll x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      SegmentSeqSlicedTest.TestCase(
        60`(`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60) ::
        Nil,
        (true  forAll x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      SegmentSeqSlicedTest.TestCase(
        80`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70) ::
        Nil,
        (false forAll x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      SegmentSeqSlicedTest.TestCase(
        80`[`,
        reference,
        (true  forAll x) ::
        Nil
      )
    )
}
