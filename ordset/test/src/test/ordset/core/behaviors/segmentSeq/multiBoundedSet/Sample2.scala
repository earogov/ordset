package test.ordset.core.behaviors.segmentSeq.multiBoundedSet

import ordset.core.{Bound, SegmentSeq}
import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import test.ordset.core.behaviors.segmentSeq.SegmentMoveToBoundTest
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample2[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override def sample: String = "2"

  override def bounds: IterableOnce[GenUpperBound] =
    ArraySeq(0 `](`, 5 `)[`, 7 `)[`, 20 `](`, 25 `)[`, 35 `](`, 40 `)[`, 60 `)[`)

  override def complementary: Boolean = true

  override def reference: Seq[GenIntervalRelation] =
    (true  forAll x <= 0) ::
    (false forAll x >  0  & x <  5) ::
    (true  forAll x >= 5  & x <  7) ::
    (false forAll x >= 7  & x <= 20) ::
    (true  forAll x >  20 & x <  25) ::
    (false forAll x >= 25 & x <= 35) ::
    (true  forAll x >  35 & x <  40) ::
    (false forAll x >= 40 & x <  60) ::
    (true  forAll x >= 60) ::
    Nil

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    ( 7`[`, false forAll x >= 7  & x <= 20) ::
    (30`)`, false forAll x >= 25 & x <= 35) ::
    ( 0`)`, true  forAll x <= 0) ::
    (40`]`, false forAll x >= 40 & x <  60) ::
    (40`]`, false forAll x >= 40 & x <  60) ::
    (45`[`, false forAll x >= 40 & x <  60) ::
    ( 5`)`, false forAll x >  0  & x <  5 ) ::
    Nil
}
