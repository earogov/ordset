package test.ordset.core.behaviors.multiBoundedSet

import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import test.ordset.core.behaviors.SegmentMoveToBoundTest
import test.ordset.core.samples.SegmentSeqSample

import scala.language.postfixOps

trait Sample1[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean] =>

  override def sample: String = "1"

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
}
