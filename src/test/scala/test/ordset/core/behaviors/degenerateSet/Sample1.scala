package test.ordset.core.behaviors.degenerateSet

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
}
