package test.ordset.core.behaviors.segmentSeq.singleBoundedSet

import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import test.ordset.core.behaviors.segmentSeq.SegmentMoveToBoundTest
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.language.postfixOps

trait Sample1[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean] =>

  override def sample: String = "1"

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
}
