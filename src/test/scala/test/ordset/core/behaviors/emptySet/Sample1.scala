package test.ordset.core.behaviors.emptySet

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
    (false forAll x) ::
    Nil

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    ( 10`)`, false forAll x) ::
    ( 15`[`, false forAll x) ::
    (-10`)`, false forAll x) ::
    (-15`[`, false forAll x) ::
    Nil
}
