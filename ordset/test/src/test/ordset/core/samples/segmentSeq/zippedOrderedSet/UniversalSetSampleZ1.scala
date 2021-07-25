package test.ordset.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchTest
import test.ordset.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class UniversalSetSampleZ1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
    with test.ordset.core.behaviors.zippedSeq.universalSet.SampleZ1[D] {
  
  override val labels: Set[Label] = super.labels + Labels.universalSet

  val firstSeq: TreapOrderedSet[Int, D] = UniformOrderedSet.defaultUniversal

  val secondSeq: TreapOrderedSet[Int, D] = UniformOrderedSet.defaultEmpty

  // firstSeq:
  // X-------------------------t-----------------------------X
  //
  // secondSeq:
  // X-------------------------f-----------------------------X
  //
  // firstSeq union secondSeq:
  // X-------------------------t-----------------------------X
  //
  override def sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.intersection(firstSeq, secondSeq)
}