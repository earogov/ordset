package ordset.test.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.Labels
import ordset.test.core.behaviors.zippedSeq.OriginalSeqPatchTest
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class UniversalSetSampleZ1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
    with ordset.test.core.behaviors.zippedSeq.universalSet.SampleZ1[D] {
  
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
