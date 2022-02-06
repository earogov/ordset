package ordset.test.core.samples.segmentSeq.set.arrayOrderedSet

import ordset.core.segmentSeq.ArraySegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.ArrayOrderedSet
import ordset.core.syntax.SetBuilderNotation.*
import ordset.random.RngManager
import ordset.util.label.Label

import scala.language.postfixOps
import ordset.test.core.Labels
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.ArraySeqSample

class SingleBoundedSetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ArraySeqSample[Int, D, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.singleBoundedSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  override val sequence: ArraySegmentSeq[Int, D, Boolean] =
    ArrayOrderedSet.getFactory.unsafeBuildAsc(bounds, complementary)
}
