package test.ordset.core.samples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.SetBuilderNotation.BoundBuilder
import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels

abstract class SegmentSeqSample[E, D <: Domain[E], W](
  implicit
  val domainOps: DomainOps[E, D],
  val rngManager: RngManager
) {
  def sample: String

  def labels: Set[Label] = Set.empty + Labels.sample(sample)

  def sequence: SegmentSeq[E, D, W]

  def bounds: IterableOnce[Bound.Upper[E]]

  def complementary: Boolean

  def reference: Seq[IntervalRelation[E, D, W]]

  override def toString: String = Labels.caseShow.show(labels)

  // Protected section -------------------------------------------------------- //
  protected type GenBound = Bound[E]
  protected type GenSegmentSeq = SegmentSeq[E, D, W]
  protected type GenIntervalRelation = IntervalRelation[E, D, W]

  protected val x: BoundBuilder[E, D] = BoundBuilder[E, D](domainOps)
}
