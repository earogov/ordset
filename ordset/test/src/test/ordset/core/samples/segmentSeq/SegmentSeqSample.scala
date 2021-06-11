package test.ordset.core.samples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.SetBuilderNotation.BoundBuilder
import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels

abstract class SegmentSeqSample[E, D <: Domain[E], V](
  implicit
  val domainOps: DomainOps[E, D],
  val rngManager: RngManager
) {
  type GenBound = Bound.Upper[E]
  
  type GenSegmentSeq = SegmentSeq[E, D, V]
  
  type GenIntervalRelation = IntervalRelation[E, D, V]

  val x: BoundBuilder[E, D] = BoundBuilder[E, D](domainOps)
  
  def sample: String

  def labels: Set[Label] = Set.empty + Labels.sample(sample)

  def sequence: GenSegmentSeq

  def bounds: IterableOnce[GenBound]

  def complementary: Boolean

  def reference: Seq[GenIntervalRelation]

  override def toString: String = Labels.caseShow.show(labels)
}
