package test.ordset.core.samples

import cats.Show
import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.SetBuilderNotation.BoundBuilder
import ordset.util.label.Label

abstract class SegmentSeqSample[E, D <: Domain[E], W](
  implicit val domainOps: DomainOps[E, D]
) {

  def sample: String

  def labels: Set[Label] = Set.empty + Labels.sample(sample)

  def sequence: SegmentSeq[E, D, W]

  def reference: Seq[IntervalRelation[E, D, W]]

  override def toString: String = labelSetShow.show(labels)

  // Protected section -------------------------------------------------------- //
  protected type GenBound = Bound[E]
  protected type GenSegmentSeq = SegmentSeq[E, D, W]
  protected type GenIntervalRelation = IntervalRelation[E, D, W]

  protected val x: BoundBuilder[E, D] = BoundBuilder[E, D](domainOps)

  protected val labelSetShow: Show[Set[Label]] =
    Label.customSetShow("case(", ", ", ")")(Label.defaultShow)
}
