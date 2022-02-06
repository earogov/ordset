package ordset.test.core.samples.segmentSeq.set.mappedOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.MappedSegmentSeq
import ordset.core.segmentSeq.set.{ArrayOrderedSet, MappedValueOrderedSet, OrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.MappedSeqSample
import ordset.test.core.Labels

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class DegenerateSetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends MappedSeqSample[Int, D, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.degenerateSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.degenerateSeq

  override val originalSeq: OrderedSet[Int, D] =
    ArrayOrderedSet.getFactory.unsafeBuildAsc(bounds, !complementary)

  // sequence:
  //
  //        true               false     false
  // X---f---)|(---f---)[---t---)|(---t---)|(---t---X
  //          0        10        20        30
  //
  // originalSeq:
  //
  //        false              true      true
  // X---t---)|(---t---)[---f---)|(---f---)|(---f---X
  //          0        10        20        30
  override val sequence: MappedSegmentSeq[Int, D, Boolean, Boolean, Any] =
    MappedValueOrderedSet.inversion(originalSeq)
}
