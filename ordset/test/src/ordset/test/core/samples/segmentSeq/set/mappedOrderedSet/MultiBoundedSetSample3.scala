package ordset.test.core.samples.segmentSeq.set.mappedOrderedSet

import ordset.core.Bound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.MappedSegmentSeq
import ordset.core.segmentSeq.set.{ArrayOrderedSet, MappedOrderedSet, OrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.core.SegmentSeqLabels
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.MappedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class MultiBoundedSetSample3[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends MappedSeqSample[Int, D, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.multiBoundedSet.Sample3[D] {

  override val labels: Set[Label] = super.labels + SegmentSeqLabels.multiBoundedSeq

  override val originalSeq: OrderedSet[Int, D] =
    ArrayOrderedSet.unchecked(
      ArraySeq.from(bounds).appendedAll(List(90`)[`, 100`](`)),
      !complementary
    )

  // sequence:
  //
  // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[---------t---------X
  //       0      10     20     30     40     50     60     70     80
  //
  // originalSeq:
  //
  // X--t--)[--f--)[--t--)[--f--)[--t--)[--f--](--t--](--f--)[--t--)[--f--)[--t--](--f--X
  //       0      10     20     30     40     50     60     70     80     90     100
  override val sequence: MappedSegmentSeq[Int, D, Boolean, Boolean, Any] =
    MappedOrderedSet.apply(
      originalSeq,
      s =>
        if (s.domainOps.extendedOrd.lteqv(s.lower, 80`)[`)) !s.value
        else true
    )
}
