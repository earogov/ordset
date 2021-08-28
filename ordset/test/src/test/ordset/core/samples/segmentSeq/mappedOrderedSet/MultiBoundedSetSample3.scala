package test.ordset.core.samples.segmentSeq.mappedOrderedSet

import ordset.core.{MappedSegmentSeq, Bound}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, MappedOrderedSet, OrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.implementations.domain.BoundSelector
import test.ordset.core.samples.segmentSeq.MappedSeqSample
import test.ordset.core.{Labels, TestRngUtil}

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class MultiBoundedSetSample3[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends MappedSeqSample[Int, D, Boolean, Boolean]
  with test.ordset.core.behaviors.segmentSeq.multiBoundedSet.Sample3[D] {

  override val labels: Set[Label] = super.labels + Labels.multiBoundedSeq

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
        if (s.domainOps.extendedOrd.lteqv(s.lowerExtended, 80`)[`)) !s.value
        else true
    )
}
