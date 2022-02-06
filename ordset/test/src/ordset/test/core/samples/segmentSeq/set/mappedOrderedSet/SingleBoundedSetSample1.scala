package ordset.test.core.samples.segmentSeq.set.mappedOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.MappedSegmentSeq
import ordset.core.segmentSeq.set.{ArrayOrderedSet, MappedOrderedSet, OrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.MappedSeqSample
import ordset.test.core.Labels

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class SingleBoundedSetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends MappedSeqSample[Int, D, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.singleBoundedSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  override val originalSeq: OrderedSet[Int, D] =
    ArrayOrderedSet.unchecked(
      ArraySeq(-100`)[`, -80`](`, -60`)[`, -40`)[`, -20`)[`, 0`](`, 20`](`, 40`](`),
      false
    )

  // sequence:
  //               true                                   false
  // X----------------------------------------](------------------------X
  //                                          0
  // originalSeq:                             |
  //                                          |
  //  false   true  false  true   false  true | false  true     false
  // X-----)[-----](-----)[-----)[-----)[-----](-----](-----](----------X
  //     -100    -80    -60    -40    -20     0      20     40
  override val sequence: MappedSegmentSeq[Int, D, Boolean, Boolean, Any] =
    MappedOrderedSet.apply(originalSeq, s => s.domainOps.extendedOrd.lteqv(s.upper, 0`](`))
}
