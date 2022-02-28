package ordset.test.core.samples.segmentSeq.set.mappedOrderedSet

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

class MultiBoundedSetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends MappedSeqSample[Int, D, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.multiBoundedSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + SegmentSeqLabels.multiBoundedSeq

  override val originalSeq: OrderedSet[Int, D] =
    ArrayOrderedSet.unchecked(
      ArraySeq(-10`)[`, 0`)[`, 10`)[`, 12`)[`, 15`](`, 20`)[`, 22`)[`, 30`)[`, 40`)[`),
      false
    )
  
  // sequence:
  // 
  //       false        true            false               true       false   true 
  // X--------------)[-------)[----------------------)[-------------)[------)[------X
  //                0        10                      20             30      40
  // originalSeq:
  //
  //   false   true    false    true    false   true   false   true   false    true
  // X------)[------)[-------)[------)[------](------)[-----)[------)[------)[------X
  //       -10      0        10      12      15      20     22      30      40
  override val sequence: MappedSegmentSeq[Int, D, Boolean, Boolean, Any] = {
    val originalArraySeq = ArrayOrderedSet.unchecked[Int, D](ArraySeq.from(bounds), complementary)
    MappedOrderedSet.apply(originalSeq, s => originalArraySeq.getValueForExtended(s.upper))
  }
}
