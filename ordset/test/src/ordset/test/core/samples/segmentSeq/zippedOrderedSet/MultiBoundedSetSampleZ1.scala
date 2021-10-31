package ordset.test.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.*
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

class MultiBoundedSetSampleZ1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
  with ordset.test.core.behaviors.zippedSeq.multiBoundedSet.SampleZ1[D] {
  
  override val labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  override val firstSeq: OrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuildAsc(
    ArraySeq(10`](`, 20`](`, 30`)[`),
    complementary = true,
    domainOps
  )()

  override val secondSeq: OrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuildAsc(
    ArraySeq(5`)[`, 15`)[`, 25`](`, 35`](`),
    complementary = false,
    domainOps
  )()

  // firstSeq:
  // X-----t----](------f-----](-------t-----)[-------f------X
  //            10            20             30
  // secondSeq:
  // X--f--)[-----t----)[------f-----](------t-------](---f--X
  //       5           15            25              35
  //
  // firstSeq intersection secondSeq:
  // X--f--)[-t-](---------f---------](---t--)[------f-------X
  //       5    10                   25      30
  override def sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.intersection(firstSeq, secondSeq)
}
