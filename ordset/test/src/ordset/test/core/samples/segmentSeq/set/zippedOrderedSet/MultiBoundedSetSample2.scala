package ordset.test.core.samples.segmentSeq.set.zippedOrderedSet

import ordset.core.segmentSeq.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.{ArrayOrderedSet, OrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.core.SegmentSeqLabels
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class MultiBoundedSetSample2[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.multiBoundedSet.Sample2[D] {

  override val labels: Set[Label] = super.labels + SegmentSeqLabels.multiBoundedSeq

  override val firstSeq: OrderedSet[Int, D] =
    ArrayOrderedSet.unchecked[Int, D](
      ArraySeq(7 `)[`, 20 `](`, 25 `)[`, 35 `](`),
      complementary = true
    )

  override val secondSeq: OrderedSet[Int, D] =
    ZippedOrderedSet.union(
      // a
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
        complementary = true
      ),
      // b
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq(5 `)[`, 12 `](`, 20 `)[`, 30 `](`, 60 `)[`),
        complementary = false
      )
    )

  // firstSeq intersection secondSeq:
  //      in       out     in             out               in            out             in       out     in
  // X--------0](0-----5)[5--7)[7--------------------20](20----25)[25--------------35](35----40)[40--60)[60---X
  //
  // firstSeq:
  //              in                       out              in            out                    in
  // X-----------------------7)[7--------------------20](20----25)[25--------------35](35---------------------X
  //
  // secondSeq = a union b (merged):
  //     in        out                                        in                                  out      in
  // X--------0](0-----5)[5------------------------------------------------------------------40)[40--60)[60---X
  //
  // secondSeq = a union b:
  //     in        out      in       in         in               in         in        in          out      in
  // X--------0](0-----5)[5----10)[10--12](12-------20)[20---------------30)|(30-------------40)[40--60)[60---X
  //
  // b:
  //         out               in               out              in                    out                 in
  // X-----------------5)[5------------12](12-------20)[20----------------30](30---------------------60)[60---X
  //
  // a:
  //     in             out                in                   out                  in                 out
  // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
  override val sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.intersection(firstSeq, secondSeq)
}
