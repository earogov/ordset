package test.ordset.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class MultiBoundedSetSample2[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
  with test.ordset.core.behaviors.segmentSeq.multiBoundedSet.Sample2[D] {

  override def labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  override def sequence: GenSegmentSeq =
    // c intersection d:
    //      in       out     in             out               in            out             in       out     in
    // X--------0](0-----5)[5--7)[7--------------------20](20----25)[25--------------35](35----40)[40--60)[60---X
    //
    // c:
    //              in                       out              in            out                    in
    // X-----------------------7)[7--------------------20](20----25)[25--------------35](35---------------------X
    //
    // d = a union b (merged):
    //     in        out                                        in                                  out      in
    // X--------0](0-----5)[5------------------------------------------------------------------40)[40--60)[60---X
    //
    // d = a union b:
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
    ZippedOrderedSet.intersection(
      // c
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq(7 `)[`, 20 `](`, 25 `)[`, 35 `](`),
        complementary = true
      ),
      // d
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
    )
}
