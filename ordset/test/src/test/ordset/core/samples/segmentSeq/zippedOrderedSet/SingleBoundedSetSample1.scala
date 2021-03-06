package test.ordset.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class SingleBoundedSetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.singleBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  override def sequence: GenSegmentSeq =
    // b union c:
    //                          in                                                 out
    // X-----------------------------------------------0](0-----------------------------------------------------X
    //
    // c = a union b:
    //                          in                                                 out
    // X-----------------------------------------------0](0-----------------------------------------------------X
    //
    // b:
    //                          in                                                 out
    // X-----------------------------------------------0](0-----------------------------------------------------X
    //
    // a:
    //                                                 out
    // X--------------------------------------------------------------------------------------------------------X
    ZippedOrderedSet.union(
      // b
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq(0 `](`),
        complementary = true
      ),
      // c
      ZippedOrderedSet.union(
        // b
        ArrayOrderedSet.unchecked[Int, D](
          ArraySeq(0 `](`),
          complementary = true
        ),
        // a
        ArrayOrderedSet.unchecked[Int, D](
          ArraySeq.empty,
          complementary = false
        )
      )
    )
}
