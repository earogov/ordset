package test.ordset

import org.scalatest.funspec.AnyFunSpec

class ZippedOrderedSetSpec extends AnyFunSpec
with SegmentSeqCases[Int, Boolean]
with SegmentSeqBehaviors[Int, Boolean] {

  import ordset._
  import scala.collection.immutable.ArraySeq
  import OrderWithDir._

  import scala.language.postfixOps
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._
  import test.syntax.ArraySyntax._

  type SegmentSeq = SetSegmentSeq[Int]

  implicit val domain: Domain[Int] = ContinuousDomain()

  override val emptyCase: Option[ordset.SegmentSeq[Int, Boolean]] = None

  override val universalCase: Option[ordset.SegmentSeq[Int, Boolean]] = None

  override val singleBoundedCase: Option[ordset.SegmentSeq[Int, Boolean]] = None

  override val multiBoundedCase: Option[ordset.SegmentSeq[Int, Boolean]] = Some(
    ZippedOrderedSet.union(
      new ArrayOrderedSet[Int](Array(0`)[`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq, complement = true),
      new ArrayOrderedSet[Int](Array(5`)[`, 12`](`, 15`)[`, 18`](`, 60`)[`).toImmutableArraySeq, complement = false)
    )
  )

  override val degenerateCase: Option[ordset.SegmentSeq[Int, Boolean]] = None

  describe("Zipped ordered set as a segment sequence") {

    it("should print all segments") {

      multiBoundedCase.get.firstSegment.forwardLazyList.toList.foreach(println(_))
    }
  }
}
