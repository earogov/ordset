package test.ordset

import org.scalatest.funspec.AnyFunSpec

import scala.annotation.tailrec

trait IntervalFlowBehaviors[T, V] { this: AnyFunSpec =>

  import ordset._

  type IMappingLazyList = LazyList[IntervalMapping[T, V]]

  protected def toIMappingList(flow: IntervalFlow[T, V]): IMappingLazyList = flow.toLazyList.map(_._2)

  def coverDomainWithoutGapsAndOverlapping(
      descr: String, flow: IntervalFlow[T, V], expected: IMappingLazyList): Unit = {

    it(s"should cover domain without gaps and overlapping for $descr") {
      assert(toIMappingList(flow) == expected)
    }
  }

  def supportMoveToElement(
      descr: String, flow: IntervalFlow[T, V], moveSeq: Seq[(Bound[T], IntervalMapping[T, V])]): Unit = {

    it(s"should support move to specified bound for $descr") {

      @tailrec
      def loop(flow: IntervalFlow[T, V], moveSeq: Seq[(Bound[T], IntervalMapping[T, V])]): Unit = moveSeq match {
        case x :: xs =>
          val res = flow.moveTo(x._1)
          assert(res._2 == x._2)
          // Use new flow to test their sequence.
          loop(res._1, xs)
        case _ =>
      }
      loop(flow, moveSeq)
    }
  }

  def supportMoveToElementAndThenMoveNext(
    descr: String, flow: IntervalFlow[T, V], moveSeq: Seq[(Bound[T], Option[IntervalMapping[T, V]])]): Unit = {

    it(s"should support move to specified bound and then move to next interval for $descr") {

      @tailrec
      def loop(flow: IntervalFlow[T, V], moveSeq: Seq[(Bound[T], Option[IntervalMapping[T, V]])]): Unit = moveSeq match {
        case x :: xs =>
          val res = flow.moveTo(x._1)
          res._1 match {
            case f: IntervalFlow.Active[T, V] =>
              val resNext = f.moveNext
              (resNext._1, x._2) match {
                case (f @ IntervalFlow.Active(_, _), Some(expected)) => assert(f.moveNext._2 == expected)
                case (IntervalFlow.Finished(_), None) => // success case
                case (IntervalFlow.Finished(_), Some(_)) => fail("Active flow expected")
                case _ => fail("Finished flow expected")
            }
            case _ => fail("Active flow expected to call moveNext")
          }
          // Use new flow (before moveNext) to test their sequence.
          loop(res._1, xs)
        case _ =>
      }
      loop(flow, moveSeq)
    }
  }
}