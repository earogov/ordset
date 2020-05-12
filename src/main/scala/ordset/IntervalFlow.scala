package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait IntervalFlow[@sp(spNum) T, +V] {
  import IntervalFlow._

  def moveTo(bound: Bound[T]): Result[T, V]

  def toLazyList: LazyList[Result[T, V]]
}

object IntervalFlow {

  type Result[T, +V] = (IntervalFlow[T, V], IntervalMapping[T, V])

  case class Active[T, +V](private val mvTo: Bound[T] => Result[T, V], private val mvNext: () => Result[T, V]
  ) extends IntervalFlow[T, V] {

    override def moveTo(bound: Bound[T]): Result[T, V] = mvTo(bound)

    def moveNext: Result[T, V] = mvNext()

    override def toLazyList: LazyList[Result[T, V]] = {
      val next = moveNext
      LazyList.cons(next, next._1.toLazyList)
    }
  }

  case class Finished[T, V](private val mvTo: Bound[T] => Result[T, V]) extends IntervalFlow[T, V] {

    override def moveTo(bound: Bound[T]): Result[T, V] = mvTo(bound)

    override def toLazyList: LazyList[Result[T, Nothing]] = LazyList.empty
  }
}
