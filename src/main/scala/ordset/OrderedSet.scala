package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

trait OrderedSet[@sp(spNum) T] {
  import OrderedSet._

  def order: Order[Bound[T]]

  def isEmpty: Boolean

  def isUniversal: Boolean

  def intervalFlow: IFlow[T]
}

object OrderedSet {

  type IFlow[T] = IntervalFlow[T, Boolean]
  type IMapping[+T] = IntervalMapping[T, Boolean]
  type IFlowResult[T] = IntervalFlow.Result[T, Boolean]
}
