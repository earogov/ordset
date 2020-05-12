package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait OperatorTree[@sp(spNum) T, @sp(Boolean) V] {

}

object OperatorTree {

  case class Operator[T, V](left: OperatorTree[T, V], right: OperatorTree[T, V]) extends OperatorTree[T, V]

  case class Operand[T, V]()
}
