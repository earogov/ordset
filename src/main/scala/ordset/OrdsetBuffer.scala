package ordset

import scala.util.{Success, Try}

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

abstract class OrdsetBuffer[@sp(spNum) T](
    val validation: InnerBoundsValidation[T]
) {

  protected var prev: Bound[T]

  def addBound(bound: Bound[T]): Try[Unit] = {
    val v = if (prev != null) validation(prev, bound) else Success({})
    if (v.isSuccess) {prev = bound; collect(bound)}
    v
  }

  protected def collect(bound: Bound[T]): Unit
}

