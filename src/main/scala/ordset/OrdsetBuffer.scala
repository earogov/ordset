package ordset

import scala.util.{Success, Try}

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

abstract class OrdsetBuffer[@sp(spNum) E](
    val validation: InnerBoundsValidation[E]
) {

  protected var prev: Bound[E]

  def addBound(bound: Bound[E]): Try[Unit] = {
    val v = if (prev != null) validation(prev, bound) else Success({})
    if (v.isSuccess) {prev = bound; collect(bound)}
    v
  }

  protected def collect(bound: Bound[E]): Unit
}

