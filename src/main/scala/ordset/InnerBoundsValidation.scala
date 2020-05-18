package ordset

import scala.util.{Success, Try}

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

trait InnerBoundsValidation[@sp(spNum)-E] extends Function2[Bound[E], Bound[E], Try[Unit]] {

  def apply(v1: Bound[E], v2: Bound[E]): Try[Unit] = run(v1, v2)

  def run(prev: Bound[E], next: Bound[E]): Try[Unit]
}

object InnerBoundsValidation {

  def unchecked[E]: InnerBoundsValidation[E] = Unchecked

  private object Unchecked extends InnerBoundsValidation[Any] {

    override def run(prev: Bound[Any], next: Bound[Any]): Try[Unit] = Success({})
  }
}