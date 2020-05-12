package ordset

import scala.util.{Success, Try}

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

trait InnerBoundsValidation[@sp(spNum)-T] extends Function2[Bound[T], Bound[T], Try[Unit]] {

  def apply(v1: Bound[T], v2: Bound[T]): Try[Unit] = run(v1, v2)

  def run(prev: Bound[T], next: Bound[T]): Try[Unit]
}

object InnerBoundsValidation {

  def unchecked[T]: InnerBoundsValidation[T] = Unchecked

  private object Unchecked extends InnerBoundsValidation[Any] {

    override def run(prev: Bound[Any], next: Bound[Any]): Try[Unit] = Success({})
  }
}