package ordset.util.types

trait Dual[-U] { type Out }

object Dual {

  object Implicits {

    implicit def reversedDual[U, V](implicit ev: V >|< U): U >|< V = new Dual[U] { type Out = V }
  }
}