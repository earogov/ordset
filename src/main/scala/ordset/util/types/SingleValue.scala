package ordset.util.types

trait SingleValue[+V] {

  def get: V
}

object SingleValue {

  def apply[V](value: V): SingleValue[V] = DefaultImpl(value)

  private case class DefaultImpl[+V](override val get: V) extends SingleValue[V]
}
