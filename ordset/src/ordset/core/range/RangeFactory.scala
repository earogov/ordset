package ordset.core.range

trait RangeFactory[-E, +R <: Range[E]] {

  def empty: R & Range.Empty

  def between(lower: E, upper: E): R

  def like(r: Range[E]): R = 
    r match {
      case r: Range.NonEmpty[E] => between(r.lower, r.upper)
      case _ => empty
    }
}

object RangeFactory {

  implicit def defaultFactory[E](): DefaultImpl[E] = factoryInstance.asInstanceOf

  class DefaultImpl[E] extends RangeFactory[E, Range[E]] {

    override val empty: Range.Empty = Range.Empty

    override def between(lower: E, upper: E): Range[E] = Range.NonEmpty(lower, upper)

    override def like(r: Range[E]): Range[E] = r
  }

  // Private section ---------------------------------------------------------- //
  private val factoryInstance: RangeFactory[Any, Range[Any]] = new DefaultImpl
}