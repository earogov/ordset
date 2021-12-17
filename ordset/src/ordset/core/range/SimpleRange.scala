package ordset.core.range

import ordset.{Eq, Hash}

/**
 * Simple range of elements (empty or non-empty).
 * 
 * @tparam E type of elements.
 */
sealed trait SimpleRange[+E] extends Range[E]

object SimpleRange {

  /**
   * Returns empty range.
   */
  def empty: SimpleRange.Empty.type = SimpleRange.Empty

  /**
   * Returns non-empty range which includes all elements between `lower` (including) and `upper` (including).
   */
  def apply[E](lower: E, upper: E): SimpleRange.NonEmpty[E] = new NonEmpty(lower, upper)

  /**
   * Returns default [[Hash]] and [[Eq]] implementation for simple range.
   */
  implicit def defaultHash[E](implicit elementHash: Hash[E]): Hash[SimpleRange[E]] = new DefaultHash(elementHash)

  /**
   * Simple empty range. Doesn't include any elements.
   */
  object Empty extends SimpleRange[Nothing] with Range.Empty

  /**
   * Simple non-empty range. Includes all elements between `lower` (including) and `upper` (including).
   * 
   * @tparam E type of elements.
   */
  case class NonEmpty[+E](override val lower: E, override val upper: E) extends SimpleRange[E] with Range.NonEmpty[E]

  /**
   * Default [[Hash]] and [[Eq]] implementation for simple range.
   */
  class DefaultHash[E](val elementHash: Hash[E]) extends Hash[SimpleRange[E]] {

    import ordset.util.HashUtil._

    private val hashConst: Int = 0x97C30FE5

    override def eqv(x: SimpleRange[E], y: SimpleRange[E]): Boolean = 
      x match {
        case x: SimpleRange.NonEmpty[E] =>
          y match {
            case y: SimpleRange.NonEmpty[E] =>
              elementHash.eqv(x.lower, y.lower) && elementHash.eqv(x.upper, y.upper)
            case _ =>
              false
          }
        case _ => 
          y.isEmpty
      }

    override def hash(x: SimpleRange[E]): Int = 
      x match {
        case x: SimpleRange.NonEmpty[E] => product2Hash(elementHash.hash(x.lower), elementHash.hash(x.upper))
        case _ => hashConst
      }
  }
}
