package ordset.core.range

import ordset.{Hash, Show}

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
   * Returns default [[Hash]] and [[Eq]] implementation for range.
   */
  implicit def defaultHash[E](implicit elementHash: Hash[E]): Hash[SimpleRange[E]] = new Range.DefaultHash(elementHash)

  /**
   * Returns default [[Show]] implementation for range.
   */
  implicit def defaultShow[E](implicit elementShow: Show[E]): Show[SimpleRange[E]] = new Range.DefaultShow(elementShow)

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
}
