package ordset.core.range

/**
 * Range of elements (empty or non-empty).
 * 
 * Note, any range implementation MUST be a subclass of [[Range.Empty]] or [[Range.NonEmpty]].
 * 
 * @tparam E type of elements.
 */
trait Range[+E] {

  /** 
   * Returns `true` if range is empty. 
   */
  def isEmpty: Boolean

  /** 
   * Returns `true` if range is non-empty. 
   */
  def isNonEmpty: Boolean
}

object Range {

  /**
   * Empty range. Doesn't include any elements.
   */
  trait Empty extends Range[Nothing] {

    final override def isEmpty: Boolean = true

    final override def isNonEmpty: Boolean = false
  }

  /**
   * Non-empty range. Includes all elements between `lower` (including) and `upper` (including).
   * 
   * @tparam E type of elements.
   */
  trait NonEmpty[+E] extends Range[E] {

    /**
     * Lower bound of range (including).
     */
    def lower: E

    /**
     * Upper bound of range (including).
     */
    def upper: E

    final override def isEmpty: Boolean = false

    final override def isNonEmpty: Boolean = true
  }
}