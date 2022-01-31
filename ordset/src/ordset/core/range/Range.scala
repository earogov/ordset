package ordset.core.range

import ordset.{Eq, Hash, Show}

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

  override def toString(): String = Range.defaultShowInstance.show(this)
}

object Range {

  /**
   * Returns default [[Hash]] and [[Eq]] implementation for range.
   */
  implicit def defaultHash[E](implicit elementHash: Hash[E]): Hash[Range[E]] = new DefaultHash(elementHash)

  /**
   * Returns default [[Show]] implementation for range.
   */
  implicit def defaultShow[E](implicit elementShow: Show[E]): Show[Range[E]] = new DefaultShow(elementShow)

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

  /**
   * Default [[Hash]] and [[Eq]] implementation for range.
   */
  class DefaultHash[E, R[X] <: Range[X]](val elementHash: Hash[E]) extends Hash[R[E]] {

    import ordset.util.HashUtil._

    private val hashConst: Int = 0x97C30FE5

    override def eqv(x: R[E], y: R[E]): Boolean = 
      x match {
        case x: Range.NonEmpty[E] =>
          y match {
            case y: Range.NonEmpty[E] =>
              elementHash.eqv(x.lower, y.lower) && elementHash.eqv(x.upper, y.upper)
            case _ =>
              false
          }
        case _ => 
          y.isEmpty
      }

    override def hash(x: R[E]): Int = 
      x match {
        case x: Range.NonEmpty[E] => product2Hash(elementHash.hash(x.lower), elementHash.hash(x.upper))
        case _ => hashConst
      }
  }

  /**
   * Default [[Show]] implementation for range.
   */
  class DefaultShow[E, R[+X] <: Range[X]](val elementShow: Show[E]) extends Show[R[E]] {

    import DefaultShow.*

    override def show(x: R[E]): String = 
      x match {
        case x: Range.NonEmpty[E] =>
          val lowerStr = elementShow.show(x.lower)
          val upperStr = elementShow.show(x.upper)
          s"$rangeBegin$lowerStr$separator$upperStr"
        case _ => 
          emptyRange
      }
  }

  object DefaultShow {

    val emptyRange: String = "{}"

    val rangeBegin: String = "["

    val rangeEnd: String = "]"

    val separator: String = ", "
  }

  // Private section ---------------------------------------------------------- //
  private val defaultShowInstance: DefaultShow[Any, Range] = new DefaultShow(Show.fromToString)
}