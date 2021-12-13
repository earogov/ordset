package ordset

import ordset.util.OptionUtil

/**
 * Typeclass specifying sequence of elements of discrete ordered set. 
 *
 * Implementations MUST enforce conditions: 
 * <tr>
 *   1.a If `x` has successor, then: `predecessor(successor(x)) = x`.
 * </tr>
 * <tr>
 *   1.b Otherwise `∄` `y` such that: `predecessor(y) = x`.
 * </tr>
 * <tr>
 *   2.a If `x` has predecessor, then: `successor(predecessor(x)) = x`.
 * </tr>
 * <tr>
 *   2.b Otherwise `∄` `y` such that: `successor(y) = x`.
 * </tr>
 * <tr>
 *   3. If `x` is not included in set, then it doesn't have successor and predecessor.
 * </tr>
 */
trait Discrete[E] extends Discrete.Succeeding[E] with Discrete.Preceding[E]

object Discrete {

  /** 
   * Typeclass to get succeeding elements of sequence.
   * 
   * See condition 3 of [[Discrete]].
   */
  trait Succeeding[E] {

    /**
     * Returns:
     * <tr>`null`, if element has no successor;</tr>
     * <tr>the successor of element otherwise.</tr>
     */
    def successorOrNull(x: E): E | Null

    /**
     * Returns:
     * <tr>[[Option.empty]], if element has no successor;</tr>
     * <tr>the successor of element otherwise.</tr>
     */
    def successorOpt(x: E): Option[E] = OptionUtil.optionOfNullable(successorOrNull(x))

    /**
     * Returns `true` if element has successor.
     */ 
    def hasSuccessor(x: E): Boolean
  }

  object Succeeding {

    /** 
     * Typeclass to get succeeding elements of infinite sequence.
     */
    trait Infinite[E] extends Succeeding[E] {

      /**
       * Returns the successor of element.
       */
      def successor(x: E): E

      override def successorOrNull(x: E): E | Null = successor(x)

      override def hasSuccessor(x: E): Boolean = true
    }
  }

  /** 
   * Typeclass to get preceding elements of sequence.
   * 
   * See condition 3 of [[Discrete]].
   */
  trait Preceding[E] {

    /**
     * Returns:
     * <tr>`null`, if element has no predecessor;</tr>
     * <tr>the predecessor of element otherwise.</tr>
     */
    def predecessorOrNull(x: E): E | Null

    /**
     * Returns:
     * <tr>[[Option.empty]], if element has no predecessor;</tr>
     * <tr>the predecessor of element otherwise.</tr>
     */
    def predecessorOpt(x: E): Option[E] = Option(x)

    /**
     * Returns `true` if element has predecessor.
     */ 
    def hasPredecessor(x: E): Boolean
  }

  object Preceding {

    /** 
     * Typeclass to get preceding elements of infinite sequence.
     */
    trait Infinite[E] extends Preceding[E] {

      /**
       * Returns the predecessor of element.
       */
      def predecessor(x: E): E

      override def predecessorOrNull(x: E): E | Null = predecessor(x)

      override def hasPredecessor(x: E): Boolean = true
    }
  }

  /**
   * Typeclass specifying infinite sequence of elements of discrete ordered set. 
   *
   * Each element of the set has both successor and predecessor.
   * 
   * See conditions 1.a and 2.a of [[Discrete]].
   */
  trait Infinite[E] extends Discrete[E] with Succeeding.Infinite[E] with Preceding.Infinite[E] {

    /**
     * Returns the successor of element.
     */
    def successor(x: E): E

    /**
     * Returns the predecessor of element.
     */
    def predecessor(x: E): E

    override def successorOrNull(x: E): E | Null = successor(x)

    override def predecessorOrNull(x: E): E | Null = predecessor(x)
  }
}