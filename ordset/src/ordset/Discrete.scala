package ordset

import ordset.util.OptionUtil

/**
 * Typeclass specifying sequence of elements of discrete ordered set. 
 *
 * Implementations must enforce conditions:
 * <tr>1. predecessor(successor(x)) `=` x</tr>
 * <tr>2. successor(predecessor(x)) `=` x</tr>  
 * 
 * @tparam E element type parameter
 */
trait Discrete[E] extends Discrete.Succeeding[E] with Discrete.Preceding[E]

object Discrete {

  /** 
   * Typeclass to get succeeding elements of sequence.
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