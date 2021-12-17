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
trait Discrete[E] extends Discrete.Succeeding[E] with Discrete.Preceding[E] with Reversible[Discrete[E], Discrete[E]] {

  override def reversed: Discrete[E] = new Discrete.ReversedImpl(this)
}

object Discrete {

  /** 
   * Typeclass to get succeeding elements of sequence.
   * 
   * See condition 3 of [[Discrete]].
   */
  trait Succeeding[E] extends Reversible[Succeeding[E], Preceding[E]] {

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

    override def reversed: Preceding[E] = new Preceding.ReversedImpl(this)
  }

  object Succeeding {

    /** 
     * Typeclass to get succeeding elements of infinite sequence.
     */
    trait Infinite[E] extends Succeeding[E] with Reversible[Succeeding.Infinite[E], Preceding.Infinite[E]] {

      /**
       * Returns the successor of element.
       */
      def successor(x: E): E

      override def successorOrNull(x: E): E | Null = successor(x)

      override def hasSuccessor(x: E): Boolean = true

      override def reversed: Preceding.Infinite[E] = new Preceding.Infinite.ReversedImpl(this)
    }

    object Infinite {

      /**
       * [[Discrete.Succeeding.Infinite]] typeclass received by reverting [[Discrete.Preceding.Infinite]] instance.
       */
      trait Reversed[E] extends Succeeding.Reversed[E] with Succeeding.Infinite[E] {

        override def successor(x: E): E = reversed.predecessor(x)
      }

      class ReversedImpl[E](original: Preceding.Infinite[E]) extends Reversed[E] {

        override val reversed: Preceding.Infinite[E] = original
      }
    }

    /**
     * [[Discrete.Succeeding]] typeclass received by reverting [[Discrete.Preceding]] instance.
     */
    trait Reversed[E] extends Succeeding[E] {

      override def successorOrNull(x: E): E | Null = reversed.predecessorOrNull(x)

      override def hasSuccessor(x: E): Boolean = reversed.hasPredecessor(x)
    }

    class ReversedImpl[E](original: Preceding[E]) extends Reversed[E] {

      override val reversed: Preceding[E] = original
    }
  }

  /** 
   * Typeclass to get preceding elements of sequence.
   * 
   * See condition 3 of [[Discrete]].
   */
  trait Preceding[E] extends Reversible[Preceding[E], Succeeding[E]] {

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

    override def reversed: Succeeding[E] = new Succeeding.ReversedImpl(this)
  }

  object Preceding {

    /** 
     * Typeclass to get preceding elements of infinite sequence.
     */
    trait Infinite[E] extends Preceding[E] with Reversible[Preceding.Infinite[E], Succeeding.Infinite[E]] {

      /**
       * Returns the predecessor of element.
       */
      def predecessor(x: E): E

      override def predecessorOrNull(x: E): E | Null = predecessor(x)

      override def hasPredecessor(x: E): Boolean = true

      override def reversed: Succeeding.Infinite[E] = new Succeeding.Infinite.ReversedImpl(this)
    }

    object Infinite {

      /**
       * [[Discrete.Preceding.Infinite]] typeclass received by reverting [[Discrete.Succeeding.Infinite]] instance.
       */
      trait Reversed[E] extends Preceding.Reversed[E] with Preceding.Infinite[E] {

        override def predecessor(x: E): E = reversed.successor(x)
      }

      class ReversedImpl[E](original: Succeeding.Infinite[E]) extends Reversed[E] {

        override val reversed: Succeeding.Infinite[E] = original
      }
    }

    /**
     * [[Discrete.Preceding]] typeclass received by reverting [[Discrete.Succeeding]] instance.
     */
    trait Reversed[E] extends Preceding[E] {

      override def predecessorOrNull(x: E): E | Null = reversed.successorOrNull(x)

      override def hasPredecessor(x: E): Boolean = reversed.hasSuccessor(x)
    }

    class ReversedImpl[E](original: Succeeding[E]) extends Reversed[E] {

      override val reversed: Succeeding[E] = original
    }
  }

  /**
   * Typeclass specifying infinite sequence of elements of discrete ordered set. 
   *
   * Each element of the set has both successor and predecessor.
   * 
   * See conditions 1.a and 2.a of [[Discrete]].
   */
  trait Infinite[E] 
    extends Discrete[E] 
    with Succeeding.Infinite[E] 
    with Preceding.Infinite[E]
    with Reversible[Infinite[E], Infinite[E]] {

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

    override def reversed: Infinite[E] = new Infinite.ReversedImpl(this)
  }

  object Infinite {

    /**
     * [[Discrete.Infinite]] typeclass received by reverting another [[Discrete.Infinite]] instance.
     */
    trait Reversed[E]
      extends Discrete.Reversed[E]
      with Succeeding.Infinite.Reversed[E]
      with Preceding.Infinite.Reversed[E]
      with Infinite[E]

    class ReversedImpl[E](original: Infinite[E]) extends Reversed[E] {

      override val reversed: Infinite[E] = original
    }
  }

  /**
   * [[Discrete]] typeclass received by reverting another [[Discrete]] instance.
   */
  trait Reversed[E] extends Succeeding.Reversed[E] with Preceding.Reversed[E] with Discrete[E]

  class ReversedImpl[E](original: Discrete[E]) extends Reversed[E] {

    override val reversed: Discrete[E] = original
  }
}