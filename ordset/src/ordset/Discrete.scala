package ordset

/**
 * Typeclass specifying sequence of elements of discrete ordered set. 
 *
 * Implementations must enforce conditions: 
 * <div>
 *   1.a If `x` has successor, then: `predecessor(successor(x)) = x`.
 * </div>
 * <div>
 *   1.b Otherwise `∄` `y` such that: `predecessor(y) = x`.
 * </div>
 * <div>
 *   2.a If `x` has predecessor, then: `successor(predecessor(x)) = x`.
 * </div>
 * <div>
 *   2.b Otherwise `∄` `y` such that: `successor(y) = x`.
 * </div>
 * <div>
 *   3. If `x` is not included in set, then it doesn't have successor and predecessor.
 * </div>
 */
trait Discrete[E] extends Discrete.Succeeding[E] with Discrete.Preceding[E] with Reversible[Discrete[E], Discrete[E]] {

  override def reversed: Discrete[E] = new Discrete.ReversedImpl(this)
}

object Discrete {

  type Maybe[+E] = E | None.type

  /**
   * Returns 'true', if input argument equals to [[Discrete.None]] object.
   */
  def isNone[E](x: Maybe[E]): Boolean = None == x

  /**
   * Returns 'true', if input argument doesn't equal to [[Discrete.None]] object.
   */
  def isPresent[E](x: Maybe[E]): Boolean = None != x

  /**
   * Object representing absence of successor or predecessor.
   */
  object None {

    override def toString(): String = "Discrete.None"
  }

  /** 
   * Typeclass to get succeeding elements of sequence.
   * 
   * See condition 3 of [[Discrete]].
   */
  trait Succeeding[E] extends Reversible[Succeeding[E], Preceding[E]] {

    /**
     * Returns:
     * <div>[[Discrete.None]], if element has no successor;</div>
     * <div>the successor of element otherwise.</div>
     */
    def successorOrNone(x: E): Discrete.Maybe[E]

    /**
     * Returns:
     * <div>[[Option.empty]], if element has no successor;</div>
     * <div>the successor of element otherwise.</div>
     */
    def successorOpt(x: E): Option[E] = successorOrNone(x) match {
      case Discrete.None => Option.empty
      case x: E @unchecked => Some(x)
    }

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

      override def successorOrNone(x: E): Discrete.Maybe[E] = successor(x)

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

      override def successorOrNone(x: E): Discrete.Maybe[E] = reversed.predecessorOrNone(x)

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
     * <div>[[Discrete.None]], if element has no predecessor;</div>
     * <div>the predecessor of element otherwise.</div>
     */
    def predecessorOrNone(x: E): Discrete.Maybe[E]

    /**
     * Returns:
     * <div>[[Option.empty]], if element has no predecessor;</div>
     * <div>the predecessor of element otherwise.</div>
     */
    def predecessorOpt(x: E): Option[E] =  predecessorOrNone(x) match {
      case Discrete.None => Option.empty
      case x: E @unchecked => Some(x)
    }

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

      override def predecessorOrNone(x: E): Discrete.Maybe[E] = predecessor(x)

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

      override def predecessorOrNone(x: E): Discrete.Maybe[E] = reversed.successorOrNone(x)

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

    override def successorOrNone(x: E): Discrete.Maybe[E] = successor(x)

    override def predecessorOrNone(x: E): Discrete.Maybe[E] = predecessor(x)

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