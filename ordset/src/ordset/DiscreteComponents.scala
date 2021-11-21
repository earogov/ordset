package ordset

object DiscreteComponents {

  trait SucceedingProxy[E] extends Discrete.Succeeding[E] {

    protected val original: Discrete.Succeeding[E]

    override def successorOrNull(x: E): E | Null = original.successorOrNull(x)

    override def successorOpt(x: E): Option[E] = original.successorOpt(x)

    override def hasSuccessor(x: E): Boolean = original.hasSuccessor(x)
  }

  trait SucceedingInfiniteProxy[E] 
    extends Discrete.Succeeding.Infinite[E] 
    with SucceedingProxy[E] {

    override protected val original: Discrete.Succeeding.Infinite[E]

    override def successor(x: E): E = original.successor(x)
  }

  trait PrecedingProxy[E] extends Discrete.Preceding[E] {

    protected val original: Discrete.Preceding[E]

    override def predecessorOrNull(x: E): E | Null = original.predecessorOrNull(x)

    override def predecessorOpt(x: E): Option[E] = original.predecessorOpt(x)

    override def hasPredecessor(x: E): Boolean = original.hasPredecessor(x)
  }

  trait PrecedingInfiniteProxy[E] 
    extends Discrete.Preceding.Infinite[E] 
    with PrecedingProxy[E] {

    override protected val original: Discrete.Preceding.Infinite[E]

    override def predecessor(x: E): E = original.predecessor(x)
  }

  trait DiscreteProxy[E] 
    extends Discrete[E] 
    with SucceedingProxy[E] 
    with PrecedingProxy[E] {

    override protected val original: Discrete[E]
  }

  trait DiscreteInfiniteProxy[E] 
    extends Discrete.Infinite[E] 
    with SucceedingInfiniteProxy[E] 
    with PrecedingInfiniteProxy[E] {

    override protected val original: Discrete.Infinite[E]
  }

  object Reversed {

    trait SucceedingProxy[E] extends Discrete.Succeeding[E] {

      protected val original: Discrete.Preceding[E]

      override def successorOrNull(x: E): E | Null = original.predecessorOrNull(x)

      override def successorOpt(x: E): Option[E] = original.predecessorOpt(x)

      override def hasSuccessor(x: E): Boolean = original.hasPredecessor(x)
    }

    trait SucceedingInfiniteProxy[E] 
      extends Discrete.Succeeding.Infinite[E] 
      with SucceedingProxy[E] {

      override protected val original: Discrete.Preceding.Infinite[E]

      override def successor(x: E): E = original.predecessor(x)
    }

    trait PrecedingProxy[E] extends Discrete.Preceding[E] {

      protected val original: Discrete.Succeeding[E]

      override def predecessorOrNull(x: E): E | Null = original.successorOrNull(x)

      override def predecessorOpt(x: E): Option[E] = original.successorOpt(x)

      override def hasPredecessor(x: E): Boolean = original.hasSuccessor(x)
    }

    trait PrecedingInfiniteProxy[E] 
      extends Discrete.Preceding.Infinite[E] 
      with PrecedingProxy[E] {

      override protected val original: Discrete.Succeeding.Infinite[E]

      override def predecessor(x: E): E = original.successor(x)
    }

    trait DiscreteProxy[E] 
      extends Discrete[E] 
      with SucceedingProxy[E] 
      with PrecedingProxy[E] {

      override protected val original: Discrete[E]
    }

    trait DiscreteInfiniteProxy[E] 
      extends Discrete.Infinite[E] 
      with SucceedingInfiniteProxy[E] 
      with PrecedingInfiniteProxy[E] {

      override protected val original: Discrete.Infinite[E]
    }
  }

  object Numeric {

    trait Succeeding[E] extends Discrete.Succeeding[E] {

      def num: Numeric[E]

      override def successorOrNull(x: E): E | Null = if hasSuccessor(x) then num.plus(x, num.one) else null
    }

    trait SucceedingInfinite[E] extends Discrete.Succeeding.Infinite[E] {

      def num: Numeric[E]

      override def successor(x: E): E = num.plus(x, num.one)
    }

    trait Preceding[E] extends Discrete.Preceding[E] {

      def num: Numeric[E]

      override def predecessorOrNull(x: E): E | Null = if hasPredecessor(x) then num.minus(x, num.one) else null
    }

    trait PrecedingInfinite[E] extends Discrete.Preceding.Infinite[E] {

      def num: Numeric[E]

      override def predecessor(x: E): E = num.minus(x, num.one)
    }

    trait Discrete[E] extends ordset.Discrete[E] with Succeeding[E] with Preceding[E]

    trait DiscreteInfinite[E] extends Discrete.Infinite[E] with SucceedingInfinite[E] with PrecedingInfinite[E]
  } 
}