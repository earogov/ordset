package ordset.core.validation

/**
 * Base validation trait.
 */
sealed trait ValidationPredicate[-E]

object ValidationPredicate {

  /**
   * Functional trait that allows to validate single element.
   */
  @FunctionalInterface
  trait Arity1[-E] extends ValidationPredicate[E] with ((E) => Boolean) {

    /**
     * Returns `true` if element satisfies validation condition.
     */
    override def apply(x: E): Boolean

    /**
     * Same as [[apply]] but throws exception if validation is failed.
     */
    @throws[ValidationException]("if validation is failed")
    def validate(x: E): Unit = 
      if !apply(x) 
      then throw ValidationException.invalidObject(x.toString)
  }

  object Arity1 {

    /**
     * Predicate that always returns `true`.
     */
    lazy val alwaysTrue: Arity1[Any] = _ => true

    /**
     * Predicate that always returns `false`.
     */
    lazy val alwaysFalse: Arity1[Any] = _ => false

    extension [E] (self: Arity1[E]) {

      /**
       * Returns predicate which performs validation of initial predicate and specified `other` predicate. 
       */
      def and(other: Arity1[E]): Arity1[E] = 
        new Arity1[E] {
          override def apply(x: E): Boolean = 
            self.apply(x) && other.apply(x)

          override def validate(x: E): Unit = {
            self.validate(x)
            other.validate(x)
          }
        }
    }
  }

  /**
   * Functional trait that allows to validate two adjacent elements of sequence.
   */
  @FunctionalInterface
  trait Arity2[-E] extends ValidationPredicate[E] with ((E, E) => Boolean) { self =>

    /**
     * Returns `true` if two given elements satisfy validation condition.
     * Order of arguments corresponds to order in validated sequence.
     */
    override def apply(prev: E, next: E): Boolean

    /**
     * Same as [[apply]] but throws exception if validation is failed.
     */
    @throws[IllegalArgumentException]("if validation is failed")
    def validate(prev: E, next: E): Unit =
      if !apply(prev, next)
      then throw throw ValidationException.invalidObjectsSeq(prev.toString, next.toString)
  }

  object Arity2 {

    /**
     * Predicate that always returns `true`.
     */
    lazy val alwaysTrue: Arity2[Any] = (_, _) => true

    /**
     * Predicate that always returns `false`.
     */
    lazy val alwaysFalse: Arity2[Any] = (_, _) => false

    extension [E] (self: Arity2[E]) {

      /**
       * Returns predicate which performs validation of initial predicate and specified `other` predicate. 
       */
      def and(other: Arity2[E]): Arity2[E] = 
        new Arity2[E] {
          override def apply(prev: E, next: E): Boolean = 
            self.apply(prev, next) && other.apply(prev, next)

          override def validate(prev: E, next: E): Unit = {
            self.validate(prev, next)
            other.validate(prev, next)
          }
        }
    }
  }
}
