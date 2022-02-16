package ordset.core.segmentSeq.validation

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
     * 
     * @param x validated element.
     */
    override def apply(x: E): Boolean

    /**
     * Same as [[apply]] but throws exception if validation is failed. 
     * The result must be consistent with [[apply]] apply.
     * 
     * @param x validated element.
     * @param index index of validated element. It must not affect on validation result, and should be used only
     *              in error message for debug purposes.
     */
    @throws[ValidationException]("if validation is failed")
    def validate(x: E, index: Long): Unit = 
      if !apply(x) 
      then throw ValidationException.invalidObject(x.toString, index)
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

          override def validate(x: E, index: Long): Unit = {
            self.validate(x, index)
            other.validate(x, index)
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
     * 
     * @param prev preceding validated element.
     * @param next succeeding validated element.
     */
    override def apply(prev: E, next: E): Boolean

    /**
     * Same as [[apply]] but throws exception if validation is failed.
     * The result must be consistent with [[apply]] apply.
     * 
     * @param prev preceding validated element.
     * @param next succeeding validated element.
     * @param index index of succeeding validated element. It must not affect on validation result, and should be
     *              used only in error message for debug purposes.
     */
    @throws[IllegalArgumentException]("if validation is failed")
    def validate(prev: E, next: E, index: Long): Unit =
      if !apply(prev, next)
      then throw throw ValidationException.invalidObjectsSeq(prev.toString, next.toString, index)
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

          override def validate(prev: E, next: E, index: Long): Unit = {
            self.validate(prev, next, index)
            other.validate(prev, next, index)
          }
        }
    }
  }
}
