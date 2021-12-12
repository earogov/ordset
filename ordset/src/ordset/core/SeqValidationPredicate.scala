package ordset.core

/**
 * Functional trait that allows to validate two adjacent elements of sequence.
 */
@FunctionalInterface
trait SeqValidationPredicate[-E] extends ((E, E) => Boolean) {

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
    if (!apply(prev, next))
      throw new IllegalArgumentException(s"Validation of adjacent elements of sequence failed: $prev, $next")
}

object SeqValidationPredicate {

  /**
   * Get predicate that always returns `true`.
   */
  lazy val alwaysTrue: SeqValidationPredicate[Any] = (_, _) => true

  /**
   * Get predicate that always returns `false`.
   */
  lazy val alwaysFalse: SeqValidationPredicate[Any] = (_, _) => false

  /**
   * Returns `true` if all adjacent elements of sequence satisfy validation condition, i.e.
   *
   * apply(seq,,i-1,,, seq,,i,,) == true for each i in [1, seq.size]
   */
  def applyToIterable[E](seq: IterableOnce[E], func: SeqValidationPredicate[E]): Boolean = {
    var isValid = true
    val iterator = seq.iterator
    if (iterator.hasNext) {
      var prev = iterator.next()
      if (iterator.hasNext) {
        var next = iterator.next()
        isValid = func(prev, next)
        while (iterator.hasNext && isValid) {
          prev = next
          next = iterator.next()
          isValid = func(prev, next)
        }
      }
    }
    isValid
  }

  /**
   * Same as [[applyToIterable]] but throws exception if validation is failed.
   */
  @throws[IllegalArgumentException]("if validation is failed")
  def validateIterable[E](seq: IterableOnce[E], func: SeqValidationPredicate[E]): Unit = {
    val iterator = seq.iterator
    if (iterator.hasNext) {
      var prev = iterator.next()
      if (iterator.hasNext) {
        var next = iterator.next()
        func.validate(prev, next)
        while (iterator.hasNext) {
          prev = next
          next = iterator.next()
          func.validate(prev, next)
        }
      }
    }
  }

  /**
   * Same as [[validateIterable]] but applies function `op` <u>before</u> each validation.
   *
   * Note, `op` is always applied to the first element.
   */
  @throws[IllegalArgumentException]("if validation is failed")
  def foldIterableBefore[E, R](
    seq: IterableOnce[E],
    func: SeqValidationPredicate[E],
    init: R,
    op: (R, E) => R
  ): R = {
    var result = init
    val iterator = seq.iterator
    if (iterator.hasNext) {
      var prev = iterator.next()
      result = op(result, prev)
      if (iterator.hasNext) {
        var next = iterator.next()
        result = op(result, next)
        func.validate(prev, next)
        while (iterator.hasNext) {
          prev = next
          next = iterator.next()
          result = op(result, next)
          func.validate(prev, next)
        }
      }
    }
    result
  }

  /**
   * Same as [[validateIterable]] but applies function `op` <u>after</u> each validation.
   *
   * Note, `op` is always applied to the first element.
   */
  @throws[IllegalArgumentException]("if validation is failed")
  def foldIterableAfter[E, R](
    seq: IterableOnce[E],
    func: SeqValidationPredicate[E],
    init: R,
    op: (R, E) => R
  ): R = {
    var result = init
    val iterator = seq.iterator
    if (iterator.hasNext) {
      var prev = iterator.next()
      result = op(result, prev)
      if (iterator.hasNext) {
        var next = iterator.next()
        func.validate(prev, next)
        result = op(result, next)
        while (iterator.hasNext) {
          prev = next
          next = iterator.next()
          func.validate(prev, next)
          result = op(result, next)
        }
      }
    }
    result
  }
}
