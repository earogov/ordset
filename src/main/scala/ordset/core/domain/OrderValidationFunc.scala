package ordset.core.domain


trait OrderValidationFunc[-E] extends ((E, E) => Boolean) {

  /**
   * @return `true` if two given elements follow according to required order.
   */
  override def apply(prev: E, next: E): Boolean

  /**
   * Same as [[apply]] but throws exception if validation fails.
   */
  @throws[IllegalArgumentException]("if validation is failed")
  def validate(prev: E, next: E): Unit =
    if (!apply(prev, next)) throw new IllegalArgumentException(s"Illegal elements order: $prev >= $next")
}

object OrderValidationFunc {

  /**
   * @return `true` if all `elements` follow according to required order:
   *
   *         apply(elements^i-1^, elements^i^) == true for each i in [1, elements.size]
   */
  def applyToIterable[E](elements: IterableOnce[E], func: OrderValidationFunc[E]): Boolean = {
    var isValid = true
    val iterator = elements.iterator
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
   * Same as [[applyToIterable]] but throws exception if validation fails.
   */
  @throws[IllegalArgumentException]("if validation is failed")
  def validateIterable[E](elements: IterableOnce[E], func: OrderValidationFunc[E]): Unit = {
    val iterator = elements.iterator
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
   * Note `op` is always applied to the first element.
   */
  @throws[IllegalArgumentException]("if validation is failed")
  def foldIterableBefore[E, R](elements: IterableOnce[E], func: OrderValidationFunc[E], init: R, op: (R, E) => R): R = {
    var result = init
    val iterator = elements.iterator
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
   * Note `op` is always applied to the first element.
   */
  @throws[IllegalArgumentException]("if validation is failed")
  def foldIterableAfter[E, R](elements: IterableOnce[E], func: OrderValidationFunc[E], init: R, op: (R, E) => R): R = {
    var result = init
    val iterator = elements.iterator
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
