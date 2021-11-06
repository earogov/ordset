package ordset.core.domain.entity

import ordset.core.domain.{OrderDir, OrderDirection}

/**
 * Base trait for abstract entities with direction like order, etc.
 *
 * Direction is defined both with type parameter ([[DirectedEntity!.Dir]]) and value ([[DirectedEntity!.direction]]).
 *
 * @tparam Dir direction type parameter
 */
trait DirectedEntity[+Dir <: OrderDir] {

  /**
   * Value representation of Dir type parameter.
   */
  def direction: Dir

  /**
   * Returns `true` if `direction` is ascending.
   */
  final def isAscending: Boolean = OrderDirection.isAscending(direction)

  /**
   * Returns `true` if `direction` is descending.
   */
  final def isDescending: Boolean = OrderDirection.isDescending(direction)

  /**
   * Sign equals to
   * {{{
   *   1 if (isAscending == true)
   *   -1 if (isAscending == false)
   * }}}
   */
  final def sign: Int = if (isAscending) 1 else -1

  /**
   * Inverted sign equals to
   * {{{
   *   -1 if (isAscending == true)
   *   1 if (isAscending == false)
   * }}}
   */
  final def invertedSign: Int = if (isAscending) -1 else 1
}