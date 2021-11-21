package ordset

/**
 * Entity with direction property. Direction can be either ascending or descending.
 * It is defined both with type parameter ([[Directed!.Dir]]) and value ([[Directed!.direction]]).
 * 
 * Trait should be combined with typeclasses like [[Order]], [[Discrete]], etc to distinguish instances
 * with different directions.
 * 
 * @tparam Dir direction type parameter
 */ 
trait Directed[+Dir <: Direction] {

  /**
   * Value representation of Dir type parameter.
   */
  def direction: Dir

  /**
   * Returns `true` if `direction` is ascending.
   */
  final def isAscending: Boolean = Direction.isAscending(direction)

  /**
   * Returns `true` if `direction` is descending.
   */
  final def isDescending: Boolean = Direction.isDescending(direction)

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

object Directed {

  /** Typeclass which adds ascending direction. */
  trait Ascending extends Directed[Asc] {

    override val direction: Asc = Direction.Asc.value
  }

  /** Typeclass which adds descending direction. */
  trait Descending extends Directed[Desc] {

    override val direction: Desc = Direction.Desc.value
  }
}