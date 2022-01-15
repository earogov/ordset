package ordset

/**
 * Allows to get reversed instance for current one. Can be used for typeclasses like [[Bounded]], [[Order]], etc.
 *
 * Reversed type `R` may differ from original type `O` as long as condition is satisfied:
 * <tr>
 *   - if `original` has type `O` and `original.reversed` has type `R`, 
 *     then type of `original.reversed.reversed` is `O`.
 * </tr>
 * 
 * @tparam O original type
 * @tparam R reversed type
 */
trait Reversible[+O <: Reversible[O, R], +R <: Reversible[R, O]] { self: O =>
  
  /**
   * Returns reversed instance for current one.
   */
  def reversed: R
}
