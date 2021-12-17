package ordset

trait Reversible[+O <: Reversible[O, R], +R <: Reversible[R, O]] { self: O =>
  
  def reversed: R
}
