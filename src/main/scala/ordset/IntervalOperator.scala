package ordset

trait IntervalOperator[E, +D <: Domain[E]] {

  def domain: D

  def interpret(x: Interval[E]): Interval[E]

  def cross(x: Interval[E], y: Interval[E]): Interval[E]
}
