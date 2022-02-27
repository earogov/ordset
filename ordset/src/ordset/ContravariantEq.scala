package ordset

import scala.annotation.unchecked.uncheckedVariance

/**
 * Contravariant version of [[Eq]] typeclass (see [[ContravariantOrder]] for details on contravariance and 
 * [[scala.annotation.unchecked.uncheckedVariance]] annotation).
 */
trait ContravariantEq[-E] extends Eq[E @uncheckedVariance] {
  
  override def eqv(x: E, y: E): Boolean

  override def neqv(x: E, y: E): Boolean = 
    !eqv(x, y)
}

object ContravariantEq {

  /**
   * Constructs contravariant equality from given `eqv` function.
   */
  def byEqv[E](eqvFunc: (x: E, y: E) => Boolean): ContravariantEq[E] = 
    new ContravariantEq[E] {
      override def eqv(x: E, y: E): Boolean = eqvFunc(x, y)
    }

  /**
   * Returns contravariant version of given [[Eq]] instance.
   */
  def fromEq[E](eq: Eq[E]): ContravariantEq[E] = 
    eq match {
      case eq: ContravariantEq[E] => eq
      case _ => new ProxyImpl(eq)
    }

  /**
   * [[ContravariantEq]] implementation delegating to another [[Eq]] instance.
   */
  trait Proxy[-E, EE >: E] extends ContravariantEq[E] { 

    protected def original: Eq[EE]

    override def eqv(x: E, y: E): Boolean =
      original.eqv(x, y)

    override def neqv(x: E, y: E): Boolean = 
      original.neqv(x, y)
  }

  class ProxyImpl[-E, EE >: E](override val original: Eq[EE]) extends Proxy[E, EE]
}