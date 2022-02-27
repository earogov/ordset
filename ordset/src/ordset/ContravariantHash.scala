package ordset

import scala.annotation.unchecked.uncheckedVariance

/**
 * Contravariant version of [[Hash]] typeclass (see [[ContravariantOrder]] for details on contravariance and 
 * [[scala.annotation.unchecked.uncheckedVariance]] annotation).
 */
trait ContravariantHash[-E] extends ContravariantEq[E] with Hash[E @uncheckedVariance] {
  
  override def hash(x: E): Int
}

object ContravariantHash {

  /**
   * Constructs contravariant hash from given `eqv` and `hash` functions.
   */
  def byEqAndHash[E](eqvFunc: (x: E, y: E) => Boolean, hashFunc: (x: E) => Int): ContravariantHash[E] = 
    new ContravariantHash[E] {
      override def eqv(x: E, y: E): Boolean = eqvFunc(x, y)

      override def hash(x: E): Int = hashFunc(x)
    }

  /**
   * Returns contravariant version of given [[Hash]] instance.
   */
  def fromHash[E](hash: Hash[E]): ContravariantHash[E] = 
    hash match {
      case hash: ContravariantHash[E] => hash
      case _ => new ProxyImpl(hash)
    }

  /**
   * [[ContravariantHash]] implementation delegating to another [[Hash]] instance.
   */
  trait Proxy[-E, EE >: E] extends ContravariantHash[E] with ContravariantEq.Proxy[E, EE] { 

    override protected def original: Hash[EE]

    override def  hash(x: E): Int = 
      original.hash(x)
  }

  class ProxyImpl[-E, EE >: E](override val original: Hash[EE]) extends Proxy[E, EE]
}