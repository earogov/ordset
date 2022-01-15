package ordset

import ordset.Order
import cats.kernel.Comparison
import scala.annotation.unchecked.uncheckedVariance

/**
 * Contravariant version of [[Order]] typeclass.
 * 
 * In Scala 3 resolution of contravariant implicits has been changed. Consider for instance:
 * {{{
 * 
 *   trait Order[-E]
 *
 *   trait Foo
 *
 *   object Foo {
 *      
 *     implicit def fooOrder: Order[Foo] = 
 *       new Order[Foo] {
 *         override def toString(): String = "fooOrder"
 *       }
 *   }
 *
 *   trait Bar extends Foo
 *
 *   object Bar {
 *      
 *     implicit def barOrder: Order[Bar] = 
 *       new Order[Bar] {
 *         override def toString(): String = "barOrder"
 *       }
 *   }
 *
 *   println(implicitly[Order[Bar]])
 * }}}
 * 
 * Scala 2.13.7 gives compile error: "ambiguous implicit values".
 * 
 * Scala 3.0.0 prints: "barOrder".
 * 
 * For this reason typeclasses like [[Eq]] and [[Order]] were invariant in Scala 2, but since Scala 3 it's possible
 * to make them contravariant. 
 * 
 * [[ContravariantOrder]] extends invariant [[Order]] to simplify interoperability: 
 * <tr>
 *  - Most of methods can require as before invariant [[Order]], so there is no need to migrate all it's subclasses.
 *    In the same time new contravariant instances will also fit.
 * </tr>
 * <tr>
 *  - When contravariance of order is really needed, then invariant instance can be converted into contravariant one
 *    with a wrapper (see [[OrderExtensions]]).
 * </tr>
 * 
 * <h3>Note to unchecked variance</h3>
 * 
 * [[ContravariantOrder]] extends [[Order]] with [[uncheckedVariance]] annotation. Let's prove that it's safe.
 * 
 * If `Bar <: Foo`, then due to contravariance we can cast:
 * {{{
 * 
 *   val fooOrd: ContravariantOrder[Foo] = getFooOrder()
 *   val barOrd: ContravariantOrder[Bar] = fooOrd
 * }}}
 * 
 * This leads to the following requirements:
 * <tr>1. Methods of [[Order]] that receives arguments of type `E`, must also be able to receive subtypes of `E`.</tr>
 * <tr>2. Methods of [[Order]] that produces result of type `E`, must also be able to produce subtypes of `E`.</tr>
 * 
 * Item 1 is trivial, because it must be valid for all well behaved methods.
 * 
 * Item 2 is applied only to 3 methods of [[Order]]: `min`, `max` and `toOrdering` (as they return type `E`).
 * First let's consider `min` and `max`. Current trait provides contravariant implementations for them, since there is
 * no object instantiation inside. When we cast `ContravariantOrder[Foo]` to `ContravariantOrder[Bar]` in our example,
 * methods `min` and `max` will receive input arguments of type `Bar`, so they will also output instances of type `Bar`.
 * Hence, the requirement 2 is satisfied. Same reasoning can be applied to methods of [[Ordering]] instance returned by
 * `toOrdering` method. We have proved that all methods of [[ContravariantOrder]] satisfies contravariance 
 * requirements.
 * 
 * Methods `min`, `max` and `toOrdering` are marked as `final`, so contravariance can't be broken in subtypes.
 */
trait ContravariantOrder[-E] extends ContravariantEq[E] with Order[E @uncheckedVariance] {
  
  override def compare(x: E, y: E): Int

  override def comparison(x: E, y: E): Comparison = Comparison.fromInt(compare(x, y))

  override def partialCompare(x: E, y: E): Double = compare(x, y).toDouble

  override def eqv(x: E, y: E): Boolean =
    compare(x, y) == 0

  override def neqv(x: E, y: E): Boolean =
    compare(x, y) != 0

  override def lteqv(x: E, y: E): Boolean =
    compare(x, y) <= 0

  override def lt(x: E, y: E): Boolean =
    compare(x, y) < 0

  override def gteqv(x: E, y: E): Boolean =
    compare(x, y) >= 0

  override def gt(x: E, y: E): Boolean =
    compare(x, y) > 0

  final override def min(x: E, y: E): E @uncheckedVariance = 
    if (lt(x, y)) x else y

  final override def max(x: E, y: E): E @uncheckedVariance = 
    if (gt(x, y)) x else y

  final override def toOrdering: Ordering[E @uncheckedVariance] =
    Ordering.fromLessThan(lt)
}

object ContravariantOrder {

  /**
   * Constructs contravariant order from given `compare` function.
   */
  def byCompare[E](cmpFunc: (x: E, y: E) => Int): ContravariantOrder[E] = 
    new ContravariantOrder[E] {
      override def compare(x: E, y: E): Int = cmpFunc(x, y)
    }

  /**
   * Returns contravariant version of given [[Order]] instance.
   */
  def fromOrder[E](ord: Order[E]): ContravariantOrder[E] = 
    ord match {
      case ord: ContravariantOrder[E] => ord
      case _ => new ProxyImpl(ord)
    }

  /**
   * [[ContravariantOrder]] implementation delegating to another [[Order]] instance.
   */
  trait Proxy[-E, EE >: E] extends ContravariantOrder[E] with ContravariantEq.Proxy[E, EE] { 

    override protected val original: Order[EE]

    override def compare(x: E, y: E): Int = 
      original.compare(y, x)

    override def comparison(x: E, y: E): Comparison = 
      original.comparison(x, y)

    override def partialCompare(x: E, y: E): Double =  
      original.partialCompare(x, y)

    override def lteqv(x: E, y: E): Boolean = 
      original.lteqv(x, y)

    override def lt(x: E, y: E): Boolean = 
      original.lt(x, y)

    override def gteqv(x: E, y: E): Boolean = 
      original.gteqv(x, y)

    override def gt(x: E, y: E): Boolean = 
      original.gt(x, y)
  }

  class ProxyImpl[-E, EE >: E](override val original: Order[EE]) extends Proxy[E, EE]
}