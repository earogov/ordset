package ordset.test.core.examples.segmentSeq

import ordset.Eq
import ordset.core.domain.{DomainOps, Domain}
import ordset.core.value.ValueOps
import ordset.core.segmentSeq.set.OrderedSet
import ordset.core.interval.IntervalRelation
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import scala.language.postfixOps

object OrderedSetExample {

  import ordset.givens.bigInt.*
  import ordset.givens.string.*
  import ordset.givens.iterable.*
  import ordset.implementations.bigInt
  import ordset.test.core.TestRngUtil.Givens.*

  type Dom[X] = Domain.DiscreteUnbounded[X]

  private val sep = "-----------------"

  private implicit val domainOps: DomainOps[BigInt, Dom] = DomainOps.UnboundedOps.default[BigInt, Dom]

  private val x: BoundBuilder[BigInt, Dom] = BoundBuilder[BigInt, Dom]

  private val eq: Eq[Iterable[IntervalRelation[BigInt, Dom, Boolean]]] =
    iterableEq(domainOps.intervalRelations.hash(ValueOps.booleanValueOps.valueHash))
  
  def main(args: Array[String]): Unit = {
    example1()
    example2()
  }

  def example1(): Unit = {

    println()
    println(s"$sep Example 1 $sep")

    val a = OrderedSet.Try(
      x <= 10,
      x >= 20 & x <= 30,
      x >= 40
    ).get

    println("")
    println(s"a = $a")

    val b = ~a

    println("")
    println(s"b = ~a = $b")

    val c = ~b

    println("")
    println(s"c = ~(~a) = $c")

    val isEqual = eq.eqv(a.intervalRelations, c.intervalRelations)

    println("")
    println(s"a == c is $isEqual")
  }

  def example2(): Unit = {

    println()
    println(s"$sep Example 2 $sep")

    val a = OrderedSet.Try(
      x <= 10,
      x >= 20 & x <= 30,
      x >= 40
    ).get

    println("")
    println(s"a = $a")

    val b = OrderedSet.Try(
      x <= 5,
      x >= 15 & x <= 25,
      x >= 35 
    ).get

    println("")
    println(s"b = $b")

    val c = ~(a | b)

    println("")
    println(s"c = ~(a | b) = $c")

    val d = ~a & ~b

    println("")
    println(s"d = ~a & ~b = $d")

    val isEqual = eq.eqv(c.intervalRelations, d.intervalRelations)

    println("")
    println(s"c == d is $isEqual")
  }
}
