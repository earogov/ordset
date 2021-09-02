package test.ordset.core.examples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{TreapOrderedMap, UniformOrderedMap}
import ordset.core.set.{TreapOrderedSet, UniformOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.value.ValueOps
import ordset.core.*
import ordset.random.RngManager
import test.ordset.core.implementations.domain.BoundSelector

import scala.language.postfixOps

object MapExample {

  import ordset.core.instances.boolean.*
  import ordset.core.instances.int.*
  import test.ordset.core.TestRngUtil.Implicits.*

  private val sep = "-----------------"

  private val intValueOps: ValueOps[Int] = implicitly[ValueOps[Int]]
  private val booleanValueOps: ValueOps[Boolean] = implicitly[ValueOps[Boolean]]
  private val domainOps: DomainOps[Int, Domain[Int]] = implicitly[DomainOps[Int, Domain[Int]]]

  @main
  def mapExampleMain(): Unit = {
    example1()
    example2()
  }

  def example1(): Unit = {
    println(s"$sep SegmentSeq.mapSegments example $sep")

    val seq1 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (0`)[`, false),
        (10`)[`, true),
        (20`)[`, false),
        (30`)[`, true),
        (40`)[`, false),
        (AboveAll, true)
      ),
      domainOps,
      booleanValueOps
    )()

    println("Initial sequence:")
    println(seq1)

    val bound1 = Bound.Upper.exclusive(20)
    println(s"Map all segments below $bound1 to `true` and above - to `false`")
    val seq2 = seq1.mapSegments(s => s.domainOps.extendedOrd.lteqv(s.upperExtended, bound1))(booleanValueOps)

    println("Received sequence:")
    println(seq2)
  }

  def example2(): Unit = {
    println(s"$sep SegmentSeq.map example $sep")

    val seq1 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (0`)[`, -20),
        (10`)[`, -5),
        (20`)[`, 10),
        (30`)[`, -7),
        (40`)[`, 15),
        (AboveAll, 20)
      ),
      domainOps,
      intValueOps
    )()

    println("Initial sequence:")
    println(seq1)

    println(s"Map all positive or zero values to `true` and negative to `false`")
    val seq2 = seq1.map(_ >= 0)(booleanValueOps)

    println("Received sequence:")
    println(seq2)
  }
}
