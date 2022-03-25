package ordset.test.core.examples.segmentSeq

import ordset.core.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.*
import ordset.core.segmentSeq.map.{TreapOrderedMap, UniformOrderedMap, OrderedMap}
import ordset.core.segmentSeq.set.{TreapOrderedSet, UniformOrderedSet, OrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.test.core.implementations.domain.BoundSelector

import scala.language.postfixOps

object MapExample {

  import ordset.givens.boolean.*
  import ordset.givens.int.*
  import ordset.test.core.TestRngUtil.Givens.*

  type Dom[X] = Domain.ContinuousUnbounded[X]

  private val sep = "-----------------"

  private implicit val intOps: ValueOps[Int] = ValueOps.intValueOps
  private implicit val booleanOps: ValueOps[Boolean] = ValueOps.booleanValueOps
  private implicit val domainOps: DomainOps[Int, Dom] = DomainOps.default

  private val x: BoundBuilder[Int, Dom] = BoundBuilder.apply

  def main(args: Array[String]): Unit = {
    example1()
    example2()
  }

  def example1(): Unit = {
    println()
    println(s"$sep SegmentSeq.mapSegments example $sep")

    println("Initial sequence:")
    val seq1 = 
      OrderedSet.tryBuild(
        List(
          x >= 0 & x < 10,
          x >= 20 & x < 30,
          x >= 40
        )
      ).get
    println(seq1)

    val bound1 = Bound.Upper.excluding(20)
    
    println()
    println(s"Map all segments below $bound1 to `true` and above - to `false`.")
    val mapFunc = (s: Segment[Int, _ <: Domain, Boolean]) => s.domainOps.extendedOrd.lteqv(s.upper, bound1)
    val seq2 = seq1.mapSegments(mapFunc)

    println("Received sequence:")
    println(seq2)

    println()
    println("We can also apply mapping to some segment and receive corresponding mapped segment:")
    val mappedSegment = seq1.getSegmentForElement(15).mapSegments(mapFunc)
    println(mappedSegment)

    println()
    println("Or we can apply mapping to truncation and get mapped truncation:")
    val mappedTruncation = seq1.getSegmentForBound(15`]`).truncation(15`]`).mapSegments(mapFunc)
    println(mappedTruncation)
  }

  def example2(): Unit = {
    println()
    println(s"$sep SegmentSeq.map example $sep")

    println("Initial sequence:")
    val seq1  = 
      OrderedMap.tryBuild(
        0,
        List(
          -20 forAll x < 0,
           -5 forAll x >= 0 & x < 10,
           10 forAll x >= 10 & x < 20,
           -7 forAll x >= 20 & x < 30,
           15 forAll x >= 30 & x < 40,
           20 forAll x >= 40
        )
      ).get
    println(seq1)

    println()
    println(s"Map all positive or zero values to `true` and negative to `false`")
    val seq2 = seq1.map(_ >= 0)

    println("Received sequence:")
    println(seq2)
  }
}
