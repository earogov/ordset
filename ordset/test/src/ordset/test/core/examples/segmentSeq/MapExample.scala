package ordset.test.core.examples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{TreapOrderedMap, UniformOrderedMap}
import ordset.core.set.{TreapOrderedSet, UniformOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.value.ValueOps
import ordset.core.*
import ordset.random.RngManager
import ordset.test.core.implementations.domain.BoundSelector

import scala.language.postfixOps

object MapExample {

  import ordset.instances.boolean.*
  import ordset.instances.int.*
  import ordset.test.core.TestRngUtil.Implicits.*

  private val sep = "-----------------"

  private val intValueOps: ValueOps[Int] = implicitly
  private val booleanValueOps: ValueOps[Boolean] = implicitly
  private val domainOps: DomainOps[Int, Domain.ContinuousUnbounded[Int]] = implicitly

  @main
  def mapExampleMain(): Unit = {
    example1()
    example2()
  }

  def example1(): Unit = {
    println()
    println(s"$sep SegmentSeq.mapSegments example $sep")

    println("Initial sequence:")
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
    println(seq1)

    val bound1 = Bound.Upper.excluding(20)
    
    println()
    println(s"Map all segments below $bound1 to `true` and above - to `false`.")
    val mapFunc = (s: Segment[Int, _ <: Domain[Int], Boolean]) => s.domainOps.extendedOrd.lteqv(s.upperExtended, bound1)
    val seq2 = seq1.mapSegments(mapFunc)(booleanValueOps)

    println("Received sequence:")
    println(seq2)

    println()
    println("We can also apply mapping to some segment and receive corresponding mapped segment:")
    val mappedSegment = seq1.getSegmentForElement(15).mapSegments(mapFunc)(booleanValueOps)
    println(mappedSegment)

    println()
    println("Or we can apply mapping to truncation and get mapped truncation:")
    val mappedTruncation = seq1.getSegmentForBound(15`]`).truncation(15`]`).mapSegments(mapFunc)(booleanValueOps)
    println(mappedTruncation)
  }

  def example2(): Unit = {
    println()
    println(s"$sep SegmentSeq.map example $sep")

    println("Initial sequence:")
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
    println(seq1)

    println()
    println(s"Map all positive or zero values to `true` and negative to `false`")
    val seq2 = seq1.map(_ >= 0)(booleanValueOps)

    println("Received sequence:")
    println(seq2)
  }
}
