package test.ordset.core.examples.segmentSeq

import ordset.core.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{TreapOrderedMap, UniformOrderedMap}
import ordset.core.set.{TreapOrderedSet, UniformOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.value.ValueOps
import ordset.random.RngManager
import test.ordset.core.examples.segmentSeq.PatchLazyExample.{domainOps, stringValueOps}
import test.ordset.core.implementations.domain.BoundSelector

import scala.language.postfixOps

object ZipExample {

  import ordset.core.instances.boolean.*
  import ordset.core.instances.int.*
  import test.ordset.core.TestRngUtil.Implicits.*

  private val sep = "-----------------"
  
  private val stringValueOps: ValueOps[String] = implicitly[ValueOps[String]]
  private val domainOps: DomainOps[Int, Domain[Int]] = implicitly[DomainOps[Int, Domain[Int]]]

  @main
  def zipExampleMain(): Unit = {
    example1()
    example2()
    example3()
  }

  def example1(): Unit = {
    println(s"$sep SegmentSeq.zipIntoTuple example $sep")

    println("First initial sequence:")
    val seq1 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (0`)[`, "A"),
        (10`)[`, "B"),
        (AboveAll, "C")
      ),
      domainOps,
      stringValueOps
    )()
    println(seq1)

    println("Second initial sequence:")
    val seq2 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (5`)[`, "1"),
        (AboveAll, "2")
      ),
      domainOps,
      stringValueOps
    )()
    println(seq2)

    println("Zip sequences into tuple:")
    val zippedSeq = seq1.zipIntoTuple(seq2)
    println(zippedSeq)
  }

  def example2(): Unit = {
    println(s"$sep SegmentSeq.zip example $sep")

    println("First initial sequence:")
    val seq1 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (0`)[`, "A"),
        (10`)[`, "B"),
        (AboveAll, "C")
      ),
      domainOps,
      stringValueOps
    )()
    println(seq1)

    println("Second initial sequence:")
    val seq2 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (5`)[`, "C"),
        (15`)[`, "B"),
        (AboveAll, "A")
      ),
      domainOps,
      stringValueOps
    )()
    println(seq2)

    println("Zip sequences with concatenation operator:")
    val zippedSeq = seq1.zip(seq2, (v: String, u: String) => v + u)
    println(zippedSeq)
  }

  def example3(): Unit = {
    println(s"$sep Truncation.zip example $sep")

    println("First initial sequence:")
    val seq1 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      List(0`)`, 10`)`),
      complementary = true,
      domainOps
    )()
    println(seq1)

    println("Second initial sequence:")
    val seq2 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      List(5`)[`, 15`)[`),
      complementary = true,
      domainOps,
    )()
    println(seq2)

    println("Let's get truncation of first sequence at the lowest bound:")
    val firstTruncation = seq1.firstSegment.lowerTruncation
    println(firstTruncation)

    println("")
    println("Zip this truncation with second sequence using `or` operator.")
    println("In the result zipped sequence is built and its truncation at the lowest bound is returned:")
    val zippedTruncation = firstTruncation.zip(seq2, (v: Boolean, u: Boolean) => v || u)
    println(zippedTruncation)

    println("")
    println("Also we have access to the whole zipped sequence:")
    println(zippedTruncation.sequence)
  }
}