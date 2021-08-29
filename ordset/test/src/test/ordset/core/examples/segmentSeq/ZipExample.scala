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

  private val intValueOps: ValueOps[Int] = implicitly[ValueOps[Int]]
  private val stringValueOps: ValueOps[String] = implicitly[ValueOps[String]]
  private val domainOps: DomainOps[Int, Domain[Int]] = implicitly[DomainOps[Int, Domain[Int]]]

  @main
  def zipExampleMain(): Unit = {
    example1()
    example2()
  }

  def example1(): Unit = {
    println(s"$sep SegmentSeqT.zipToTuple example $sep")

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
    val zippedSeq = seq1.zipToTuple(seq2)
    println(zippedSeq)
  }

  def example2(): Unit = {
    println(s"$sep SegmentSeqT.zip example $sep")

    println("First initial sequence:")
    val seq1 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (0`)[`, 0),
        (10`)[`, 1),
        (AboveAll, 2)
      ),
      domainOps,
      intValueOps
    )()
    println(seq1)

    println("Second initial sequence:")
    val seq2 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (5`)[`, 2),
        (15`)[`, 1),
        (AboveAll, 0)
      ),
      domainOps,
      intValueOps
    )()
    println(seq2)

    println("Zip sequences with modulo addition operator (v + u) % 3:")
    val zippedSeq = seq1.zip(seq2, (v: Int, u: Int) => (v + u) % 3)
    println(zippedSeq)
  }
}
