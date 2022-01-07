package ordset.test.core.examples.segmentSeq

import ordset.core.domain.{DomainOps, Domain}
import ordset.core.value.ValueOps
import ordset.core.map.{TreapOrderedMap, UniformOrderedMap}
import ordset.core.set.{TreapOrderedSet, UniformOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.{ExtendedBound, Bound, Segment, SegmentSeq, SeqValidationPredicate}
import ordset.random.RngManager
import ordset.test.core.implementations.domain.BoundSelector

import scala.language.postfixOps

object FlatmapExample {

  import ordset.givens.boolean.*
  import ordset.givens.string.*
  import ordset.givens.int.*
  import ordset.test.core.TestRngUtil.Implicits.*

  private val sep = "-----------------"

  private val booleanOps: ValueOps[Boolean] = implicitly
  private val stringOps: ValueOps[String] = implicitly
  private val domainOps: DomainOps[Int, Domain.ContinuousUnbounded[Int]] = implicitly

  def main(args: Array[String]): Unit = {
    example1()
    example2()
    example3()
    example4()
  }

  def example1(): Unit = {
    println()
    println(s"$sep Segment.flatMap example $sep")

    println("Initial sequence:")
    val seq1 = TreapOrderedSet.getFactory.unsafeBuildAsc(
      List(0`)[`, 10`)[`, 20`)[`, 30`)[`, 40`)[`),
      complementary = false,
      domainOps
    )()
    println(seq1)

    val segment1 = seq1.getSegmentForBound(15`[`)

    println()
    println(s"Apply flatMap to segment $segment1")

    val seq2 = segment1.flatMap { () =>
      TreapOrderedSet.getFactory.unsafeBuildAsc(
        List(12`)`, 15`]`, 17`)`),
        complementary = false,
        seq1.domainOps
      )(
      )(
        seq1.rngManager
      )
    }

    println("Received lazy sequence:")
    println(seq2)

    println()
    println("Compute all lazy values:")
    println(TreapOrderedSet.getFactory.convertSet(seq2))
  }

  def example2(): Unit = {
    println()
    println(s"$sep SegmentSeq.flatMapSegments example $sep")

    /**
     * Splits (if possible) input `segment` into two with random different values:
     * {{{
     *
     *   ------)[-----------)[-------
     *
     *              VVV
     *
     *   ------)[-----)[----)[-------
     *           true   false
     * }}}
     */
    def randomSplit[E, D <: Domain[E], V](
      boundSelector: BoundSelector[E],
      valuesGenerator: () => (V, V)
    )(
      segment: Segment[E, D, V]
    ): SegmentSeq[E, D, V] = {
      val values = valuesGenerator()
      val boundOrd = segment.domainOps.extendedOrd
      implicit val rngManager = segment.rngManager

      val midBoundOpt = boundSelector
        .between(segment.lower, segment.upper)(boundOrd)
        .map(_.provideUpper)
        .filter(segment.containsExtended(_))

      midBoundOpt
        .map(b =>
          TreapOrderedMap.getFactory.unsafeBuildSingleBounded(
            b, values._1, values._2, segment.domainOps, segment.valueOps
          )()
        )
        .getOrElse(
          TreapOrderedMap.getFactory.buildUniform(
            values._1, segment.domainOps, segment.valueOps
          )
        )
    }

    def booleanGenerator(rngManager: RngManager): () => (Boolean, Boolean) =
      () => {
        val rnd = rngManager.newUnsafeUniformRng()
        val value = rnd.nextBoolean()
        (value, !value)
      }

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
      booleanOps
    )()
    println(seq1)

    println()
    println("Split each segment into two with random values. Adjacent segments with same values are merged.")
    val seq2 = seq1.flatMapSegments(randomSplit(BoundSelector.intBoundSelector, booleanGenerator(seq1.rngManager)))

    println("Received lazy sequence:")
    println(seq2)

    val bound1 = Bound.Upper.including(35)
    println()
    println(s"Get value at bound $bound1")
    val value1 = seq2.getValueForBound(bound1)
    println(s"Received value: $value1")

    println()
    println("Internal state of lazy sequence was changed - corresponding lazy segment was computed and cached.")
    println("Now we have lazy sequence:")
    println(seq2)

    println()
    println("Compute all lazy values:")
    println(TreapOrderedMap.getFactory.convertMap(seq2))
  }

  def example3(): Unit = {
    println()
    println(s"$sep SegmentSeq.flatMap example $sep")

    println("Initial sequence:")
    val seq1 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (0`)[`, true),
        (10`)[`, false),
        (AboveAll, true)
      ),
      domainOps,
      booleanOps
    )()
    println(seq1)

    println()
    println("Apply flatMap with function such that:")
    println("- if segment value is `false`, then returns universal sequence with string `empty`;")
    println("- if segment value is `true`, then returns sequence:")

    val seq2 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (-10`)[`, "A"),
        (-5`)[`, "B"),
        (5`)[`, "C"),
        (7`)[`, "D"),
        (20`)[`, "E"),
        (25`)[`, "F"),
        (AboveAll, "G")
      ),
      domainOps,
      stringOps
    )()
    println(seq2)

    val seq3 = seq1.flatMap { v => if v then seq2 else UniformOrderedMap.default("empty") }

    println()
    println("Received lazy sequence:")
    println(seq3)

    println()
    println("Compute all lazy values:")
    println(TreapOrderedMap.getFactory.convertMap(seq3))
  }

  def example4(): Unit = {
    println()
    println(s"$sep SegmentSeq.flatMap advanced example $sep")

    println("Initial sequence:")
    val seq1 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (20`)[`, false),
        (AboveAll, true)
      ),
      domainOps,
      booleanOps
    )()
    println(seq1)

    println()
    println("Let's apply flatMap with function, that returns following sequence for each segment:")
    val seq2 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (0`)[`, "A"),
        (10`)[`, "B"),
        (20`)[`, "C"),
        (30`)[`, "D"),
        (40`)[`, "E"),
        (AboveAll, "F")
      ),
      domainOps,
      stringOps
    )()
    println(seq2)

    println()
    println("Received lazy sequence `seq3`:")
    val seq3 = seq1.flatMap { v => seq2 }
    println(seq3)

    println()
    println("Let's get some new sequence `seq4` from the received one.")
    println("For example, we will drop all bounds greater than 25.")
    val seq4 = seq3.takeBelowBound(25`]`)
    println(seq4)

    println()
    println("Then request value of the first segment of `seq3` to compute corresponding lazy value. The result:")
    val firstValue = seq3.firstSegment.value
    println(firstValue)

    println()
    println("Finally let's look again at `seq4`:")
    println(seq4)

    println()
    println("Note, that first lazy value printed as `computed`, because it was calculated earlier and cached.")
    println("Nevertheless, lazy segment sequence is effectively immutable, because it always returns same values")
    println("to the client.")
  }
}
