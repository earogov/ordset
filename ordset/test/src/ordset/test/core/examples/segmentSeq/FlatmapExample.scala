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

  import ordset.core.instances.boolean.*
  import ordset.core.instances.int.*
  import ordset.test.core.TestRngUtil.Implicits.*

  private val sep = "-----------------"

  private val valueOps: ValueOps[Boolean] = implicitly
  private val domainOps: DomainOps[Int, Domain.UnboundedContinuous[Int]] = implicitly

  @main
  def flatmapExampleMain(): Unit = {
    example1()
    example2()
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
    println(s"Apply flatmap to segment $segment1")

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
    println(s"$sep SegmentSeq.flatMap example $sep")

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
        .between(segment.lowerExtended, segment.upperExtended)(boundOrd)
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
      valueOps
    )()
    println(seq1)

    println()
    println("Split each segment into two with random values. Adjacent segments with same values are merged.")
    val seq2 = seq1.flatMap(randomSplit(BoundSelector.intBoundSelector, booleanGenerator(seq1.rngManager)))

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
}
