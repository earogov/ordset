package test.ordset.core.examples.segmentSeq

import ordset.core.domain.Domain
import ordset.core.map.{TreapOrderedMap, UniformOrderedMap}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.{ExtendedBound, Segment, SegmentSeq, SeqValidationPredicate}
import ordset.random.RngManager
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.implementations.domain.BoundSelector
import test.ordset.core.samples.segmentSeq.treapOrderedSet.MultiBoundedSetSample1

import scala.language.postfixOps

class FlatmapExample extends AnyFunSpec {

  import ordset.core.instances.boolean.*
  import ordset.core.instances.int.*
  import test.ordset.core.TestRngUtil.Implicits.*

  it("SegmentLikeT.flatMap example") {

    val seq1 = new MultiBoundedSetSample1(10).sequence
    val seq2 = seq1.getSegmentForBound(15`[`).flatMap { () =>
      TreapOrderedMap.getFactory.unsafeBuildAsc(
        List(
          (12`)`, false),
          (15`]`, true),
          (17`)`, false),
          (AboveAll, true)
        ),
        seq1.domainOps,
        seq1.valueOps
      )(
      )(
        seq1.rngManager
      )
    }
    val seq3 = seq2.getSegmentForBound(15`[`).flatMap { () =>
      TreapOrderedMap.getFactory.unsafeBuildAsc(
        List(
          (10`)`, false),
          (13`]`, true),
          (14`)`, false),
          (20`)`, true),
          (AboveAll, false)
        ),
        seq1.domainOps,
        seq1.valueOps
      )(
      )(
        seq1.rngManager
      )
    }
    println(seq3)
  }

  it("SegmentSeqT.flatMap example") {

    def randomSplit[E, D <: Domain[E], V](
      boundSelector: BoundSelector[E],
      valuesGenerator: () => (V, V)
    )(
      segment: Segment[E, D, V]
    ): SegmentSeq[E, D, V] = {
      val boundOrd = segment.domainOps.extendedOrd
      val midBoundOpt = boundSelector
        .between(segment.lowerExtended, segment.upperExtended)(boundOrd)
        .map(_.provideUpper)
        .filter(b => boundOrd.gteqv(b, segment.lowerExtended) && boundOrd.lteqv(b, segment.upperExtended))
      val values = valuesGenerator()
      val boundsSeq = midBoundOpt
        .map(b => List((b, values._1), (ExtendedBound.AboveAll, values._2)))
        .getOrElse(List((ExtendedBound.AboveAll, values._1)))
      TreapOrderedMap.getFactory.unsafeBuildAsc(
        boundsSeq,
        segment.domainOps,
        segment.valueOps
      )(
      )(
        segment.rngManager
      )
    }

    def booleanGenerator(rngManager: RngManager): () => (Boolean, Boolean) =
      () => {
        val rnd = rngManager.newUnsafeUniformRng()
        val value = rnd.nextBoolean()
        (value, !value)
      }

    val seq1 = new MultiBoundedSetSample1(10).sequence
    val seq2 = seq1
      .flatMap(randomSplit(BoundSelector.intBoundSelector, booleanGenerator(seq1.rngManager)))
      .flatMap(randomSplit(BoundSelector.intBoundSelector, booleanGenerator(seq1.rngManager)))

    println(seq2.firstSegment.forwardIterable.map(_.intervalRelation))
  }
}
