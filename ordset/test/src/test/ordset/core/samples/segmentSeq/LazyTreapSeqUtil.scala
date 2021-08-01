package test.ordset.core.samples.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{Bound, ExtendedBound, LazySegmentSeq, SegmentSeq}
import ordset.random.RngManager
import test.ordset.core.RandomUtil
import test.ordset.core.domain.BoundSelector

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object LazyTreapSeqUtil {

  /**
   * Performs random access to different parts of sequence to shuffle its internal state.
   */
  def shuffleLazySeq[E, D <: Domain[E], V, SS <: LazySegmentSeq[E, D, V]](
    seq: SS,
    bounds: Iterable[ExtendedBound[E]]
  )(
    implicit rngManager: RngManager
  ): SS = {
    val rng = rngManager.newUnsafeUniformRng()
    val requestsNum = rng.nextInt(bounds.size + 1)
    val boundsArr = ArraySeq.from(bounds)
    (1 to requestsNum) foreach { _ =>
      val rnd = rng.nextInt()
      RandomUtil.randomPick(boundsArr, rnd).foreach { bound =>
        // Only in 1 of 4 cases segment is requested instead of value.
        // If we will request segments to often then more likely then final states will be the same - totally stable.
        val getSegment = (rnd % 4) == 0
        if (getSegment) seq.getSegmentForExtended(bound)
        else seq.getValueForExtended(bound)
      }
    }
    seq
  }

  /**
   * Takes input sequence and builds equivalent lazy sequence with random internal state:
   * different parts of sequence may be lazy or eager.
   */
  def makeRandomLazySeq[E, D <: Domain[E], V](
    seq: SegmentSeq[E, D, V]
  )(
    implicit
    boundSelector: BoundSelector[E],
    rngManager: RngManager
  ): LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] = {

    def split(lowerBound: ExtendedBound[E], upperBound: ExtendedBound[E]): Seq[ExtendedBound[E]] = {
      val ord = seq.domainOps.extendedOrd
      val midOpt = boundSelector.between(lowerBound, upperBound)(ord)
        // Convert bound into upper and keep it only if it's steel between input bounds.
        .map(_.provideUpper)
        .filter(b => ord.gt(b, lowerBound) && ord.lt(b, upperBound))
      if (midOpt.isDefined) Seq(lowerBound, midOpt.get)
      else Seq(lowerBound)
    }

    @tailrec
    def splitN(bounds: Seq[ExtendedBound[E]], steps: Int): Seq[ExtendedBound[E]] =
      if (steps <= 0) bounds
      else {
        val newBounds = bounds.iterator
          .sliding(2, 1).withPartial(false)
          .flatMap(boundsPair => split(boundsPair.head, boundsPair(1)))
          .toSeq
          .appended(ExtendedBound.AboveAll)

        splitN(newBounds, steps - 1)
      }

    @tailrec
    def buildControlSeq(
      bounds: Array[ExtendedBound.Upper[E]],
      boundsIndexes: List[Int],
      initControlSeq: List[(ExtendedBound.Upper[E], () => SegmentSeq[E, D, V])]
    ): List[(ExtendedBound.Upper[E], () => SegmentSeq[E, D, V])] =
      boundsIndexes match {
        case index :: boundsIndexesTail =>
          val newControlSeq = (bounds(index), () => seq) :: initControlSeq
          buildControlSeq(bounds, boundsIndexesTail, newControlSeq)
        case _ =>
          initControlSeq
      }

    val maxSplitSteps = 3

    val rng = rngManager.newUnsafeUniformRng()

    // Get collection of bounds of input `seq`:
    //
    //    |                |                |                   |
    // BelowAll            b1               b2               AboveAll

    val initBounds = LazyList(ExtendedBound.BelowAll).appendedAll(seq.extendedUpperBounds)

    // Split with some probability each interval (some intervals may be splitted several times, others may leave
    // unsplitted):
    //
    //        n1      n2                             n3
    //    |    |      |    |                |        |          |
    // BelowAll            b1               b2               AboveAll

    // Value from 0 to maxSplitSteps.
    val splitSteps = rng.nextInt(maxSplitSteps + 1)
    val splittedBounds = splitN(initBounds, splitSteps)
      .tail // drop `BelowAll` bound
      .map {
        // `split` operation leaves only upper bounds in sequence and also `BelowAll` bound.
        // `BelowAll` was dropped above => all remaining bounds are upper.
        case bound: ExtendedBound.Upper[E] => bound
        case bound => throw new AssertionError(s"Expected upper bound, but got $bound")
      }
      .toArray

    // Get random length of lazy segments. Each number in received sequence defines how many next bounds will include
    // segment:
    //
    // Assume we get [1, 3, 2], then lazy segments will be:
    //
    //        n1      n2                             n3
    //         |      |    |                |        |          |
    //                     b1               b2               AboveAll
    //
    // |-------|
    //         |----------------------------|
    //                                      |-------------------|

    val segmentsSizeSeq = RandomUtil.randomTerms(splittedBounds.length, RandomUtil.Distribution.smallPreferablePow1)

    val boundsIndexList = segmentsSizeSeq.foldLeft(List.empty[Int])((list, size) =>
      list match {
        case head :: _ => (head + size) :: list
        case _ => (size - 1) :: list
    })

    val controlSeq = buildControlSeq(splittedBounds, boundsIndexList, List.empty)

    LazyTreapSeqSample.LazyTreapSegmentSeq.totallyLazy(controlSeq)(seq.domainOps, seq.valueOps, seq.rngManager)
  }
}
