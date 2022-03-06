package ordset.test.core.implementations.segmentSeq.lazyTreap

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.*
import ordset.core.segmentSeq.map.BoundValue
import ordset.core.syntax.SetBuilderNotation.*
import ordset.random.{RngManager, UnsafeUniformRng}
import ordset.test.core.RandomUtil
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import ordset.core.segmentSeq.map.TreapOrderedMap

object LazyTreapSeqUtil {

  /**
   * Performs random access to different parts of sequence to shuffle its internal state.
   */
  def shuffleLazySeq[E, D[X] <: Domain[X], V, SS <: LazySegmentSeq[E, D, V]](
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
   * different parts of sequence may be lazy or strict.
   */
  def makeRandomLazySeq[E, D[X] <: Domain[X], V](
    seq: SegmentSeq[E, D, V]
  )(
    implicit
    boundSelector: BoundSelector[E],
    rngManager: RngManager
  ): LazyTreapSegmentSeq[E, D, V] = {

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
    // non-splitted):
    //
    //        n1      n2                             n3
    //    |    |      |    |                |        |          |
    // BelowAll            b1               b2               AboveAll

    // Value from 0 to maxSplitSteps.
    val splitSteps = rng.nextInt(maxSplitSteps + 1)
    val splittedBounds = splitBounds(initBounds, splitSteps)(seq.domainOps.domain, boundSelector)
      .tail // drop `BelowAll` bound
      .map {
        // `split` operation has left only upper bounds in sequence and also `BelowAll` bound.
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

    LazyTreapSegmentSeq.totallyLazy(controlSeq)(seq.domainOps, seq.valueOps, seq.rngManager)
  }

  /**
   * Builds random treap segment sequence.
   *
   * Each upper bound of sequence satisfies the condition:
   *
   * `lower` `<` bound `<` `upper`
   * 
   * Each value of sequence belongs to input `values` set.
   */
  def makeRandomTreapSeq[E, D[X] <: Domain[X], V](
    lower: ExtendedBound.Lower[E],
    upper: ExtendedBound.Upper[E],
    values: IndexedSeq[V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager,
    boundSelector: BoundSelector[E],
  ): TreapSegmentSeq[E, D, V] = {

    @tailrec
    def generateValues(
      bounds: Vector[ExtendedBound.Upper[E]], 
      prevValue: Option[V],
      acc: Vector[BoundValue[E, V]],
      rng: UnsafeUniformRng
    ): Vector[BoundValue[E, V]] = 
      if (bounds.isEmpty) acc
      else {
        val bound = bounds(0)
        val availableValues = prevValue
          .map(pv => values.filter(v => valueOps.neqv(v, pv)))
          .getOrElse(values)

        val value = RandomUtil.randomPick(availableValues, rng.nextInt())
          .getOrElse(throw new AssertionError("Expected non empty available values"))

        val newAcc = acc.appended((bound, value))
        generateValues(bounds.tail, Some(value), newAcc, rng)
      }

    val seqFactory = TreapOrderedMap.getFactory[E, D, V]

    if (values.isEmpty) seqFactory.buildUniform(valueOps.unit)
    else if (values.size == 1) seqFactory.buildUniform(values(0))
    else {
      val rng = rngManager.newUnsafeUniformRng()

      val maxSplitSteps = 3
      // Value from 0 to maxSplitSteps.
      val splitSteps = rng.nextInt(maxSplitSteps + 1)

      val initBounds = List(lower, upper)
      val splittedBounds = splitBounds(initBounds, splitSteps)(domainOps.domain, boundSelector)
        .drop(1)       // drop first lower bound
        .dropRight(1)  // drop last upper bound
        .appended(ExtendedBound.AboveAll)
        .map {
          // `split` operation has left only upper bounds in sequence and also first lower bound.
          // First lower bound was dropped above => all remaining bounds are upper.
          case bound: ExtendedBound.Upper[E] => bound
          case bound => throw new AssertionError(s"Expected upper bound, but got $bound")
        }
        .toVector

      val boundValues = generateValues(splittedBounds, None, Vector.empty, rng)
      seqFactory.unsafeBuild(boundValues)
    }  
  }

  /**
   * Adds new upper bound (if possible) between each pair of bounds of input collection.
   * Operation is repeated `iterations` times.
   */
  @tailrec
  def splitBounds[E, D[X] <: Domain[X]](
    bounds: Seq[ExtendedBound[E]],
    iterations: Int
  )(
    implicit
    domain: D[E],
    boundSelector: BoundSelector[E]
  ): Seq[ExtendedBound[E]] = {

    def split(lower: ExtendedBound[E], upper: ExtendedBound[E]): Seq[ExtendedBound[E]] = {
      val ord = domain.extendedOrd
      val midOpt = boundSelector.between(lower, upper)(ord)
        // Convert bound into upper and keep it only if it's steel between input bounds.
        .map(_.provideUpper)
        .filter(b => ord.gt(b, lower) && ord.lt(b, upper))
      if (midOpt.isDefined) Seq(lower, midOpt.get)
      else Seq(lower)
    }

    if (iterations <= 0 || bounds.isEmpty) bounds
    else {
      val newBounds = bounds.iterator
        .sliding(2, 1).withPartial(false)
        .flatMap(boundsPair => split(boundsPair.head, boundsPair(1)))
        .toSeq
        .appended(bounds(bounds.size - 1))

      splitBounds(newBounds, iterations - 1)
    }
  }
}