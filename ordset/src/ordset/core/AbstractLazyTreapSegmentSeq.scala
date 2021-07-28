package ordset.core

import ordset.{Hash, util}
import ordset.core.domain.Domain
import ordset.core.map.{NonuniformTreapOrderedMap, TreapOrderedMap, UniformOrderedMap, ZippedOrderedMap}
import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap
import ordset.core.internal.{MappedSegmentLikeT, MappedSegmentT}
import AbstractZippedSegmentSeq._
import AbstractTreapSegmentSeq._
import AbstractUniformSegmentSeq._
import AbstractLazyTreapSegmentSeq._
import ordset.core.util.{SegmentSeqUtil, TreapSegmentSeqUtil}
import ordset.util.tag.Tag

import scala.annotation.tailrec

/**
 * Segment sequence that may contain lazy segments - value of such segment is a function that returns some new segment
 * sequence. After being computed new sequence is merged into internal treap structure and then all its segments are
 * accessed directly with standard cost of balanced tree search.
 *
 * <h2>Implementation</h2>
 *
 * Lazy treap sequence consists of three internal sequences.
 *
 * <b>1. Base sequence</b>
 *
 * Base sequence - is a treap sequence that contains computed (eager) values.
 * Lazy segments are allowed to have any value.
 * {{{
 *
 *
 *   X-----------](------------)[------------X
 *          A            B         any value
 * }}}
 *
 * <b>2. Control sequence</b>
 *
 * Control sequence - is a treap sequence that specifies which part of base sequence is lazy and which is eager.
 * {{{
 *
 *
 *   X-----------](------------)[------------X
 *         s             u            ?
 * }}}
 *
 *  where
 *
 *  ? - lazy segment: value hasn't been computed yet, computation will produce new segment sequence instead
 *      of given segment.
 *
 *  u - unstable eager segment: value has been computed, but segment type is undefined (initial, inner, terminal or
 *      single) because of some adjacent segments are lazy.
 *
 *  s - stable eager segment: value had been computed and segment type is defined.
 *
 * <b>3. Zipped sequence</b>
 *
 * Zipped sequence merges base and control sequences. Value of zipped segment is a tuple:
 * <tr>(value of base segment, control value)</tr>
 * {{{
 *
 *
 *   X-----------](------------)[------------X
 *      (A, s)        (B, u)    (?, any value)
 * }}}
 *
 * <h3>Output lazy sequence</h3>
 *
 * External api of class is represented by lazy segments that wrap zipped segments. The basic rule is that:
 * <tr><u>only stable segments are exposed outside.</u></tr>
 * <tr></tr>
 * Lazy and eager unstable segments are transformed into stable one on demand. In that case:
 * <tr>- new versions of base, control and zipped sequences are created such that required segment become stable;</tr>
 * <tr>- new versions of sequences are cached (with synchronization);</tr>
 * <tr>- stable segment is created and returned.</tr>
 *
 * <h3>Stability condition</h3>
 *
 * Eager segment is stable iff it has no adjacent segments such that:
 * <tr>1. Segment is lazy.</tr>
 * {{{
 *
 *                ?        u
 *           X--------](--------
 *             /              \
 *    lazy adjacent       segment is unstable
 * }}}
 * <tr>2. Segment is unstable, has the same value as current segment and has adjacent lazy segment.</tr>
 * {{{
 *               ?         u         u
 *          X--------](--------](---------
 *              -      /   A        A   \
 *     unstable adjacent               segment is unstable
 *       segment with
 *      the same value
 * }}}
 */
abstract class AbstractLazyTreapSegmentSeq[E, D <: Domain[E], V]
  extends AbstractSegmentSeq[E, D, V, LazySegmentBase[E, D, V]] {

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = ???

  final override def isUniversal: Boolean = ???

  final override def isUniform: Boolean = ???

  final override def includesBound(bound: Bound[E]): Boolean = super.includesBound(bound)

  final override def includesExtended(bound: ExtendedBound[E]): Boolean = super.includesExtended(bound)

  final override def includesElement(element: E): Boolean = super.includesElement(element)

  final override def toString: String = zippedSeq.toString

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds

  final override lazy val firstSegment: LazyFirstSegment[E, D, V] = makeFirstSegment

  final override lazy val lastSegment: LazyLastSegment[E, D, V] = makeLastSegment

  final override def getSegmentForBound(bound: Bound[E]): LazySegment[E, D, V] =
    makeSegment(zippedSeq.getSegmentForBound(bound).truncation(bound))

  final override def getSegmentForExtended(bound: ExtendedBound[E]): LazySegment[E, D, V] =
    super.getSegmentForExtended(bound)

  final override def getSegmentForElement(element: E): LazySegment[E, D, V] =
    super.getSegmentForElement(element)

  final override def getValueForBound(bound: Bound[E]): V =
    cacheEager(zippedSeq.getSegmentForBound(bound).truncation(bound)).value._1

  final override def getValueForExtended(bound: ExtendedBound[E]): V =
    cacheEager(zippedSeq.getSegmentForExtended(bound).truncation(bound)).value._1

  final override def getValueForElement(element: E): V =
    super.getValueForElement(element)

  // Transformation ----------------------------------------------------------- //
  final override def takeAboveBound(bound: Bound[E]): TreapSegmentSeq[E, D, V] = sliceAtBound(bound)._2

  final override def takeAboveExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] = super.takeAboveExtended(bound)

  final override def takeBelowBound(bound: Bound[E]): TreapSegmentSeq[E, D, V] = sliceAtBound(bound)._1

  final override def takeBelowExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] = super.takeBelowExtended(bound)

  final override def sliceAtBound(bound: Bound[E]): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) = ???

  final override def sliceAtExtended(bound: ExtendedBound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    super.sliceAtExtended(bound)

  final override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def prependBelowBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    super.prependBelowExtended(bound, other)

  final override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def appendAboveBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    super.appendAboveExtended(bound, other)

  // Protected section -------------------------------------------------------- //
  /**
   * Zipped sequence which joins `baseSeq` and `controlSeq`.
   * <h3>Note</h3>
   *
   * To construct `zippedSeq` from `baseSeq` and `controlSeq` we use type equality:
   *
   * SegmentSeqT[E, D, V, S1] | SegmentSeqT[E, D, V, S2] == SegmentSeq[E, D, V, S1 | S2]
   *
   * due to covariance by S
   *
   * where
   * <tr>S1 == [[TreapSegmentBase]]</tr>
   * <tr>S2 == [[UniformSingleSegment]]</tr>
   */
  @volatile
  protected var zippedSeq: ZSegmentSeq[E, D, V] = _

  protected final val lock = new Object()

  protected override def isValueIncluded(value: V): Boolean

  protected override def consUniform(value: V): SegmentSeq[E, D, V]

  /**
   * <tr>1. If segment of input `ztruncation` is stable then returns it.</tr>
   * <tr>2. Otherwise:</tr>
   * <tr>
   *   2.1. Applies [[provideStableSegment]] to input segment to build new zipped sequence Z with stable segment
   *   at bound `ztruncation.bound`.
   * </tr>
   * <tr>
   *   2.2. Saves new sequence Z into field `zippedSeq`.
   * </tr>
   * <tr>
   *   2.3. Returns segment of new sequence Z at bound `ztruncation.bound`.
   * </tr>
   * <tr></tr>
   * Steps 2.1 - 2.3 are performed with synchronization on field `lock`.
   */
  protected final def cacheStable(ztruncation: ZTruncation[E, D, V]): Stable.ZSegment[E, D, V] = {
    val zsegment = ztruncation.segment
    if (zsegment.value._2.isStable) Stable.unsafe(zsegment)
    else lock.synchronized {
      val newZsegment = provideStableSegment(ztruncation)
      zippedSeq = newZsegment.self.sequence
      newZsegment
    }
  }

  /**
   * <tr>1. If segment of input `ztruncation` is eager then returns it.</tr>
   * <tr>2. Otherwise:</tr>
   * <tr>
   *   2.1. Applies [[provideEagerSeq]] to input segment and receives new zipped sequence Z with eager segments
   *   between bounds of `ztruncation.segment`.
   * </tr>
   * <tr>
   *   2.2. Saves new sequence Z into field `zippedSeq`.
   * </tr>
   * <tr>
   *   2.3. Returns segment of new sequence Z at bound `ztruncation.bound`.
   * </tr>
   * <tr></tr>
   * Steps 2.1 - 2.3 are performed with synchronization on field `lock`.
   */
  protected final def cacheEager(ztruncation: ZTruncation[E, D, V]): Eager.ZSegment[E, D, V] = {
    val zsegment = ztruncation.segment
    if (zsegment.value._2.isEager) Eager.assert(zsegment)
    else lock.synchronized {
      val newZippedSeq = provideEagerSeq(zsegment)
      // Zipped sequence was changed (segment became eager) => search segment in new sequence.
      val newZsegment = Eager.assert(newZippedSeq.getSegmentForExtended(ztruncation.bound))
      zippedSeq = newZippedSeq
      newZsegment
    }
  }

  /**
   * Builds new zipped sequence (if required) with stable segment at bound `ztruncation.bound` and returns this
   * segment.
   *
   * <h3>Algorithm</h3>
   * <tr>
   *   Let's denote as Z - zipped sequence at current step.
   * </tr>
   * <tr>
   *   1. If segment of input `ztruncation` is already stable then return this segment else go to step 2
   *   with Z = `ztruncation.segment.sequence`.
   * </tr>
   * <tr>
   *   2. Apply [[provideEagerSeq]] to input segment to compute lazy value (if required) and get zipped sequence Z,,1,,
   *   that contains only eager segments between bounds of input segment. These segments however may be both stable
   *   and unstable. If segment of Z,,1,, at bound `ztruncation.bound` is stable then return this segment otherwise
   *   go to step 3 with Z = Z,,1,,.
   * </tr>
   * <tr>
   *   3. Get segment of sequence Z at bound `ztruncation.bound` and try to move to the previous segment:
   *   <p>
   *     3.1. If lazy segment was found then apply [[provideEagerSeq]] to it and receive sequence Z,,2,,.
   *     If segment of Z,,2,, at bound `ztruncation.bound` is stable then return this segment otherwise
   *     go to step 3 with Z = Z,,2,,.
   *   </p>
   *   <p>
   *     3.2. Otherwise go step 4 with current sequence Z.
   *   </p>
   * </tr>
   * <tr>
   *   4. Get segment of sequence Z at bound `ztruncation.bound` and try to move to the next segment:
   *   <p>
   *     4.1. If lazy segment was found then apply [[provideEagerSeq]] to it and receive sequence Z,,3,,.
   *     If segment of Z,,3,, at bound `ztruncation.bound` is stable then return this segment otherwise
   *     go to step 4 with Z = Z,,3,,.
   *   </p>
   *   <p>
   *     4.2. Otherwise assertion error must be thrown: the only possibility that segment at bound `ztruncation.bound`
   *     is steel unstable is that it has lazy next segment, but we haven't found it and got contradiction.
   *   </p>
   * </tr>
   * {{{
   *   Example of iteration over steps 3 and 4:
   *
   *                `ztruncation.bound`
   *                         ]
   *       S1       S2       S3      S4     S5
   *   X-------](--------)[------](-----](-------X
   *    (-, ?)    (A, u)   (B, u)  (-, ?)  (-, ?)
   *
   *      step 3: found non lazy S2 => go to step 4
   *                          V
   *
   *                         ]
   *       S1       S2       S3      S4     S5
   *   X-------](--------)[------](-----](-------X
   *    (-, ?)    (A, u)   (B, u)  (-, ?)  (-, ?)
   *
   *      step 4: found lazy S4, compute lazy value,
   *              received unstable S6 => go to step 4
   *                          V
   *
   *                         ]
   *       S1       S2           S6         S5
   *   X-------](--------)[-------------](-------X
   *    (-, ?)    (A, u)       (B, u)     (-, ?)
   *
   *      step 4: found lazy S5, compute lazy value,
   *              received stable S7 => stop
   *                          V
   *
   *                         ]
   *       S1       S2           S7
   *   X-------](--------)[----------------------X
   *    (-, ?)    (A, u)           (B, s)
   *
   *  where
   *  (A, ...) - value of base sequence;
   *  (..., u) - control value: s - eager stable, u - eager unstable, ? - lazy.
   * }}}
   */
  protected final def provideStableSegment(ztruncation: ZTruncation[E, D, V]): Stable.ZSegment[E, D, V] = {

    def getNewZSegment(
      zsegment: ZSegment[E, D, V],
      bound: ExtendedBound[E],
      newZippedSeq: ZSegmentSeq[E, D, V],
    ): ZSegment[E, D, V] =
      if (newZippedSeq.eq(zsegment.sequence)) zsegment   // take old segment if sequence didn't change
      else newZippedSeq.getSegmentForExtended(bound)     // search segment in new sequence

    @tailrec
    def stabilizeLowerBound(zsegment: ZSegment[E, D, V], bound: ExtendedBound[E]): ZSegment[E, D, V] =
      zsegment match {
        case s: ZSegmentWithPrev[E, D, V] =>
          val newZippedSeq = provideEagerSeq(s.movePrev)
          if (newZippedSeq.eq(zsegment.sequence)) zsegment.self
          else {
            val newZsegment = newZippedSeq.getSegmentForExtended(bound)
            if (newZsegment.value._2.isStable) newZsegment
            else stabilizeLowerBound(newZsegment, bound)
          }
        case _ => zsegment.self
      }

    @tailrec
    def stabilizeUpperBound(zsegment: ZSegment[E, D, V], bound: ExtendedBound[E]): ZSegment[E, D, V] =
      zsegment match {
        case s: ZSegmentWithNext[E, D, V] =>
          val newZippedSeq = provideEagerSeq(s.moveNext)
          if (newZippedSeq.eq(zsegment.sequence)) zsegment.self
          else {
            val newZsegment = newZippedSeq.getSegmentForExtended(bound)
            if (newZsegment.value._2.isStable) newZsegment
            else stabilizeUpperBound(newZsegment, bound)
          }
        case _ => zsegment.self
      }

    val bound = ztruncation.bound
    val zsegment = ztruncation.segment
    if (zsegment.value._2.isStable) Stable.unsafe(zsegment.self)
    else {
      val zsegment1 = getNewZSegment(zsegment, bound, provideEagerSeq(zsegment))
      if (zsegment1.value._2.isStable) Stable.unsafe(zsegment1.self)
      else {
        val zsegment2 = stabilizeLowerBound(zsegment1, bound)
        if (zsegment2.value._2.isStable) Stable.unsafe(zsegment2.self)
        else Stable.assert(stabilizeUpperBound(zsegment2, bound))
      }
    }
  }

  /**
   * Returns zipped sequence which contains eager segments (stable or unstable) between bounds of input `zsegment`.
   *
   * <h3>Algorithm</h3>
   *
   * If input `segment` is lazy then:
   * <tr>- compute lazy value (new base sequence for `segment`);</tr>
   * <tr>- patch existing base sequence with new one (see [[patchBaseSeq]]);</tr>
   * <tr>- patch control sequence to indicate that `segment` is not lazy anymore (see [[patchControlSeq]]);</tr>
   * <tr>- build new zipped sequence with new base and control sequences and return it.</tr>
   * <tr></tr>
   * If input `segment` is not lazy then return sequence of this segment.
   * <tr></tr>
   */
  protected final def provideEagerSeq(zsegment: ZSegment[E, D, V]): ZSegmentSeq[E, D, V] =
    zsegment.value._2 match {
      case lazyValue: LazyValue[E, D, V] =>
        val seq = lazyValue.compute
        val newBaseSeq: BaseSegmentSeq[E, D, V] = patchBaseSeq(zsegment, seq)
        val newControlSeq: ControlSegmentSeq[E, D, V] = patchControlSeq(zsegment, seq)
        makeZippedSeq(newBaseSeq, newControlSeq)
      case _ =>
        zsegment.self.sequence
    }

  /**
   * Appends lazy sequence `other` to current sequence above specified `bound` (see [[SegmentSeqT.appendAboveBound]]).
   * {{{
   *
   *  current sequence:
   *
   *               bound   otherBoundZsegment
   *                 ]     /
   *  X-------](--------------)[---------X
   *   (A, s)       (B, s)       (C, s)
   *
   *  other sequence:
   *
   *  X---------)[----------](-----------X
   *    (D, u)      (-, ?)      (E, u)
   *
   *  result:
   *
   *  X-------](-----](-----](-----------X
   *   (A, s)  (B, u)  (-, ?)   (E, u)
   *
   *  where
   *  (A, ...) - value of base sequence;
   *  (..., u) - control value: s - eager stable, u - eager unstable, ? - lazy.
   * }}}
   * Input `originalBoundZsegment` must satisfy precondition:
   * {{{
   *   originalBoundZsegment.containsBound(bound.provideUpper) == true    (1)
   * }}}
   * Note, if provided segment other then one defined by condition 1, the behaviour of method is undefined.
   */
  protected final def appendZippedInternal(
    bound: Bound[E],
    originalBoundZsegment: ZSegment[E, D, V],
    other: ZSegmentSeq[E, D, V]
  ): ZSegmentSeq[E, D, V] = {

    //             bound
    //               ]
    //  --)[--------------](--------    current sequence
    //             s
    //
    //  ------)[--------------](----    other sequence
    //                u
    //
    // Depending on control values of adjacent segments after merge next cases are possible:
    //
    //      s    u    ?
    //  s   +    -    -
    //  u   -    -    +
    //  ?   -    +    +
    //
    // where: + means that no additional correction required; - means correction.
    //
    // 1) u-s, s-u and u-u cases
    //
    // Each unstable segment may become stable.
    //
    //             bound
    //               ]
    //  --)[--------------](--------    current sequence
    //  s          u           ?
    //
    //  ------)[--------------](----    other sequence
    //     ?          u          s
    //
    //  --)[---------](-------](----    result sequence
    //  s       s         s      s
    //
    // 2) ?-s and s-? cases
    //
    // Each stable segment becomes unstable.
    //
    //             bound
    //               ]
    //  --)[--------------](--------    current sequence
    //  u          ?           u
    //
    //  ------)[--------------](----    other sequence
    //     s          s          s
    //
    //  --)[---------](-------](----    result sequence
    //  u       ?         u      s

//    val originalBoundControlSegment = originalBoundZsegment.self.secondSeqSegment
//
//    val otherBoundZsegment = other.getSegmentForBound(bound.provideLower)
//    val originalControlValue = originalBoundZsegment.value._2
//    val otherControlValue = otherBoundZsegment.value._2
//
//    val newBaseSeq = TreapSegmentSeqUtil.appendAboveTruncation(
//      originalBoundZsegment.self.firstSeqSegment.truncation(bound),
//      otherBoundZsegment.sequence.firstSeq
//    )
//
//    val newControlSeq = {
//      // s-s, u-?, ?-u, ?-?
//      if (
//        originalControlValue.isStable && otherControlValue.isStable ||
//        originalControlValue.isLazy && otherControlValue.isUnstable ||  // note lazy value is also unstable
//        originalControlValue.isUnstable && otherControlValue.isLazy
//      ) {
//        TreapSegmentSeqUtil.appendAboveTruncation(
//          originalBoundControlSegment.truncation(bound),
//          otherBoundZsegment.sequence.secondSeq
//        )
//      // ?-s
//      } else if (originalControlValue.isLazy) {
//        TreapSegmentSeqUtil.appendAboveTruncation(
//          originalBoundControlSegment.truncation(bound),
//          otherBoundZsegment..upperTruncation.prepend(makeUniformControlSeq(EagerValue.unstable))
//        )
//      // s-?
//      } else if (otherControlValue.isLazy) {
//        TreapSegmentSeqUtil.prependBelowTruncation(
//          otherBoundZsegment.truncation(bound),
//          originalBoundControlSegment.lowerTruncation.append(makeUniformControlSeq(EagerValue.unstable)),
//        )
//      } else {
//        null
//      }
//    }
//    makeZippedSeq(newBaseSeq, newControlSeq)

    null
  }

  /**
   * Creates zipped sequence with specified base and control sequences.
   */
  protected final def makeZippedSeq(
    baseSeq: BaseSegmentSeq[E, D, V],
    controlSeq: ControlSegmentSeq[E, D, V]
  ): ZSegmentSeq[E, D, V] =
    ZippedOrderedMap.apply(
      baseSeq, controlSeq, Tuple2.apply, _ => false, _ => false
    )(
      domainOps, zippedSeq.valueOps, rngManager
    )

  /**
   * Creates uniform zipped sequence with lazy value `seqFunc`.
   */
  protected final def makeUniformLazyZippedSeq(
    seqFunc: () => SegmentSeq[E, D, V]
  ): ZSegmentSeq[E, D, V] =
    makeZippedSeq(
      makeUniformBaseSeq(valueOps.unit),
      makeUniformControlSeq(LazyValue(seqFunc))
    )

  /**
   * Creates uniform base sequence with specified `value`.
   */
  protected final def makeUniformBaseSeq(
    value: V
  ): UniformSegmentSeq[E, D, V] =
    UniformOrderedMap.apply(
      value, TreapOrderedMap.getFactory
    )(
      domainOps, valueOps, rngManager
    )

  /**
   * Creates uniform control sequence with specified `value`.
   */
  protected final def makeUniformControlSeq(
    value: ControlValue[E, D, V]
  ): UniformSegmentSeq[E, D, ControlValue[E, D, V]] =
    UniformOrderedMap.apply(
      value, TreapOrderedMap.getFactory
    )(
      domainOps, ControlValueOps.get, rngManager
    )

  /**
   * Creates nonuniform control sequence from specified treap `root` and `value` (value of last segment).
   */
  protected final def makeNonuniformControlSeq(
    root: ImmutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]],
    value: ControlValue[E, D, V]
  ): NonuniformTreapSegmentSeq[E, D, ControlValue[E, D, V]] =
    NonuniformTreapOrderedMap.unchecked(
      root, value
    )(
      domainOps, ControlValueOps.get, rngManager
    )

  /**
   * Creates nonuniform control sequence:
   * {{{
   *
   *     `firstValue`  `secondValue`
   *   X-------------|-------------X
   *              `bound`
   * }}}
   * Preconditions:
   *
   * 1. `firstValue` != `secondValue`
   */
  protected final def makeSingleBoundedControlSeq(
    firstValue: ControlValue[E, D, V],
    secondValue: ControlValue[E, D, V],
    bound: Bound.Upper[E]
  ): NonuniformTreapSegmentSeq[E, D, ControlValue[E, D, V]] = {
    val buffer =
      BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], ControlValue[E, D, V]](
        List.empty[MutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]]],
        bound,
        rngManager.newUnsafeUniformRng().nextInt(),
        firstValue
      )(
        domainOps.boundOrd
      )
    val root = BuildAsc.finalizeBuffer(buffer)
    root match {
      case root: ImmutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]] =>
        makeNonuniformControlSeq(root, secondValue)
      // `buffer` is non-empty => `root` is a treap node.
      case _ =>
        throw new AssertionError(s"Expected non-empty tree $root for control sequence.")
    }
  }

  /**
   * Takes new `baseSeq` and applies patch operation to existing base sequence.
   * All modifications of base sequence are performed within `zsegment`, everything outside of it remains unchanged.
   *
   * {{{
   *   zsegment:
   *
   *                (----------------------]
   *
   *   baseSeq (old):
   *                             Z                             - values
   *   X----------------------------------------------------X
   *
   *   baseSeq (new):
   *
   *        A         B           C          D         E       - values
   *   X--------](---------](----------](--------](---------X
   *
   *   controlSeq (output):
   *
   *          Z         B         C       D         Z          - values
   *   X-----------](------](----------](--](---------------X
   *                ^                      ^
   *        zsegment.lowerBound      zsegment.upperBound
   * }}}
   * where u - unstable eager segment; s - stable eager segment; ? - lazy segment.
   *
   * @param zsegment zipped segment (contains base segment and control segment).
   * @param baseSeq sequence that was computed for `zsegment` to patch corresponding base segment.
   */
  protected final def patchBaseSeq(
    zsegment: ZSegment[E, D, V],
    baseSeq: SegmentSeq[E, D, V]
  ): BaseSegmentSeq[E, D, V] =
  // Performance note
  // Both original sequences of `zsegment` are treap based. The result of patch operation in most cases will
  // be also a treap based sequence. `convertMap` doesn't perform real conversion for them. Instead it just
  // returns unmodified input sequence.
  // The only exception - when uniform sequence is patched by some non-treap `baseSeq`. In such a case
  // patch operation may produce sequence of any type and `convertMap` will create new treap based instance.
    TreapOrderedMap.getFactory.convertMap(
      zsegment.self.patchFirstSeq(baseSeq)
    )

  /**
   * Builds new control sequence for a given `zsegment` and applies patch operation to existing control sequence.
   * Note, modifications of control sequence may lay outside of `zsegment`. In example below left adjacent segment
   * was updated to actualize stability indicators.
   * {{{
   *
   *        left adjacent segment
   *              |
   *     ?        |  u               ?           ?
   *  ------](--------------](--------------](--------  old control seq
   *        |               |               |
   *        |    A       B  |               |
   *  ------](------](------](------------------------  old base seq
   *        |               |               |
   *        |               |               |
   *        |               |(--------------]           input zipped segment
   *        |               |               |
   *        |         C     |               |  D
   *  ------------------------------](----------------  input base seq
   *        |               |               |
   *     ?       u           s           u        ?
   *  ------](------](--------------](------](--------  new control seq
   *        |               |               |
   *             A       B       C       D
   *  ------](------](------](------](------](--------  new base seq
   * }}}
   * where u - unstable eager segment; s - stable eager segment; ? - lazy segment.
   */
  protected final def patchControlSeq(
    zsegment: ZSegment[E, D, V],
    baseSeq: SegmentSeq[E, D, V]
  ): ControlSegmentSeq[E, D, V] = {

    // Let's call as patch sequence new control sequence that will replace existing one in some region around given
    // `zsegment`. Method builds patch sequence, inserts it into existing control sequence and returns new one.
    //
    // 1. Starting from `zsegment` try to move backward to the previous segment:
    //    - if `zsegment` is first then define `patchLowerBound` as lower bound of `zsegment`;
    //    - if previous segment is stable or lazy then define `patchLowerBound` as lower bound of `zsegment`;
    //    - if previous segment is eager unstable then define `patchLowerBound` as lower bound of previous segment.
    //
    //    Similarly define `patchUpperBound`.
    //
    //   ?      u       u                         u       s
    // ----](------)[------](----------------](------)[--------   zipped seq
    //              ^            zsegment            ^
    //       patchLowerBound                   patchUpperBound
    //
    // 2. If `patchLowerBound` != `zsegment.lowerBound` then define first segment of patch sequence as
    //    segment of zipped sequence at bound `patchLowerBound` with control value:
    //    - eager stable iff all its adjacent segments are eager;
    //    - eager unstable otherwise.
    //
    //    If `patchUpperBound` != `zsegment.upperBound` then similarly define last segment of patch sequence.
    //
    //    If both conditions are met then:
    //    - merge first and last segments:
    //      - build `leftPatchSequence` (see 4.1.1 and 4.1.2) and `rightPatchSequence` (see 4.2.1 and 4.2.2);
    //      - get merged sequence as `leftPatchSequence.append(rightPatchSequence)`;
    //    - apply received patch sequence to old control sequence within `patchLowerBound` and `patchUpperBound`
    //      and return the result.
    //
    //    Otherwise go to step 3.
    //
    //                         patch sequence
    //              +--------------------------------+
    //   ?      u   |                s               |    s
    // ----](------)[--------------------------------)[--------   new control seq
    //              |                                |
    //      patchLowerBound                    patchUpperBound
    //
    // 3. If one or both conditions are `true`:
    //    - `patchLowerBound` == `zsegment.lowerBound`
    //    - `patchUpperBound` == `zsegment.upperBound`
    //
    //                  lower bound     upper bound
    //   ?      u       u   |                |        ?
    // ----](------)[------](----------------](----------------   zipped seq
    //         S1   |   S2       zsegment    |        S3
    //     patchLowerBound                 patchUpperBound
    //
    //    Call method `makeControlSeq` with parameters:
    //    - `stableLowerBound` = (lower bound of `zsegment` satisfies stability condition*)
    //    - `stableUpperBound` = (upper bound of `zsegment` satisfies stability condition*)
    //    * see class description.
    //
    //    In example above we have:
    //
    //    - `stableLowerBound` == `true`:   left adjacent segment of `zsegment` (S2) is eager unstable and assume
    //                                      it has the same value as `zsegment` but its left adjacent S1 is eager.
    //
    //    - `stableUpperBound` == `false`:  left adjacent segment of `zsegment` (S3) is lazy.
    //
    //    Let's denote received sequence as `innerPatchSequence`.
    //
    //    If both conditions are `true`:
    //    - `patchLowerBound` == `zsegment.lowerBound`
    //    - `patchUpperBound` == `zsegment.upperBound`
    //
    //    then apply `innerPatchSequence` to old control sequence within `patchLowerBound` and `patchUpperBound`
    //    and return the result.
    //
    //    Otherwise go to step 4.
    //
    // 4. Merge `innerPatchSequence` with first or last segment of patch sequence obtained at step 2 (see 4.1 and 4.2).
    //
    //                     patch sequence
    //              +------------------------+
    //              |          inner patch   |
    //              |       +----------------+
    //   ?      u   |       | s           u  |        ?
    // ----](------)[-----------------)[-----](----------------   new control seq
    //              ^                        ^
    //     patchLowerBound             patchUpperBound
    //
    //   Apply patch sequence to old control sequence within `patchLowerBound` and `patchUpperBound` and
    //   return the result
    //
    // 4.1 To merge first segment with `innerPatchSequence` build `leftPatchSequence`:
    //
    // 4.1.1
    //           u
    //        (-----)                  first patch segment
    //        u     |       s
    // X------------)[------------X    `leftPatchSequence`
    //
    // 4.1.2
    //           s
    //        (-----)                  first patch segment
    //              s
    // X--------------------------X    `leftPatchSequence`
    //
    // Then get merged sequence as `leftPatchSequence.append(innerPatchSequence)`.
    //
    // 4.2 To merge last segment with `innerPatchSequence` build `rightPatchSequence`:
    //
    // 4.2.1
    //                 u
    //              (-----)            last patch segment
    //        s     |       u
    // X-----------](-------------X    `rightPatchSequence`
    //
    // 4.2.2
    //                 s
    //              (-----)            last patch segment
    //              s
    // X--------------------------X    `rightPatchSequence`
    //
    // Then get merged sequence as `rightPatchSequence.prepend(innerPatchSequence)`.

    /**
     * Properties of either left or right side of patch sequence.
     */
    abstract sealed class PatchBoundInfo(
      /**
       * If class represents left side info:
       * <tr>contains `patchLowerBound` (see p.1) with corresponding segment of old control sequence.</tr>
       *
       * If class represents right side info:
       * <tr>contains `patchUpperBound` (see p.1) with corresponding segment of old control sequence.</tr>
       */
      val patchBoundTruncation: SegmentTruncation[E, D, ControlValue[E, D, V]],
      /**
       * `true` if corresponding bound of `zsegment` satisfies stability condition (see p.3).
       */
      val zsegmentBoundIsStable: Boolean,
    )

    /**
     * [[PatchBoundInfo]] for case when condition from p.2 isn't satisfied:
     * <tr>`patchLowerBound` == `zsegment.lowerBound` - for left side info;</tr>
     * <tr>`patchUpperBound` == `zsegment.upperBound` - for right side info.</tr>
     */
    case class NonShiftedPatchBoundInfo(
      override val patchBoundTruncation: SegmentTruncation[E, D, ControlValue[E, D, V]],
      override val zsegmentBoundIsStable: Boolean,
    ) extends PatchBoundInfo(
      patchBoundTruncation,
      zsegmentBoundIsStable
    )

    /**
     * [[PatchBoundInfo]] for case when condition from p.2 is satisfied:
     * <tr>`patchLowerBound` != `zsegment.lowerBound` - for left side info;</tr>
     * <tr>`patchUpperBound` != `zsegment.upperBound` - for right side info.</tr>
     */
    case class ShiftedPatchBoundInfo(
      override val patchBoundTruncation: SegmentTruncation[E, D, ControlValue[E, D, V]],
      override val zsegmentBoundIsStable: Boolean,
      /**
       * <tr>`leftPatchSequence` - if class represents left side info;</tr>
       * <tr>`rightPatchSequence` - if class represents right side info;</tr>
       * (see p.4.1 and 4.2)
       */
      patchBoundSequence: TreapSegmentSeq[E, D, ControlValue[E, D, V]]
    ) extends PatchBoundInfo(
      patchBoundTruncation,
      zsegmentBoundIsStable
    )

    object PatchBoundInfo {

      def getLeftSideInfo: PatchBoundInfo = makeInfo(LeftSideFactory)

      def getRightSideInfo: PatchBoundInfo = makeInfo(RightSideFactory)

      // Private section ---------------------------------------------------------- //
      private def makeInfo(factory: Factory): PatchBoundInfo = {
        var nextStep = true
        var firstStep = true
        var zsegmentBoundIsStable: Boolean | Null = null
        var patchBoundIsShifted = false
        var patchBoundIsLazy = false
        var patchBoundSegment = zsegment
        val segmentIterator = factory.getIterable(zsegment).drop(1).iterator // skip `zsegment` itself
        while (segmentIterator.hasNext && nextStep) {
          val currZsegment = segmentIterator.next()
          val currBaseValue = currZsegment.self.value._1
          val currControlValue = currZsegment.self.value._2
          // Current segment is adjacent to `zsegment`.
          if (firstStep) {
            nextStep = currControlValue.isEagerUnstable
            zsegmentBoundIsStable =
              if (currControlValue.isEagerUnstable && valueOps.eqv(currBaseValue, zsegment.value._1))
                null // bound stability will be defined on the next step
              else
                currControlValue.isEager
          // Current segment is second after `zsegment`.
          } else {
            nextStep = false
            if (zsegmentBoundIsStable == null) zsegmentBoundIsStable = currControlValue.isEager
          }
          if (nextStep) {
            patchBoundIsShifted = true
            patchBoundSegment = currZsegment
          } else {
            patchBoundIsLazy = currControlValue.isLazy
          }
          firstStep = false
        }
        if (zsegmentBoundIsStable == null) zsegmentBoundIsStable = true  // `zsegment` is the first segment of sequence
        factory.build(zsegmentBoundIsStable.nn, patchBoundIsShifted, patchBoundIsLazy, patchBoundSegment)
      }

      private trait Factory {

        def build(
          stableAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo

        def getIterable(zsegment: ZSegment[E, D, V]): Iterable[ZSegment[E, D, V]]
      }

      private object LeftSideFactory extends Factory {

        override def build(
          stableAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo = {

          val patchBoundTruncation = patchBoundZsegment.self.secondSeqLowerTruncation
          if (patchBoundIsShifted) {
            val patchBoundSequence: ControlSegmentSeq[E, D, V] = patchBoundZsegment match {
              case s: ZippedSegmentWithNext[E, D, V, ControlValue[E, D, V], ZValue[E, D, V], _, _] =>
                // Adjacent segment of first patch segment is lazy =>
                // first patch segment is eager unstable (see p.2) =>
                // case 4.1.1
                if (patchBoundIsLazy)
                  makeSingleBoundedControlSeq(EagerValue.unstable, EagerValue.stable, s.upperBound)
                // otherwise case 4.1.2
                else
                  makeUniformControlSeq(EagerValue.stable)
              case _ =>
                // `patchBoundIsShifted` = `true` =>
                // there is at least original `zsegment` after `boundZsegment` =>
                // `boundZsegment` has next segment.
                throw new AssertionError(
                  s"Expected that segment $patchBoundZsegment has next segment."
                )
            }
            ShiftedPatchBoundInfo(patchBoundTruncation, stableAdjacent, patchBoundSequence)
          } else {
            NonShiftedPatchBoundInfo(patchBoundTruncation, stableAdjacent)
          }
        }

        override def getIterable(zsegment: ZSegment[E, D, V]): Iterable[ZSegment[E, D, V]] = zsegment.backwardIterable
      }

      private object RightSideFactory extends Factory {

        override def build(
          stableAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo = {

          val patchBoundTruncation = patchBoundZsegment.self.secondSeqUpperTruncation
          if (patchBoundIsShifted) {
            val patchBoundSequence: ControlSegmentSeq[E, D, V] = patchBoundZsegment match {
              case s: ZippedSegmentWithPrev[E, D, V, ControlValue[E, D, V], ZValue[E, D, V], _, _] =>
                // Adjacent segment of last patch segment is lazy =>
                // last patch segment is eager unstable (see p.2) =>
                // case 4.2.1
                if (patchBoundIsLazy)
                  makeSingleBoundedControlSeq(EagerValue.stable, EagerValue.unstable, s.lowerBound.flipLower)
                // otherwise case 4.2.2
                else
                  makeUniformControlSeq(EagerValue.stable)
              case _ =>
                // `patchBoundIsShifted` = `true` =>
                // there is at least original `zsegment` before `patchBoundZsegment` =>
                // `patchBoundZsegment` has previous segment.
                throw new AssertionError(
                  s"Expected that segment $patchBoundZsegment has previous segment."
                )
            }
            ShiftedPatchBoundInfo(patchBoundTruncation, stableAdjacent, patchBoundSequence)
          } else {
            NonShiftedPatchBoundInfo(patchBoundTruncation, stableAdjacent)
          }
        }

        override def getIterable(zsegment: ZSegment[E, D, V]): Iterable[ZSegment[E, D, V]] = zsegment.forwardIterable
      }
    }

    val leftSideInfo = PatchBoundInfo.getLeftSideInfo
    val rightSideInfo = PatchBoundInfo.getRightSideInfo
    val patchSequence: SegmentSeq[E, D, ControlValue[E, D, V]] = (leftSideInfo, rightSideInfo) match {
      // See p.2.
      case (li: ShiftedPatchBoundInfo, ri: ShiftedPatchBoundInfo) =>
        li.patchBoundSequence.append(ri.patchBoundSequence)
      // See p.3 for all cases below.
      case (li: ShiftedPatchBoundInfo, ri: NonShiftedPatchBoundInfo) =>
        val innerPatchSequence = makeControlSeq(zsegment, baseSeq, li.zsegmentBoundIsStable, ri.zsegmentBoundIsStable)
        li.patchBoundSequence.append(innerPatchSequence)
      case (li: NonShiftedPatchBoundInfo, ri: ShiftedPatchBoundInfo) =>
        val innerPatchSequence = makeControlSeq(zsegment, baseSeq, li.zsegmentBoundIsStable, ri.zsegmentBoundIsStable)
        ri.patchBoundSequence.prepend(innerPatchSequence)
      case (li: NonShiftedPatchBoundInfo, ri: NonShiftedPatchBoundInfo) =>
        makeControlSeq(zsegment, baseSeq, li.zsegmentBoundIsStable, ri.zsegmentBoundIsStable)
    }
    val tmpSeq =leftSideInfo.patchBoundTruncation.append(patchSequence)
    val newSeq = rightSideInfo.patchBoundTruncation.prepend(tmpSeq)
    // Performance note
    // `newSeq` is always treap based. `convertMap` doesn't perform real conversion for it.
    // Instead it just returns unmodified input sequence.
    TreapOrderedMap.getFactory.convertMap(newSeq)
  }

  /**
   * Builds control sequence from a new sequence `baseSeq` that was computed for a given `zsegment`.
   *
   * `stableLowerBound` and `stableUpperBound` input arguments define bound conditions:
   * <tr>
   *   - if indicator is `true` then corresponding bound will be present after all subsequent computations of
   *   lazy values in sequence.
   * </tr>
   * <tr></tr>
   * Let's denote:
   * {{{
   *               zsegment
   *       !(------------------]!
   *      /                      \
   *   stableLowerBound == true   stableUpperBound == true
   *
   *               zsegment
   *       ?(-------------------]?
   *      /                       \
   *   stableLowerBound == false   stableUpperBound == false
   * }}}
   * <tr></tr>
   *
   * Segments of `baseSeq` are mapped to the segments of output control sequence. Output segment may get either
   * `stable` or `unstable` control value (see stability condition in class description [[AbstractLazyTreapSegmentSeq]]).
   * <tr></tr>
   *
   * 1. If some bound is marked as "unstable" (for example `stableLowerBound` == false) then corresponding bound segment
   * of new control sequence MUST be unstable.
   * {{{
   *
   *               zsegment
   *       ?(-------------------
   *
   *   output:
   *   ------------)[-----------
   *     unstable
   * }}}
   *
   * 2. If some bound is marked as "stable" then bound segment MAY be stable depending on other adjacent segment.
   * <tr></tr>
   * 2.1.
   * {{{
   *             zsegment
   *       !(--------------]?
   *
   *   output:
   *   -----------)[-------------
   *      stable      unstable
   * }}}
   *
   * Note, output segments are created according to input new base sequence. As for any sequence all base segments
   * have different values => stability condition (see [[AbstractLazyTreapSegmentSeq]]) for left segment is always
   * satisfied.
   * <tr></tr>
   * 2.2.
   * {{{
   *             zsegment
   *       !(--------------]?
   *
   *   output:
   *   ---------)[------](-------
   *     stable   stable
   * }}}
   * <tr></tr>
   * 2.3.
   * {{{
   *             zsegment
   *       !(--------------]?
   *
   *   output:
   *   --------------------------
   *            unstable
   * }}}
   *
   * 3. Segments of `baseSeq` that are completely inside input `zsegment` are mapped to stable control values.
   * {{{
   *                         zsegment:
   *               ?(----------------------]?
   *
   *   baseSeq:
   *   X--------](---------](----------](--------](---------X
   *        A         B           C          D         E       - values
   *
   *   output:
   *   X-------------------](----------](-------------------X
   *         unstable          stable         unstable         - control values
   * }}}
   *
   * @param zsegment zipped segment (contains base segment and control segment).
   * @param baseSeq sequence that was computed for `zsegment` to patch corresponding base segment.
   * @param stableLowerBound `true` if lower bound will be present after all computations of lazy values in sequence.
   * @param stableUpperBound `true` if upper bound will be present after all computations of lazy values in sequence.
   */
  protected final def makeControlSeq(
    zsegment: ZSegment[E, D, V],
    baseSeq: SegmentSeq[E, D, V],
    stableLowerBound: Boolean,
    stableUpperBound: Boolean
  ): ControlSegmentSeq[E, D, V] =
    //
    //       !(-----------]!       - zsegment
    // X------------------------X  - output
    //           stable
    if (stableLowerBound && stableUpperBound) {
      makeUniformControlSeq(EagerValue.stable)

    } else {
      val boundSegments = SegmentSeqUtil.getBoundSegments(zsegment, baseSeq)

      //          (---]            - zsegment
      //  X--)[----------)[----X   - baseSeq
      //  X--------------------X   - output
      //            \
      //        stable if both `eagerLowerBound` and `eagerUpperBound` are `true`
      if (domainOps.segmentUpperOrd.eqv(boundSegments._1, boundSegments._2)) {
        makeUniformControlSeq(EagerValue.cons(isStable = stableLowerBound && stableUpperBound))

      } else boundSegments match {
        case (lowerSegment: Segment.WithNext[E, D, V], upperSegment: Segment.WithPrev[E, D, V]) =>
          val nextSegment = lowerSegment.moveNext

          //         (----------------]           - zsegment
          //  X---)[---------)[-----------](--X   - baseSeq
          if (domainOps.segmentUpperOrd.eqv(nextSegment, upperSegment)) {
            // If `eagerLowerBound` == `eagerUpperBound` then both `eagerLowerBound` and `eagerUpperBound` are `false`,
            // because the case when they are both `true` was considered before.
            //
            //      ?(----------------]?          - zsegment
            // X------------------------------X   - output
            //            unstable
            if (stableLowerBound == stableUpperBound) {
              makeUniformControlSeq(EagerValue.unstable)

              // `eagerLowerBound` and `eagerUpperBound` are different.
              //
              //      ?(----------------]!          - zsegment
              // X-------------)[---------------X   - output
              //      unstable       stable
              //
              // or vise versa:
              //
              //      !(----------------]?          - zsegment
              // X-------------)[---------------X   - output
              //       stable        unstable
            } else {
              makeSingleBoundedControlSeq(
                EagerValue.cons(stableLowerBound),
                EagerValue.cons(stableUpperBound),
                lowerSegment.upperBound
              )
            }
            //         (-----------------]          - zsegment
            // X---)[-----)[----](----)[-----](--X  - baseSeq
          } else {
            val boundOrd = domainOps.boundOrd
            val rng = rngManager.newUnsafeUniformRng()
            var buffer = List.empty[MutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]]]

            //      ?(-----------------]          - zsegment
            // X-)[-----)[----](----)[-----](--X  - baseSeq
            // X--------)[-------                 - output
            //  unstable
            if (!stableLowerBound) {
              buffer =
                BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], ControlValue[E, D, V]](
                  buffer,
                  lowerSegment.upperBound,
                  rng.nextInt(),
                  EagerValue.unstable
                )(
                  boundOrd
                )
            }
            //       (-----------------]?         - zsegment
            // X-)[-----)[----](----)[-----](--X  - baseSeq
            //                ------)[---------X  - output
            //                stable   unstable
            if (!stableUpperBound) {
              buffer =
                BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], ControlValue[E, D, V]](
                  buffer,
                  upperSegment.lowerBound.flipLower,
                  rng.nextInt(),
                  EagerValue.stable
                )(
                  boundOrd
                )
            }
            val root = BuildAsc.finalizeBuffer(buffer)
            root match {
              case root: ImmutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]] =>
                makeNonuniformControlSeq(root, EagerValue.cons(stableUpperBound))
              // The case when both `eagerLowerBound` and `eagerUpperBound` are `true` was considered before =>
              // either `eagerLowerBound` or `eagerUpperBound` is `false` =>
              // there was at least one write to `buffer` =>
              // `buffer` is non-empty =>
              // `root` is a treap node.
              case _ =>
                throw new AssertionError(s"Expected non-empty tree $root for control sequence.")
            }
          }
        case _ =>
          // boundSegments._1 != boundSegments._2 =>
          // boundSegments._2 follows after boundSegments._1 =>
          // boundSegments._1 has next segment and boundSegments._2 has previous segment.
          throw new AssertionError(
            s"Expected that segment ${boundSegments._1} has next segment and " +
              s"segment ${boundSegments._2} has upper segment."
          )
      }
    }

  /**
   * Returns segment of lazy sequence that corresponds to input `ztruncation`.
   * `ztruncation` captures segment of zipped sequence and some bound inside this segment.
   *
   * <h3>Note</h3>
   *
   * After segment of input `ztruncation` is transformed into stable one we just wrap it with [[LazySegment]]
   * which extracts value of base sequence. We can guarantee that there will be no adjacent output segments
   * with the same value, so no additional merging of segments is applied (see Lemma 1).
   *
   * <h3>Lemma 1</h3>
   *
   * Adjacent segments of stable zipped segment S in zipped sequence have values other then value of S.
   *
   * Consider cases when one stable segment of base sequence spans several control segments:
   * {{{
   *
   * 1.  -----------](------------  control sequence
   *          s            ?
   *     -------------------------  base sequence
   *                A
   * }}}
   * Case is impossible due to stable segment must be always separated from a lazy one with unstable eager segment.
   * {{{
   *
   * 2.  -----------](------------  control sequence
   *          s            u
   *     -------------------------  base sequence
   *                A
   * }}}
   * The only possible previous state for case 2 is:
   * {{{
   *
   * 2.a       S1           S2
   *      -----------](------------  control sequence
   *           u            ?
   *      -----------](------------  base sequence
   *           A        any value
   * }}}
   * After computation lazy segment S2 should become unstable with value A, and S1 - stable.
   * S2 will be unstable iff its right adjacent segment is lazy.
   * S1 will be stable iff its left adjacent segment is eager.
   * But in that case we will get:
   * {{{
   *
   * 2.b                  S3
   *      --](--------------------------)[--  control sequence
   *       s              u               ?
   *      --](--------------------------)[--  base sequence
   *                      A
   * }}}
   * Segments S1 and S2 will be merged into eager unstable S3. So we can't get case 2 from 2.a and
   * case 2 is impossible.
   *
   * Q.E.D.
   */
  protected final def makeSegment(ztruncation: ZTruncation[E, D, V]): LazySegment[E, D, V] =
    cacheStable(ztruncation) match {
      case s: ZSegmentInner[E, D, V] @unchecked => LazyInnerSegment(this, s)
      case s: ZSegmentInitial[E, D, V] @unchecked => LazyInitialSegment(this, s)
      case s: ZSegmentTerminal[E, D, V] @unchecked => LazyTerminalSegment(this, s)
      case s: ZSegmentSingle[E, D, V] @unchecked => LazySingleSegment(this, s)
      case s => throwSegmentMustHaveOneOfBaseTypes(s) // just to remove warning
    }

  /**
   * Returns segment of lazy sequence that has previous segment and corresponds to input `ztruncation`.
   *
   * @see [[makeSegment]].
   */
  protected final def makeSegmentWithPrev(ztruncation: ZTruncationWithPrev[E, D, V]): LazySegmentWithPrev[E, D, V] =
    cacheStable(ztruncation) match {
      case s: ZSegmentInner[E, D, V] @unchecked => LazyInnerSegment(this, s)
      case s: ZSegmentTerminal[E, D, V] @unchecked => LazyTerminalSegment(this, s)
      case s => throwSegmentMustBeLastOrWithNext(s) // just to remove warning
    }

  /**
   * Returns segment of lazy sequence that has next segment and corresponds to input `ztruncation`.
   *
   * @see [[makeSegment]].
   */
  protected final def makeSegmentWithNext(ztruncation: ZTruncationWithNext[E, D, V]): LazySegmentWithNext[E, D, V] =
    cacheStable(ztruncation) match {
      case s: ZSegmentInner[E, D, V] @unchecked => LazyInnerSegment(this, s)
      case s: ZSegmentInitial[E, D, V] @unchecked => LazyInitialSegment(this, s)
      case s => throwSegmentMustBeFirstOrWithPrev(s) // just to remove warning
    }

  /**
   * Returns first segment of lazy sequence.
   *
   * @see [[makeSegment]].
   */
  protected final def makeFirstSegment: LazyFirstSegment[E, D, V] =
    cacheStable(zippedSeq.firstSegment.lowerTruncation) match {
      case s: ZSegmentInitial[E, D, V] @unchecked => LazyInitialSegment(this, s)
      case s: ZSegmentSingle[E, D, V] @unchecked => LazySingleSegment(this, s)
      case s => throwSegmentMustBeInitialOrSingle(s) // just to remove warning
    }

  /**
   * Returns last segment of lazy sequence.
   *
   * @see [[makeSegment]].
   */
  protected final def makeLastSegment: LazyLastSegment[E, D, V] =
    cacheStable(zippedSeq.lastSegment.upperTruncation) match {
      case s: ZSegmentTerminal[E, D, V] @unchecked => LazyTerminalSegment(this, s)
      case s: ZSegmentSingle[E, D, V] @unchecked => LazySingleSegment(this, s)
      case s => throwSegmentMustBeTerminalOrSingle(s) // just to remove warning
    }

  protected final def throwSegmentMustHaveOneOfBaseTypes(segment: Segment[E, D, ?]): Nothing = {
    val segmentStr = SetBuilderFormat.segment(segment, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[Any])
    throw new AssertionError(
      s"Expected that segment $segmentStr has one of base types: inner, initial, terminal or single."
    )
  }

  protected final def throwSegmentMustBeLastOrWithNext(segment: Segment[E, D, ?]): Nothing = {
    val segmentStr = SetBuilderFormat.segment(segment, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[Any])
    throw new AssertionError(
      s"Expected that segment $segmentStr is either last or has next segment."
    )
  }

  protected final def throwSegmentMustBeFirstOrWithPrev(segment: Segment[E, D, ?]): Nothing = {
    val segmentStr = SetBuilderFormat.segment(segment, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[Any])
    throw new AssertionError(
      s"Expected that segment $segmentStr is either first or has previous segment."
    )
  }

  protected final def throwSegmentMustBeInitialOrSingle(segment: Segment[E, D, ?]): Nothing = {
    val segmentStr = SetBuilderFormat.segment(segment, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[Any])
    throw new AssertionError(
      s"Expected that segment $segmentStr is either initial or single."
    )
  }

  protected final def throwSegmentMustBeTerminalOrSingle(segment: Segment[E, D, ?]): Nothing = {
    val segmentStr = SetBuilderFormat.segment(segment, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[Any])
    throw new AssertionError(
      s"Expected that segment $segmentStr is either terminal or single."
    )
  }
}

object AbstractLazyTreapSegmentSeq { outer =>

  type ZValue[E, D <: Domain[E], V] = (V, ControlValue[E, D, V])

  type ZSegmentBase[E, D <: Domain[E], V] =
    ZippedSegmentBase[
      E,
      D,
      V,
      ControlValue[E, D, V],
      ZValue[E, D, V],
      TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V],
      TreapSegmentBase[E, D, ControlValue[E, D, V]] | UniformSingleSegment[E, D, ControlValue[E, D, V]]
    ]

  type ZSegment[E, D <: Domain[E], V] = SegmentT[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  type ZSegmentWithNext[E, D <: Domain[E], V] = SegmentT.WithNext[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  type ZSegmentWithPrev[E, D <: Domain[E], V] = SegmentT.WithPrev[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  type ZSegmentInitial[E, D <: Domain[E], V] = SegmentT.Initial[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  type ZSegmentTerminal[E, D <: Domain[E], V] = SegmentT.Terminal[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  type ZSegmentInner[E, D <: Domain[E], V] = SegmentT.Inner[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  type ZSegmentSingle[E, D <: Domain[E], V] = SegmentT.Single[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  type ZTruncation[E, D <: Domain[E], V] = SegmentTruncationT[
    E,
    D,
    ZValue[E, D, V],
    ZSegmentBase[E, D, V],
    ZSegment[E, D, V]
  ]

  type ZTruncationWithPrev[E, D <: Domain[E], V] = SegmentTruncationT[
    E,
    D,
    ZValue[E, D, V],
    ZSegmentBase[E, D, V],
    ZSegmentWithPrev[E, D, V]
  ]

  type ZTruncationWithNext[E, D <: Domain[E], V] = SegmentTruncationT[
    E,
    D,
    ZValue[E, D, V],
    ZSegmentBase[E, D, V],
    ZSegmentWithNext[E, D, V]
  ]

  type ZSegmentSeq[E, D <: Domain[E], V] =
    ZippedSegmentSeq[
      E,
      D,
      V,
      ControlValue[E, D, V],
      ZValue[E, D, V],
      TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V],
      TreapSegmentBase[E, D, ControlValue[E, D, V]] | UniformSingleSegment[E, D, ControlValue[E, D, V]]
    ]

  type BaseSegment[E, D <: Domain[E], V] =
    SegmentT[
      E,
      D,
      V,
      TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V]
    ]

  type BaseSegmentSeq[E, D <: Domain[E], V] = TreapSegmentSeq[E, D, V]

  type ControlSegment[E, D <: Domain[E], V] =
    SegmentT[
      E,
      D,
      ControlValue[E, D, V],
      TreapSegmentBase[E, D, ControlValue[E, D, V]] | UniformSingleSegment[E, D, ControlValue[E, D, V]]
    ]

  type ControlSegmentSeq[E, D <: Domain[E], V] =
    TreapSegmentSeq[
      E,
      D,
      ControlValue[E, D, V]
    ]

  type LazySegment[E, D <: Domain[E], V] =
    SegmentT[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]

  type LazyFirstSegment[E, D <: Domain[E], V] =
    SegmentT.First[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]

  type LazyLastSegment[E, D <: Domain[E], V] =
    SegmentT.Last[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]

  /**
   * Types indicating that segment is stable, i.e. that its value, bounds and type (initial, inner, etc.)
   * will not change after any subsequent computations of lazy values in sequence.
   *
   * <h3>Note</h3>
   *
   * Don't use tagged type in pattern matching:
   * {{{
   *  segment match {
   *    case s: Stable.ZSegmentInner[E, D, V] => ...
   *    case s: Stable.ZSegmentTerminal[E, D, V] => ...
   *    ...
   *  }
   * }}}
   * It fill always fail because segment doesn't have [[Tag]] trait in runtime.
   * We create only typelevel wrapper to mark stable segments (see [[Stable.tagged]]).
   *
   * Pattern match to original types instead:
   * {{{
   *  segment match {
   *    case s: ZSegmentInner[E, D, V] => ...
   *    case s: ZSegmentTerminal[E, D, V] => ...
   *    ...
   *  }
   * }}}
   */
  object Stable { stable =>

    type ZSegment[E, D <: Domain[E], V] = outer.ZSegment[E, D, V] with Tag[Stable.type]

    type ZSegmentWithNext[E, D <: Domain[E], V] = outer.ZSegmentWithNext[E, D, V] with Tag[Stable.type]

    type ZSegmentWithPrev[E, D <: Domain[E], V] = outer.ZSegmentWithPrev[E, D, V] with Tag[Stable.type]

    type ZSegmentInitial[E, D <: Domain[E], V] = outer.ZSegmentInitial[E, D, V] with Tag[Stable.type]

    type ZSegmentTerminal[E, D <: Domain[E], V] = outer.ZSegmentTerminal[E, D, V] with Tag[Stable.type]

    type ZSegmentInner[E, D <: Domain[E], V] = outer.ZSegmentInner[E, D, V] with Tag[Stable.type]

    type ZSegmentSingle[E, D <: Domain[E], V] = outer.ZSegmentSingle[E, D, V] with Tag[Stable.type]

    /**
     * If input `segment` is stable marks it with [[Stable]] tag, otherwise throws exception.
     */
    @throws[AssertionError]("if input segment is not stable")
    def assert[E, D <: Domain[E], V](segment: outer.ZSegment[E, D, V]): stable.ZSegment[E, D, V] =
      if (segment.value._2.isStable) tagged(segment) else throwSegmentIsNotStable(segment)

    /**
     * Marks input segment with [[Stable]] tag. Stability check must be provided by caller.
     */
    def unsafe[E, D <: Domain[E], V](segment: outer.ZSegment[E, D, V]): stable.ZSegment[E, D, V] =
      tagged(segment)

    def throwSegmentIsNotStable[E, D <: Domain[E], V](zsegment: outer.ZSegment[E, D, V]): Nothing =
      throw AssertionError(s"Expected that segment $zsegment is stable.")

    // Private section ---------------------------------------------------------- //
    private def tagged[R](r: R): R with Tag[Stable.type] = r.asInstanceOf[R with Tag[Stable.type]]
  }

  /**
   * Types indicating that segment is eager, i.e. that its value will not change after any subsequent computations
   * of lazy values in sequence.
   *
   * <h3>Note</h3>
   *
   * Don't use tagged type in pattern matching:
   * {{{
   *  segment match {
   *    case s: Eager.ZSegmentInner[E, D, V] => ...
   *    case s: Eager.ZSegmentTerminal[E, D, V] => ...
   *    ...
   *  }
   * }}}
   * It fill always fail because segment doesn't have [[Tag]] trait in runtime.
   * We create only typelevel wrapper to mark eager segments (see [[Eager.tagged]]).
   *
   * Pattern match to original types instead:
   * {{{
   *  segment match {
   *    case s: ZSegmentInner[E, D, V] => ...
   *    case s: ZSegmentTerminal[E, D, V] => ...
   *    ...
   *  }
   * }}}
   */
  object Eager { eager =>

    type ZSegment[E, D <: Domain[E], V] = outer.ZSegment[E, D, V] with Tag[Eager.type]

    type ZSegmentWithNext[E, D <: Domain[E], V] = outer.ZSegmentWithNext[E, D, V] with Tag[Eager.type]

    type ZSegmentWithPrev[E, D <: Domain[E], V] = outer.ZSegmentWithPrev[E, D, V] with Tag[Eager.type]

    type ZSegmentInitial[E, D <: Domain[E], V] = outer.ZSegmentInitial[E, D, V] with Tag[Eager.type]

    type ZSegmentTerminal[E, D <: Domain[E], V] = outer.ZSegmentTerminal[E, D, V] with Tag[Eager.type]

    type ZSegmentInner[E, D <: Domain[E], V] = outer.ZSegmentInner[E, D, V] with Tag[Eager.type]

    type ZSegmentSingle[E, D <: Domain[E], V] = outer.ZSegmentSingle[E, D, V] with Tag[Eager.type]

    /**
     * If input `segment` is eager marks it with [[Eager]] tag, otherwise throws exception.
     */
    @throws[AssertionError]("if input segment is not eager")
    def assert[E, D <: Domain[E], V](segment: outer.ZSegment[E, D, V]): eager.ZSegment[E, D, V] =
      if (segment.value._2.isEager) tagged(segment) else throwSegmentIsNotEager(segment)

    def throwSegmentIsNotEager[E, D <: Domain[E], V](zsegment: outer.ZSegment[E, D, V]): Nothing =
      throw AssertionError(s"Expected that segment $zsegment is eager.")

    // Private section ---------------------------------------------------------- //
    private def tagged[R](r: R): R with Tag[Eager.type] = r.asInstanceOf[R with Tag[Eager.type]]
  }

  sealed trait ControlValue[E, D <: Domain[E], V] {

    def isStable: Boolean

    def isUnstable: Boolean

    def isLazy: Boolean

    def isEager: Boolean

    def isEagerUnstable: Boolean = isEager && isUnstable

    def isLazyOrStable: Boolean = isStable || isLazy
  }

  final case class LazyValue[E, D <: Domain[E], V](
    private val seqFunc: () => SegmentSeq[E, D, V]
  ) extends ControlValue[E, D, V] {

    override def isStable: Boolean = false

    override def isUnstable: Boolean = true

    override def isLazy: Boolean = true

    override def isEager: Boolean = false

    override def isEagerUnstable: Boolean = false

    override def isLazyOrStable: Boolean = true

    def compute: SegmentSeq[E, D, V] = seqFunc.apply()
  }

  final case class EagerValue[E, D <: Domain[E], V] private (
    private val stable: Boolean
  ) extends ControlValue[E, D, V] {

    override def isStable: Boolean = stable

    override def isUnstable: Boolean = !stable

    override def isLazy: Boolean = false

    override def isEager: Boolean = true

    override def isEagerUnstable: Boolean = !stable

    override def isLazyOrStable: Boolean = stable

    override def toString: String = s"EagerValue(${if (stable) "stable" else "unstable"})"
  }

  object EagerValue {

    def cons[E, D <: Domain[E], V](isStable: Boolean): EagerValue[E, D, V] = if (isStable) stable else unstable

    def stable[E, D <: Domain[E], V]: EagerValue[E, D, V] = stableInstance.asInstanceOf

    def unstable[E, D <: Domain[E], V]: EagerValue[E, D, V] = unstableInstance.asInstanceOf

    private lazy val stableInstance: EagerValue[Any, Domain[Any], Any] = new EagerValue(true)

    private lazy val unstableInstance: EagerValue[Any, Domain[Any], Any] = new EagerValue(false)
  }

  final class ControlValueHash[E, D <: Domain[E], V]
    extends Hash[ControlValue[E, D, V]] {

    import util.HashUtil._

    override def hash(x: ControlValue[E, D, V]): Int = x match {
      case x: LazyValue[_, _, _] => System.identityHashCode(x)
      case x: EagerValue[_, _, _] => product1Hash(x.isStable.##)
    }

    override def eqv(x: ControlValue[E, D, V], y: ControlValue[E, D, V]): Boolean = (x, y) match {
      case (x: LazyValue[_, _, _], y: LazyValue[_, _, _]) => x.eq(y)
      case (x: EagerValue[_, _, _], y: EagerValue[_, _, _]) => x.isStable == y.isStable
      case _ => false
    }
  }

  object ControlValueHash {

    def get[E, D <: Domain[E], V]: Hash[ControlValue[E, D, V]] = instance.asInstanceOf

    private lazy val instance: ControlValueHash[Any, Domain[Any], Any] = new ControlValueHash
  }

  final class ControlValueOps[E, D <: Domain[E], V](
    override val unit: ControlValue[E, D, V] = EagerValue.stable[E, D, V],
    override val valueHash: Hash[ControlValue[E, D, V]] = ControlValueHash.get[E, D, V],
    override val valueIncl: InclusionPredicate[ControlValue[E, D, V]] = InclusionPredicate.alwaysIncluded
  ) extends ValueOps[ControlValue[E, D, V]]

  object ControlValueOps {

    def get[E, D <: Domain[E], V]: ValueOps[ControlValue[E, D, V]] = instance.asInstanceOf

    private lazy val instance: ControlValueOps[Any, Domain[Any], Any] = new ControlValueOps()
  }

  object ZValueOps {

    def get[E, D <: Domain[E], V](valueOps: ValueOps[V]): ValueOps[ZValue[E, D, V]] =
      new ValueOps.Tuple2Impl[V, ControlValue[E, D, V]](
        InclusionPredicate.alwaysIncluded,
        valueOps,
        ControlValueOps.get
      )
  }

  /**
   * Base trait for output segments of [[AbstractLazyTreapSegmentSeq]].
   *
   * Wraps [[ZSegment]] - segment with internal representation of lazy sequence and
   * provides api to interact with lazy sequence.
   */
  sealed trait LazySegmentBase[E, D <: Domain[E], V]
    extends MappedSegmentLikeT[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]] {

    // Inspection --------------------------------------------------------------- //
    override def sequence: LazySegmentSeq[E, D, V]

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: LazyFirstSegment[E, D, V] = sequence.firstSegment

    override def moveToLast: LazyLastSegment[E, D, V] = sequence.lastSegment

    override def moveToBound(bound: Bound[E]): LazySegment[E, D, V] = sequence.getSegmentForBound(bound)

    override def moveToExtended(bound: ExtendedBound[E]): LazySegment[E, D, V] = sequence.getSegmentForExtended(bound)

    override def moveToElement(element: E): LazySegment[E, D, V] = sequence.getSegmentForElement(element)

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: SegmentSeq[E, D, V] = ???

    override def takeBelow: SegmentSeq[E, D, V] = ???

    override def slice: (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = ???

    override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

    override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] = ???

    override def lowerTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] = ???

    override def upperTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] = ???

    // Protected section -------------------------------------------------------- //
    override protected def original: Stable.ZSegment[E, D, V]

    final override protected val mapFunc: ZValue[E, D, V] => V = t => t._1

    final override protected def cons(truncation: ZTruncation[E, D, V]): LazySegment[E, D, V] =
      sequence.makeSegment(truncation)

    final override protected def consWithNext(truncation: ZTruncationWithNext[E, D, V]): LazySegmentWithNext[E, D, V] =
      sequence.makeSegmentWithNext(truncation)

    final override protected def consWithPrev(truncation: ZTruncationWithPrev[E, D, V]): LazySegmentWithPrev[E, D, V] =
      sequence.makeSegmentWithPrev(truncation)
  }

  /**
   * Lazy segment with next segment.
   */
  sealed trait LazySegmentWithNext[E, D <: Domain[E], V]
    extends MappedSegmentT.WithNext[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
      with LazySegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazySegmentWithNext[E, D, V]

    // Navigation --------------------------------------------------------------- //
    override def moveNext: LazySegmentWithPrev[E, D, V] = {
      val nextSegment = original.moveNext
      sequence.makeSegmentWithPrev(nextSegment.truncation(nextSegment.lowerBound))
    }

    // Protected section -------------------------------------------------------- //
    protected override def original: Stable.ZSegmentWithNext[E, D, V]
  }

  /**
   * Lazy segment with previous segment.
   */
  sealed trait LazySegmentWithPrev[E, D <: Domain[E], V]
    extends MappedSegmentT.WithPrev[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
      with LazySegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazySegmentWithPrev[E, D, V]

    // Navigation --------------------------------------------------------------- //
    override def movePrev: LazySegmentWithNext[E, D, V] = {
      val prevSegment = original.movePrev
      sequence.makeSegmentWithNext(prevSegment.truncation(prevSegment.upperBound))
    }

    // Protected section -------------------------------------------------------- //
    protected override def original: Stable.ZSegmentWithPrev[E, D, V]
  }

  /**
   * Lazy initial segment.
   */
  final case class LazyInitialSegment[E, D <: Domain[E], V](
    override val sequence: LazySegmentSeq[E, D, V],
    override protected val original: Stable.ZSegmentInitial[E, D, V]
  ) extends MappedSegmentT.Initial[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazyInitialSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: LazyInitialSegment[E, D, V] = this
  }

  /**
   * Lazy terminal segment.
   */
  final case class LazyTerminalSegment[E, D <: Domain[E], V](
    override val sequence: LazySegmentSeq[E, D, V],
    override protected val original: Stable.ZSegmentTerminal[E, D, V]
  ) extends MappedSegmentT.Terminal[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentWithPrev[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazyTerminalSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: LazyTerminalSegment[E, D, V] = this
  }

  /**
   * Lazy inner segment.
   */
  final case class LazyInnerSegment[E, D <: Domain[E], V](
    override val sequence: LazySegmentSeq[E, D, V],
    override protected val original: Stable.ZSegmentInner[E, D, V]
  ) extends MappedSegmentT.Inner[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentWithPrev[E, D, V]
    with LazySegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazyInnerSegment[E, D, V] = this
  }

  /**
   * Lazy single segment.
   */
  final case class LazySingleSegment[E, D <: Domain[E], V](
    override val sequence: LazySegmentSeq[E, D, V],
    override protected val original: Stable.ZSegmentSingle[E, D, V]
  ) extends MappedSegmentT.Single[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazySingleSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: LazySingleSegment[E, D, V] = this

    override def moveToLast: LazySingleSegment[E, D, V] = this

    override def moveToBound(bound: Bound[E]): LazySingleSegment[E, D, V] = this

    override def moveToExtended(bound: ExtendedBound[E]): LazySingleSegment[E, D, V] = this

    override def moveToElement(element: E): LazySingleSegment[E, D, V] = this
  }
}