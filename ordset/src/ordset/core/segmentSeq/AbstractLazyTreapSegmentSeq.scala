package ordset.core.segmentSeq

import ordset.Hash
import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap
import ordset.core.segmentSeq.internal.mappedSeq.{MappedSegmentLikeT, MappedSegmentT}
import ordset.core.segmentSeq.internal.lazySeq.*
import ordset.core.segmentSeq.internal.lazySeq.ControlValue.*
import ordset.core.segmentSeq.internal.SegmentSeqExceptionUtil.*
import ordset.core.segmentSeq.map.{NonuniformTreapOrderedMap, TreapOrderedMap, UniformOrderedMap, ZippedOrderedMap}
import ordset.core.segmentSeq.AbstractZippedSegmentSeq.*
import ordset.core.segmentSeq.AbstractUniformSegmentSeq.*
import ordset.core.segmentSeq.AbstractLazyTreapSegmentSeq.*
import ordset.core.segmentSeq.util.{SegmentSeqUtil, TreapSegmentSeqBuilder, TreapSegmentSeqUtil}
import ordset.random.{RngManager, UnsafeUniformRng}
import ordset.util.BooleanUtil
import ordset.util.HashUtil.product1Hash
import ordset.util.types.Tag

import scala.annotation.tailrec
import java.util.concurrent.atomic.AtomicReference

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
 * Base sequence - is a treap sequence that contains computed (strict) values.
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
 * Control sequence - is a treap sequence that specifies which part of base sequence is lazy and which is strict.
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
 *  u - unstable strict segment: value has been computed, but segment type is undefined (initial, inner, terminal or
 *      single) because of some adjacent segments are lazy.
 *
 *  s - stable strict segment: value had been computed and segment type is defined.
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
 * External api of the class is represented by lazy segments that wrap zipped segments. The basic rule is following:
 * <tr><u>only stable segments are exposed outside.</u></tr>
 * <tr></tr>
 * Lazy and strict unstable segments are transformed into stable one on demand. In that case:
 * <tr>- new versions of base, control and zipped sequences are created such that required segment become stable;</tr>
 * <tr>- new versions of sequences are cached (with synchronization);</tr>
 * <tr>- stable segment is created and returned.</tr>
 *
 * <h3>Stability condition</h3>
 *
 * Strict segment is stable iff it has no adjacent segments such that:
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
 *                     /   A        A   \
 *     unstable adjacent               segment is unstable
 *       segment with
 *      the same value
 * }}}
 */
abstract class AbstractLazyTreapSegmentSeq[E, D[X] <: Domain[X], V]
  extends AbstractSegmentSeq[E, D, V, LazySegmentBase[E, D, V]] {

  // Inspection --------------------------------------------------------------- //
  /**
   * Returns zipped sequence - internal representation of lazy sequence (see class description).
   * 
   * Warning! When segments are accessed state of lazy sequence is modified. So different calls of method may return
   * different instances of zipped sequence.
   */ 
  final def getZippedSeq: ZSegmentSeq[E, D, V] = zippedSeqRef.get.nn

  final override def isEmpty: Boolean = isUniform && !isValueIncluded(firstSegment.value)

  final override def isUniversal: Boolean = isUniform && isValueIncluded(firstSegment.value)

  final override def isUniform: Boolean = firstSegment.isSingle

  final override def includesBound(bound: Bound[E]): Boolean = super.includesBound(bound)

  final override def includesExtended(bound: ExtendedBound[E]): Boolean = super.includesExtended(bound)

  final override def includesElement(element: E): Boolean = super.includesElement(element)

  final override def toString: String = getZippedSeq.toString

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds

  final override def extendedUpperBounds: Iterable[ExtendedBound.Upper[E]] = super.extendedUpperBounds

  // We shouldn't use lazy val here like we do it for other types of sequences.
  // Memoized lazy segment will grab the first segment of `zippedSeq` (i.e. of our cache), 
  // and it in turn will grab the initial version of `zippedSeq`. This version will become
  // outdated after computation of any lazy value of current lazy sequence. 
  // So it's a waste of memory to store old cache version for a long time.
  final override def firstSegment: LazyFirstSegment[E, D, V] = makeFirstSegment

  // We shouldn't use lazy val (see comment for `firstSegment`).
  final override def lastSegment: LazyLastSegment[E, D, V] = makeLastSegment

  final override def getSegmentForBound(bound: Bound[E]): LazySegment[E, D, V] =
    makeSegment(getZippedSeq.getSegmentForBound(bound).truncation(bound))

  final override def getSegmentForExtended(bound: ExtendedBound[E]): LazySegment[E, D, V] =
    super.getSegmentForExtended(bound)

  final override def getSegmentForElement(element: E): LazySegment[E, D, V] =
    super.getSegmentForElement(element)

  final override def getValueForBound(bound: Bound[E]): V =
    cacheStrict(getZippedSeq.getSegmentForBound(bound).truncation(bound)).value._1

  final override def getValueForExtended(bound: ExtendedBound[E]): V =
    cacheStrict(getZippedSeq.getSegmentForExtended(bound).truncation(bound)).value._1

  final override def getValueForElement(element: E): V =
    super.getValueForElement(element)

  // Transformation ----------------------------------------------------------- //
  final override def takeAboveBound(bound: Bound[E]): LazySegmentSeq[E, D, V] =
    takeAboveBoundInternal(bound, getZippedSeq.getSegmentForBound(bound))

  final override def takeAboveExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] =
    super.takeAboveExtended(bound)

  final override def takeBelowBound(bound: Bound[E]): LazySegmentSeq[E, D, V] =
    takeBelowBoundInternal(bound, getZippedSeq.getSegmentForBound(bound))

  final override def takeBelowExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] =
    super.takeBelowExtended(bound)

  final override def sliceAtBound(bound: Bound[E]): (LazySegmentSeq[E, D, V], LazySegmentSeq[E, D, V]) = {
    val zsegment = getZippedSeq.getSegmentForBound(bound)
    (takeBelowBoundInternal(bound, zsegment), takeAboveBoundInternal(bound, zsegment))
  }

  final override def sliceAtExtended(bound: ExtendedBound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    super.sliceAtExtended(bound)

  final override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    prependBelowExtended(firstSegment.upper, other)

  final override def prependBelowBound(bound: Bound[E], other: SegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] =
    prependBelowBoundInternal(bound, getZippedSeq.getSegmentForBound(bound.provideLower), other)

  final override def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    super.prependBelowExtended(bound, other)

  final override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    appendAboveExtended(lastSegment.lower, other)

  final override def appendAboveBound(bound: Bound[E], other: SegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] =
    appendAboveBoundInternal(bound, getZippedSeq.getSegmentForBound(bound.provideUpper), other)

  final override def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    super.appendAboveExtended(bound, other)

  final override def patchLazy(supplierSeq: SupplierSegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    patchLazyDefaultInternal(supplierSeq)

  final override def strict: StrictSegmentSeq[E, D, V] = {
    // Provide optimized implementation compared with `defaultStrict` method. 
    cacheStrictTotal()
    // `firstSeq` is already strict after execution of `cacheStrictTotal`, so `firstSeq.strict` is here just to
    // cast type and has no additional performance overhead.
    getZippedSeq.firstSeq.strict
  }

  // Protected section -------------------------------------------------------- //
  protected final override type SegmentInternal = ZSegment[E, D, V]

  /**
   * Reference to zipped sequence which joins `baseSeq` and `controlSeq`. 
   * <tr>`baseSeq` - is a cache with already computed values.</tr>
   * <tr>`controlSeq` - contains lazy values, that hadn't been computed yet.</tr>
   * 
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
  protected val zippedSeqRef: AtomicReference[ZSegmentSeq[E, D, V]]

  protected override def isValueIncluded(value: V): Boolean

  protected override def consUniform(value: V): LazySegmentSeq[E, D, V]

  /**
   * Creates lazy sequence from specified zipped sequence.
   */
  protected def consLazy(zippedSeq: ZSegmentSeq[E, D, V]): LazySegmentSeq[E, D, V]

  /**
   * <tr>1. If segment of input `ztruncation` is stable then returns it.</tr>
   * <tr>2. Otherwise:</tr>
   * <tr>
   *   2.1. Applies [[provideStableSegment]] to input truncation to build new zipped sequence Z with stable segment
   *   at bound `ztruncation.bound`.
   * </tr>
   * <tr>
   *   2.2. Saves new sequence Z into `zippedSeqRef`.
   * </tr>
   * <tr>
   *   2.3. Returns segment of new sequence Z at bound `ztruncation.bound`.
   * </tr>
   * <tr></tr>
   */
  protected final def cacheStable(ztruncation: ZTruncation[E, D, V]): Stable.ZSegment[E, D, V] = {
    import scala.language.unsafeNulls
    // Input `ztruncation` can be out of date, due to cache was modified (i.e. zipped sequence inside `zippedSeqRef`).
    // Check it and refresh if needed.
    var zt = provideActualZtruncation(ztruncation, getZippedSeq)
    // If segment of `ztruncation` is already stable, then just return it.
    if (zt.segment.value._2.isStable) Stable.unsafe(zt.segment)
    // Otherwise compute lazy values and update cache.
    else {
      var zs = zt.segment
      var success = false
      while (!success) {
        val seq = getZippedSeq
        zt = provideActualZtruncation(zt, seq)
        zs = provideStableSegment(zt)
        success = zippedSeqRef.compareAndSet(seq, zs.self.sequence)
      }
      Stable.unsafe(zs)
    }
  }
  
  /**
   * <tr>1. If segment of input `ztruncation` is strict then returns it.</tr>
   * <tr>2. Otherwise:</tr>
   * <tr>
   *   2.1. Applies [[provideStrictSeq]] to input segment and receives new zipped sequence Z with strict segments
   *   between bounds of `ztruncation.segment`.
   * </tr>
   * <tr>
   *   2.2. Saves new sequence Z into `zippedSeqRef`.
   * </tr>
   * <tr>
   *   2.3. Returns segment of new sequence Z at bound `ztruncation.bound`.
   * </tr>
   * <tr></tr>
   */
  protected final def cacheStrict(ztruncation: ZTruncation[E, D, V]): Strict.ZSegment[E, D, V] = {
    import scala.language.unsafeNulls
    // Input `ztruncation` can be out of date, due to cache was modified (i.e. zipped sequence inside `zippedSeqRef`).
    // Check it and refresh if needed.
    var seq = getZippedSeq
    var zt = provideActualZtruncation(ztruncation, seq)
    // If segment of `ztruncation` is already strict, then just return it.
    if (zt.segment.value._2.isStrict) Strict.unsafe(zt.segment)
    // Otherwise compute lazy values and update cache.
    else {
      var newSeq = seq
      var success = false
      while (!success) {
        seq = getZippedSeq
        zt = provideActualZtruncation(zt, seq)
        newSeq = provideStrictSeq(zt.segment)
        success = zippedSeqRef.compareAndSet(seq, newSeq)
      }
      Strict.assert(newSeq.getSegmentForExtended(ztruncation.bound))
    }
  }

  /**
   * <tr>
   *   1. Takes current state of zipped sequence inside `zippedSeqRef`, computes all lazy values in it and builds 
   *      new totally strict zipped sequence.
   * </tr>
   * <tr>
   *   2. Saves new sequence into `zippedSeqRef`.
   * </tr>
   */
  protected final def cacheStrictTotal(): Unit =
    // If zipped sequence is already totally strict, then do nothing.
    if (!ZSegmentSeqUtil.isTotallyStrict(getZippedSeq)) {
      // Compute all lazy values and build totally strict zipped sequence.
      val newSeq = ZSegmentSeqBuilder.strictZippedSeq(getZippedSeq)
      // We can just set up new value here avoiding `compareAndSet` iterations. 
      // Other thread can write new value into `zippedSeqRef` during computation of `newSeq` in the current thread.
      // But for both versions of initial zipped sequence inside `zippedSeqRef` current thread would get the same 
      // `newSeq`.
      zippedSeqRef.set(newSeq)
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
   *   2. Apply [[provideStrictSeq]] to input segment to compute lazy value (if required) and get zipped sequence Z,,1,,
   *   that contains only strict segments between bounds of input segment. These segments however may be both stable
   *   and unstable. If segment of Z,,1,, at bound `ztruncation.bound` is stable then return this segment otherwise
   *   go to step 3 with Z = Z,,1,,.
   * </tr>
   * <tr>
   *   3. Get segment of sequence Z at bound `ztruncation.bound` and try to move to the previous segment:
   *   <p>
   *     3.1. If lazy segment was found then apply [[provideStrictSeq]] to it and receive sequence Z,,2,,.
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
   *     4.1. If lazy segment was found then apply [[provideStrictSeq]] to it and receive sequence Z,,3,,.
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
   *  (..., u) - control value: s - strict stable, u - strict unstable, ? - lazy.
   * }}}
   */
  protected final def provideStableSegment(ztruncation: ZTruncation[E, D, V]): Stable.ZSegment[E, D, V] = {

    @tailrec
    def stabilizeLowerBound(zsegment: ZSegment[E, D, V], bound: ExtendedBound[E]): ZSegment[E, D, V] =
      zsegment match {
        case s: ZSegmentWithPrev[E, D, V] =>
          val newZippedSeq = provideStrictSeq(s.movePrev)
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
          val newZippedSeq = provideStrictSeq(s.moveNext)
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
      val zsegment1 = provideActualZsegment(zsegment, bound, provideStrictSeq(zsegment))
      if (zsegment1.value._2.isStable) Stable.unsafe(zsegment1.self)
      else {
        val zsegment2 = stabilizeLowerBound(zsegment1, bound)
        if (zsegment2.value._2.isStable) Stable.unsafe(zsegment2.self)
        else Stable.assert(stabilizeUpperBound(zsegment2, bound))
      }
    }
  }

  /**
   * Returns zipped sequence which contains strict segments (stable or unstable) between bounds of input `zsegment`.
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
  protected final def provideStrictSeq(zsegment: ZSegment[E, D, V]): ZSegmentSeq[E, D, V] =
    zsegment.value._2 match {
      case lazyValue: LazyValue[E, D, V] =>
        val seq = lazyValue.compute
        val newBaseSeq = patchBaseSeq(zsegment, seq)
        val newControlSeq = patchControlSeq(zsegment, newBaseSeq)
        makeZippedSeq(newBaseSeq, newControlSeq)
      case _ =>
        zsegment.self.sequence
    }

  /**
   * Returns input `zsegment`, if it belongs to specified `zippedSeq`, otherwise returns segment of `zippedSeq`
   * at specified `bound`.
   */
  protected inline def provideActualZsegment(
    zsegment: ZSegment[E, D, V],
    bound: ExtendedBound[E],
    zippedSeq: ZSegmentSeq[E, D, V],
  ): ZSegment[E, D, V] =
    if (zippedSeq.eq(zsegment.sequence)) zsegment
    else zippedSeq.getSegmentForExtended(bound)

  /**
   * Returns input `ztruncation`, if it belongs to specified `zippedSeq`, otherwise returns truncation of `zippedSeq`
   * at the same bound as input `ztruncation`.
   */
  protected inline def provideActualZtruncation(
    ztruncation: ZTruncation[E, D, V],
    zippedSeq: ZSegmentSeq[E, D, V],
  ): ZTruncation[E, D, V] =
    if (zippedSeq.eq(ztruncation.sequence)) ztruncation
    else {
      val bound = ztruncation.bound
      zippedSeq.getSegmentForExtended(bound).truncation(bound)
    }

  /**
   * Same as [[SegmentSeqT.takeAboveBound]] but with additional argument `boundZsegment` such that:
   * {{{
   *   boundZsegment.containsBound(bound) == true     (1)
   * }}}
   * It allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note, if provided segment differs from one defined by condition 1, the behavior of method is undefined.
   */
  protected final def takeAboveBoundInternal(
    bound: Bound[E],
    boundZsegment: ZSegment[E, D, V]
  ): LazySegmentSeq[E, D, V] = {

    // 1) `boundZsegment` is strict unstable.
    // 1.1) Next segment N1 is lazy => it will remain lazy after transformation => `boundZsegment` will remain
    //      strict unstable => no correction is required, just apply `takeAboveBound` to base and control sequences
    //      and build new lazy sequence from them.
    //
    //            `bound`
    //               |
    //               u            ?
    // ------)[------------)[----------)[--------  initial sequence
    //        `boundZsegment`     N1
    //
    //          u                 ?
    // --------------------)[----------)[--------  output sequence
    //                            N1
    //
    // 1.2) Next segment N1 is stable => previous segment P1 is lazy => P1 will be dropped by transformation =>
    //      `boundZsegment` will become stable.
    //
    //            `bound`
    //               |
    //   ?           u            s
    // ------)[------------)[----------)[--------  initial sequence
    //   P1   `boundZsegment`     N1
    //
    //          s                 s
    // --------------------)[----------)[--------  output sequence
    //                            N1
    //
    // 1.3) Next segment N1 is strict unstable => next segment N2 is lazy;
    //      `boundZsegment` and N1 have different base values (otherwise they can't be separate segments) =>
    //      stability condition is satisfied for right side of `boundZsegment` (see class description) =>
    //      case is equivalent to p.1.2 => `boundZsegment` will become stable.
    //
    //            `bound`
    //               |
    //   ?           u            u          ?
    // ------)[------------)[----------)[--------  initial sequence
    //   P1   `boundZsegment`     N1        N2
    //
    //          s                 u          ?
    // --------------------)[----------)[--------  output sequence
    //                            N1        N2
    //
    // 2) `boundZsegment` is stable.
    //
    // Dropping either lower or upper bound of stable segment can't change its stability => no correction.
    //
    //            `bound`
    //               |
    //               s            u          ?
    // ------)[------------)[----------)[--------  initial sequence
    //        `boundZsegment`     N1        N2
    //
    //          s                 u          ?
    // --------------------)[----------)[--------  output sequence
    //                            N1        N2
    //
    // 3) `boundZsegment` is lazy.
    //
    // Regardless of adjacent segments one kind of correction is obligatory: function f that computes lazy value
    // of `boundZsegment` must be replaced with `() => f.apply().takeAboveBound(bound)` in output sequence.
    // I.e. we must also restrict sequence that hadn't been computed yet.
    //
    // Consider example:
    //
    //            `bound`
    //               |
    //                    ? (function f)
    // X----------------------------------------X  initial sequence (seq)
    //
    //      A              B              C        base values
    // X--------](------------------)[----------X  sequence that will be returned by function f
    //
    // Assume we applied `seq.takeAboveBound(bound)` without any correction:
    //
    //                    ? (function f)
    // X----------------------------------------X  seq.takeAboveBound(bound)  // wrong case
    //
    // And then we compute lazy value:
    //
    //     A               B              C        base values
    // X--------](------------------)[----------X  seq.takeAboveBound(bound) with computed lazy value  // wrong case
    //
    // We have got wrong answer. The right sequence has dropped bounds below specified `bound`:
    //
    //            `bound`
    //               |
    //                    B               C        base values
    // X----------------------------)[----------X  output sequence
    //
    // To receive correct result we should replace function f when `takeAboveBound` transformation was applied.
    // New function should apply `takeAboveBound` to the sequence returned by function f.
    //
    // In case of lazy segment no additional corrections of stability indicators are required (see below).
    //
    // 3.1) Next segment N1 is lazy => no correction.
    //
    //            `bound`
    //               |
    //               ?            ?
    // ------)[------------)[----------)[--------  initial sequence
    //        `boundZsegment`     N1
    //
    //          ?                 ?
    // --------------------)[----------)[--------  output sequence
    //                            N1
    //
    // 3.2) Next segment is strict unstable => no correction.
    //
    //            `bound`
    //               |
    //               ?            u
    // ------)[------------)[----------)[--------  initial sequence
    //        `boundZsegment`     N1
    //
    //          ?                 u
    // --------------------)[----------)[--------  output sequence
    //                            N1

    boundZsegment.value._2 match {
      // p.3
      case lazyValue: LazyValue[E, D, V] =>
        val newLazyValue = lazyValue.takeAboveBound(bound)
        val newBaseSeq = TreapSegmentSeqUtil.takeAboveSegment(boundZsegment.self.firstSeqSegment.self)
        val newControlSeq = TreapSegmentSeqUtil.prependBelowTruncation(
          boundZsegment.self.secondSeqUpperTruncation,
          makeUniformControlSeq(newLazyValue)
        )
        consLazy(makeZippedSeq(newBaseSeq, newControlSeq))

      case strictValue =>
        // p.1
        if (strictValue.isUnstable) {
          boundZsegment match {
            case boundZsegment: ZSegmentWithNext[E, D, V] =>
              // p.1.1
              if (boundZsegment.moveNext.value._2.isLazy) {
                SliceOps.takeAboveZSegmentWithoutCorrection(boundZsegment, this)
                // p.1.2 and p.1.3
              } else {
                val newBaseSeq = TreapSegmentSeqUtil.takeAboveSegment(boundZsegment.self.firstSeqSegment.self)
                val newControlSeq = TreapSegmentSeqUtil.prependBelowTruncation(
                  boundZsegment.self.secondSeqUpperTruncation,
                  makeUniformControlSeq(StrictValue.stable)
                )
                consLazy(makeZippedSeq(newBaseSeq, newControlSeq))
              }
            // same as p.1.2 (right bound of `boundZsegment` is stable)
            case _ =>
              consUniform(boundZsegment.value._1)
          }
        // p.2
        } else {
          SliceOps.takeAboveZSegmentWithoutCorrection(boundZsegment, this)
        }
    }
  }

  /**
   * Same as [[SegmentSeqT.takeBelowBound]] but with additional argument `boundZsegment` such that:
   * {{{
   *   boundZsegment.containsBound(bound) == true     (1)
   * }}}
   * It allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note, if provided segment differs from one defined by condition 1, the behavior of method is undefined.
   */
  protected final def takeBelowBoundInternal(
    bound: Bound[E],
    boundZsegment: ZSegment[E, D, V]
  ): LazySegmentSeq[E, D, V] = {

    // We can use the same reasoning as for `takeAboveBoundInternal`.

    boundZsegment.value._2 match {
      // p.3
      case lazyValue: LazyValue[E, D, V] =>
        val newLazyValue = lazyValue.takeBelowBound(bound)
        val newBaseSeq = TreapSegmentSeqUtil.takeBelowSegment(boundZsegment.self.firstSeqSegment.self)
        val newControlSeq = TreapSegmentSeqUtil.appendAboveTruncation(
          boundZsegment.self.secondSeqLowerTruncation,
          makeUniformControlSeq(newLazyValue)
        )
        consLazy(makeZippedSeq(newBaseSeq, newControlSeq))

      case strictValue =>
        // p.1
        if (strictValue.isUnstable) {
          boundZsegment match {
            case boundZsegment: ZSegmentWithPrev[E, D, V] =>
              // p.1.1
              if (boundZsegment.movePrev.value._2.isLazy) {
                SliceOps.takeBelowZSegmentWithoutCorrection(boundZsegment, this)
                // p.1.2 and p.1.3
              } else {
                val newBaseSeq = TreapSegmentSeqUtil.takeBelowSegment(boundZsegment.self.firstSeqSegment.self)
                val newControlSeq = TreapSegmentSeqUtil.appendAboveTruncation(
                  boundZsegment.self.secondSeqLowerTruncation,
                  makeUniformControlSeq(StrictValue.stable)
                )
                consLazy(makeZippedSeq(newBaseSeq, newControlSeq))
              }
            // same as p.1.2 (right bound of `boundZsegment` is stable)
            case _ =>
              consUniform(boundZsegment.value._1)
          }
          // p.2
        } else {
          SliceOps.takeBelowZSegmentWithoutCorrection(boundZsegment, this)
        }
    }
  }

  protected final override def prependBelowBoundInternal(
    bound: Bound[E],
    originalBoundZsegment: ZSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): LazySegmentSeq[E, D, V] = {
    val otherZippedSeq = makeZippedSeqForTransformation(other)
    val otherBoundZsegment = otherZippedSeq.getSegmentForBound(bound.provideUpper)
    consLazy(ZSegmentSeqBuilder.appendZippedSegment(bound, otherBoundZsegment, originalBoundZsegment))
  }

  protected final override def prependBelowExtendedInternal(
    bound: ExtendedBound[E],
    originalBoundSegment: ZSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    super.prependBelowExtendedInternal(bound, originalBoundSegment, other)

  protected final override def appendAboveBoundInternal(
    bound: Bound[E],
    originalBoundZsegment: ZSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): LazySegmentSeq[E, D, V] = {
    val otherZippedSeq = makeZippedSeqForTransformation(other)
    val otherBoundZsegment = otherZippedSeq.getSegmentForBound(bound.provideLower)
    consLazy(ZSegmentSeqBuilder.appendZippedSegment(bound, originalBoundZsegment, otherBoundZsegment))
  }

  protected final override def appendAboveExtendedInternal(
    bound: ExtendedBound[E],
    originalBoundSegment: ZSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    super.appendAboveExtendedInternal(bound, originalBoundSegment, other)

  /**
   * Creates zipped sequence with specified base and control sequences.
   */
  protected final def makeZippedSeq(
    baseSeq: BaseSegmentSeq[E, D, V],
    controlSeq: ControlSegmentSeq[E, D, V]
  ): ZSegmentSeq[E, D, V] =
    ZippedOrderedMap.apply(
      baseSeq,
      controlSeq,
      ZValue.operator(valueOps),
      ZValue.baseInvariant,
      ZValue.controlInvariant
    )(
      domainOps,
      getZippedSeq.valueOps,
      rngManager
    )

  /**
   * Converts `other` sequence into zipped one for subsequent transformation operation (prepend/append):
   * <tr>
   *   - If `other` is [[AbstractLazyTreapSegmentSeq]] then extracts its zipped sequence.
   *   Subsequent transformation operation will merge treaps of base sequences and treaps of
   *   control sequences keeping all lazy parts of both sequences non-computed.
   * </tr>
   * <tr>
   *   - If `other` is [[TreapSegmentSeq]] then converts it into totally stable zipped sequence.
   *   Subsequent transformation operation will immediately merge treaps of base sequences.
   *   It's rather cheap operation and there is no point to make it lazy to delay execution.
   * </tr>
   * <tr>
   *   - Otherwise builds totally lazy zipped sequence. So subsequent transformation operation will not
   *   convert `other` into treap sequence immediately. Conversion will be delayed until it's really necessary.
   * </tr>
   */
  protected final def makeZippedSeqForTransformation(other: SegmentSeq[E, D, V]): ZSegmentSeq[E, D, V] =
    other match {
      case other: AbstractLazyTreapSegmentSeq[E, D, V] =>
        other.getZippedSeq
      case other: TreapSegmentSeq[E, D, V] =>
        makeZippedSeq(other, makeUniformControlSeq(StrictValue.stable))
      case _ =>
        makeUniformLazyZippedSeq(() => other)
    }

  /**
   * Creates uniform zipped sequence with lazy value `seqFunc`.
   */
  protected final def makeUniformLazyZippedSeq(
    seqFunc: () => SegmentSeq[E, D, V]
  ): ZSegmentSeq[E, D, V] =
    makeZippedSeq(
      makeUniformBaseSeq(valueOps.unit),
      makeUniformControlSeq(LazyValue.Default(seqFunc)(domainOps.domain))
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
   *         zsegment.lower         zsegment.upper
   * }}}
   * where u - unstable strict segment; s - stable strict segment; ? - lazy segment.
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
   * where u - unstable strict segment; s - stable strict segment; ? - lazy segment.
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
    //    - if previous segment is strict unstable then define `patchLowerBound` as lower bound of previous segment.
    //
    //    Similarly define `patchUpperBound`.
    //
    //      ?      u       u                         u       s
    //    ----](------)[------](----------------](------)[--------   zipped seq
    //                 ^            zsegment            ^
    //          patchLowerBound                   patchUpperBound
    //
    //    Let's call patch bound shifted if `patchLowerBound` != `zsegment.lower` and non-shifted otherwise.
    //
    // 2. Define isolation flag for lower bound of `zsegment`:
    //    - if left adjacent segment of `zsegment` is strict unstable and has the same base value as new base sequence
    //    at lower bound of `zsegment` then bound is non-isolated, otherwise - isolated.
    //
    //    Similarly define isolation flag for upper bound of `zsegment`.
    //
    // 3. If patch lower bound is shifted then define first segment of patch sequence as segment of zipped sequence
    //    at bound `patchLowerBound` with control value:
    //    - strict stable iff all its adjacent segments are strict;
    //    - strict unstable otherwise.
    //
    //    If patch upper bound is shifted then similarly define last segment of patch sequence.
    //
    // 4. Call method `makeControlSeq` with parameters:
    //    - `stableLowerBound` = (lower bound of `zsegment` satisfies stability condition*)
    //    - `stableUpperBound` = (upper bound of `zsegment` satisfies stability condition*)
    //    * see class description.
    //
    //                     lower bound     upper bound
    //      ?      u       u   |                |        ?
    //    ----](------)[------](----------------](----------------   zipped seq
    //            S1   |   S2       zsegment    |        S3
    //        patchLowerBound                 patchUpperBound
    //
    //    In example above we have:
    //
    //    - `stableLowerBound` == `true`:   left adjacent segment of `zsegment` (S2) is strict unstable and assume
    //                                      it has the same value as `zsegment` but its left adjacent S1 is strict.
    //
    //    - `stableUpperBound` == `false`:  left adjacent segment of `zsegment` (S3) is lazy.
    //
    //    Let's denote received sequence as `innerPatchSequence`.
    //
    //    If both patch bounds are non-shifted then insert `innerPatchSequence` into old control sequence within
    //    `patchLowerBound` and `patchUpperBound` and return the result.
    //
    //    Otherwise go to step 5.
    //
    // 5. Merge `innerPatchSequence` with first or last segment of patch sequence obtained at step 3 (see 5.1 and 5.2).
    //
    //                        patch sequence
    //                 +------------------------+
    //                 |          inner patch   |
    //                 |       +----------------+
    //      ?      u   |       | s           u  |        ?
    //    ----](------)[-----------------)[-----](----------------   new control seq
    //                 ^                        ^
    //        patchLowerBound             patchUpperBound
    //
    //    Insert patch sequence into old control sequence within `patchLowerBound` and `patchUpperBound` and
    //    return the result
    //
    // 5.1 To merge first segment with `innerPatchSequence` build `leftPatchSequence`.
    //
    // 5.1.1 If first patch segment is unstable.
    //
    //    - If lower bound of `zsegment` is isolated (see p.2).
    //
    //              u
    //           (-----)                  first patch segment
    //           u     |       s
    //    X------------)[------------X    `leftPatchSequence`
    //
    //    - If lower bound of `zsegment` is non-isolated (see p.2).
    //
    //              u
    //           (-----)                  first patch segment
    //                 u
    //    X--------------------------X    `leftPatchSequence`
    //
    // 5.1.2 If first patch segment is stable.
    //
    //    - If lower bound of `zsegment` is isolated (see p.2).
    //
    //              s
    //           (-----)                  first patch segment
    //           s     |       u
    //    X------------)[------------X    `leftPatchSequence`
    //
    //    - If lower bound of `zsegment` is non-isolated (see p.2).
    //
    //              s
    //           (-----)                  first patch segment
    //                 s
    //    X--------------------------X    `leftPatchSequence`
    //
    // 5.1.3 Get merged sequence as `leftPatchSequence.append(innerPatchSequence)`.
    //
    // 5.2 To merge last segment with `innerPatchSequence` build `rightPatchSequence`.
    //
    // 5.2.1 If last patch segment is unstable.
    //
    //    - If upper bound of `zsegment` is isolated (see p.2).
    //
    //                    u
    //                 (-----)            last patch segment
    //           s     |       u
    //    X-----------](-------------X    `rightPatchSequence`
    //
    //   - If upper bound of `zsegment` is non-isolated (see p.2).
    //
    //                    u
    //                 (-----)            last patch segment
    //                 u
    //    X--------------------------X    `rightPatchSequence`
    //
    // 5.2.2 If last patch segment is stable.
    //
    //    - If upper bound of `zsegment` is isolated (see p.2).
    //
    //                    s
    //                 (-----)            last patch segment
    //           u     |       s
    //    X-----------](-------------X    `rightPatchSequence`
    //
    //    - If upper bound of `zsegment` is non-isolated (see p.2).
    //
    //                    s
    //                 (-----)            last patch segment
    //                 s
    //    X--------------------------X    `rightPatchSequence`
    //
    // 5.2.3 Get merged sequence as `rightPatchSequence.prepend(innerPatchSequence)`.

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
       * `true` if corresponding bound of `zsegment` satisfies stability condition (see p.4).
       */
      val zsegmentBoundIsStable: Boolean,
      /**
       * `false` if corresponding adjacent segment is strict unstable and has same base value as bound of `zsegment`.
       */
      val zsegmentBoundIsIsolated: Boolean
    )

    /**
     * [[PatchBoundInfo]] for case when bound is non-shifted (p.1):
     * <tr>`patchLowerBound` == `zsegment.lower` - for left side info;</tr>
     * <tr>`patchUpperBound` == `zsegment.upper` - for right side info.</tr>
     */
    case class NonShiftedPatchBoundInfo(
      override val patchBoundTruncation: SegmentTruncation[E, D, ControlValue[E, D, V]],
      override val zsegmentBoundIsStable: Boolean,
      override val zsegmentBoundIsIsolated: Boolean
    ) extends PatchBoundInfo(
      patchBoundTruncation,
      zsegmentBoundIsStable,
      zsegmentBoundIsIsolated
    )

    /**
     * [[PatchBoundInfo]] for case when bound is shifted (p.1):
     * <tr>`patchLowerBound` != `zsegment.lower` - for left side info;</tr>
     * <tr>`patchUpperBound` != `zsegment.upper` - for right side info.</tr>
     */
    case class ShiftedPatchBoundInfo(
      override val patchBoundTruncation: SegmentTruncation[E, D, ControlValue[E, D, V]],
      override val zsegmentBoundIsStable: Boolean,
      override val zsegmentBoundIsIsolated: Boolean,
      /**
       * <tr>`leftPatchSequence` - if class represents left side info;</tr>
       * <tr>`rightPatchSequence` - if class represents right side info;</tr>
       * (see p.5.1 and 5.2)
       */
      patchBoundSequence: TreapSegmentSeq[E, D, ControlValue[E, D, V]]
    ) extends PatchBoundInfo(
      patchBoundTruncation,
      zsegmentBoundIsStable,
      zsegmentBoundIsIsolated
    )

    object PatchBoundInfo {

      def getLeftSideInfo(zsegment: ZSegment[E, D, V], newBaseSeq: SegmentSeq[E, D, V]): PatchBoundInfo =
        makeInfo(new LeftSideFactory(zsegment, newBaseSeq))

      def getRightSideInfo(zsegment: ZSegment[E, D, V], newBaseSeq: SegmentSeq[E, D, V]): PatchBoundInfo =
        makeInfo(new RightSideFactory(zsegment, newBaseSeq))

      // Private section ---------------------------------------------------------- //
      private def makeInfo(factory: Factory): PatchBoundInfo = {
        var nextStep = true
        var firstStep = true
        var zsegmentBoundIsStable: Boolean | Null = null
        var zsegmentBoundIsIsolated = true
        var patchBoundIsShifted = false
        var patchBoundIsLazy = false
        var patchBoundSegment = factory.getZsegment
        val segmentIterator = factory.getZsegmentIterable.drop(1).iterator // skip `zsegment` itself
        while (segmentIterator.hasNext && nextStep) {
          val currZsegment = segmentIterator.next()
          val currBaseValue = currZsegment.self.value._1
          val currControlValue = currZsegment.self.value._2
          // Current segment is adjacent to `zsegment`.
          if (firstStep) {
            nextStep = currControlValue.isStrictUnstable
            zsegmentBoundIsIsolated =
              !(currControlValue.isStrictUnstable && valueOps.eqv(currBaseValue, factory.getZsegmentBoundBaseValue))
            zsegmentBoundIsStable =
              if (zsegmentBoundIsIsolated) currControlValue.isStrict
              else null // bound stability will be defined on the next step
          // Current segment is second after `zsegment`.
          } else {
            nextStep = false
            if (zsegmentBoundIsStable == null) zsegmentBoundIsStable = currControlValue.isStrict
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
        factory.build(
          zsegmentBoundIsStable.nn, zsegmentBoundIsIsolated, patchBoundIsShifted, patchBoundIsLazy, patchBoundSegment
        )
      }

      private trait Factory {

        /**
         * @return left or right bound info.
         */
        def build(
          stableAdjacent: Boolean,
          isolatedAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo

        /**
         * @return `zsegment` for which we build new control sequence.
         */
        def getZsegment: ZSegment[E, D, V]

        /**
         * @return backward or forward iterable from `zsegment` (including).
         *         Direction depends on factory type (left or right).
         */
        def getZsegmentIterable: Iterable[ZSegment[E, D, V]]

        /**
         * @return value of new base sequence at corresponding bound of `zsegment`.
         *         Note, `zsegment.value._1` contains old base value.
         */
        def getZsegmentBoundBaseValue: V
      }

      private class LeftSideFactory(
        private val zsegment: ZSegment[E, D, V],
        private val newBaseSeq: SegmentSeq[E, D, V]
      ) extends Factory {

        override def build(
          stableAdjacent: Boolean,
          isolatedAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo = {

          val patchBoundTruncation = patchBoundZsegment.self.secondSeqLowerTruncation
          if (patchBoundIsShifted) {
            val patchBoundSequence: ControlSegmentSeq[E, D, V] = patchBoundZsegment match {
              case s: ZippedSegmentWithNext[E, D, V, ControlValue[E, D, V], ZValue[E, D, V], _, _] =>
                // Adjacent segment of first patch segment is lazy =>
                // first patch segment is strict unstable =>
                // case 5.1.1
                if (patchBoundIsLazy)
                  if (isolatedAdjacent)
                    makeSingleBoundedControlSeq(StrictValue.unstable, StrictValue.stable, s.upper)
                  else
                    makeUniformControlSeq(StrictValue.unstable)
                // otherwise case 5.1.2
                else
                  if (isolatedAdjacent)
                    makeSingleBoundedControlSeq(StrictValue.stable, StrictValue.unstable, s.upper)
                  else
                    makeUniformControlSeq(StrictValue.stable)
              case _ =>
                // `patchBoundIsShifted` == `true` =>
                // there is at least original `zsegment` after `boundZsegment` =>
                // `boundZsegment` has next segment.
                throw new AssertionError(
                  s"Expected that segment $patchBoundZsegment has next segment."
                )
            }
            ShiftedPatchBoundInfo(patchBoundTruncation, stableAdjacent, isolatedAdjacent, patchBoundSequence)
          } else {
            NonShiftedPatchBoundInfo(patchBoundTruncation, stableAdjacent, isolatedAdjacent)
          }
        }

        override def getZsegment: ZSegment[E, D, V] = zsegment

        override def getZsegmentIterable: Iterable[ZSegment[E, D, V]] = zsegment.backwardIterable

        override def getZsegmentBoundBaseValue: V = newBaseSeq.getValueForExtended(zsegment.lower)
      }

      private class RightSideFactory(
        private val zsegment: ZSegment[E, D, V],
        private val newBaseSeq: SegmentSeq[E, D, V]
      ) extends Factory {

        override def build(
          stableAdjacent: Boolean,
          isolatedAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo = {

          val patchBoundTruncation = patchBoundZsegment.self.secondSeqUpperTruncation
          if (patchBoundIsShifted) {
            val patchBoundSequence: ControlSegmentSeq[E, D, V] = patchBoundZsegment match {
              case s: ZippedSegmentWithPrev[E, D, V, ControlValue[E, D, V], ZValue[E, D, V], _, _] =>
                // Adjacent segment of last patch segment is lazy =>
                // last patch segment is strict unstable =>
                // case 5.2.1
                if (patchBoundIsLazy)
                  if (isolatedAdjacent)
                    makeSingleBoundedControlSeq(StrictValue.stable, StrictValue.unstable, s.lower.flipLower)
                  else
                    makeUniformControlSeq(StrictValue.unstable)
                // otherwise case 5.2.2
                else
                  if (isolatedAdjacent)
                    makeSingleBoundedControlSeq(StrictValue.unstable, StrictValue.stable, s.lower.flipLower)
                  else
                    makeUniformControlSeq(StrictValue.stable)
              case _ =>
                // `patchBoundIsShifted` == `true` =>
                // there is at least original `zsegment` before `patchBoundZsegment` =>
                // `patchBoundZsegment` has previous segment.
                throw new AssertionError(
                  s"Expected that segment $patchBoundZsegment has previous segment."
                )
            }
            ShiftedPatchBoundInfo(patchBoundTruncation, stableAdjacent, isolatedAdjacent, patchBoundSequence)
          } else {
            NonShiftedPatchBoundInfo(patchBoundTruncation, stableAdjacent, isolatedAdjacent)
          }
        }

        override def getZsegment: ZSegment[E, D, V] = zsegment

        override def getZsegmentIterable: Iterable[ZSegment[E, D, V]] = zsegment.forwardIterable

        override def getZsegmentBoundBaseValue: V = newBaseSeq.getValueForExtended(zsegment.upper)
      }
    }

    val leftSideInfo = PatchBoundInfo.getLeftSideInfo(zsegment, baseSeq)
    val rightSideInfo = PatchBoundInfo.getRightSideInfo(zsegment, baseSeq)
    val patchSequence: SegmentSeq[E, D, ControlValue[E, D, V]] = (leftSideInfo, rightSideInfo) match {
      // See p.4 and 5.
      case (li: ShiftedPatchBoundInfo, ri: ShiftedPatchBoundInfo) =>
        val innerPatchSequence = makeControlSeq(zsegment, baseSeq, li.zsegmentBoundIsStable, ri.zsegmentBoundIsStable)
        ri.patchBoundSequence.prepend(li.patchBoundSequence.append(innerPatchSequence))
      // See p.5 for all cases below.
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
   * of new control sequence must be unstable.
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
      makeUniformControlSeq(StrictValue.stable)

    } else {
      val boundSegments = SegmentSeqUtil.getBoundSegments(zsegment, baseSeq)

      //          (---]            - zsegment
      //  X--)[----------)[----X   - baseSeq
      //  X--------------------X   - output
      //            \
      //        stable if both `strictLowerBound` and `strictUpperBound` are `true`
      if (domainOps.segments.upperOrd.eqv(boundSegments._1, boundSegments._2)) {
        makeUniformControlSeq(StrictValue.cons(isStable = stableLowerBound && stableUpperBound))

      } else boundSegments match {
        case (lowerSegment: Segment.WithNext[E, D, V], upperSegment: Segment.WithPrev[E, D, V]) =>
          val nextSegment = lowerSegment.moveNext

          //         (----------------]           - zsegment
          //  X---)[---------)[-----------](--X   - baseSeq
          if (domainOps.segments.upperOrd.eqv(nextSegment, upperSegment)) {
            // If `strictLowerBound` == `strictUpperBound` then both `strictLowerBound` and `strictUpperBound` are `false`,
            // because the case when they are both `true` was considered before.
            //
            //      ?(----------------]?          - zsegment
            // X------------------------------X   - output
            //            unstable
            if (stableLowerBound == stableUpperBound) {
              makeUniformControlSeq(StrictValue.unstable)

              // `strictLowerBound` and `strictUpperBound` are different.
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
                StrictValue.cons(stableLowerBound),
                StrictValue.cons(stableUpperBound),
                lowerSegment.upper
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
                  lowerSegment.upper,
                  rng.nextInt(),
                  StrictValue.unstable
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
                  upperSegment.lower.flipLower,
                  rng.nextInt(),
                  StrictValue.stable
                )(
                  boundOrd
                )
            }
            val root = BuildAsc.finalizeBuffer(buffer)
            root match {
              case root: ImmutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]] =>
                makeNonuniformControlSeq(root, StrictValue.cons(stableUpperBound))
              // The case when both `strictLowerBound` and `strictUpperBound` are `true` was considered before =>
              // either `strictLowerBound` or `strictUpperBound` is `false` =>
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
   * Case is impossible due to stable segment must be always separated from a lazy one with unstable strict segment.
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
   * S1 will be stable iff its left adjacent segment is strict.
   * But in that case we will get:
   * {{{
   *
   * 2.b                  S3
   *      --](--------------------------)[--  control sequence
   *       s              u               ?
   *      --](--------------------------)[--  base sequence
   *                      A
   * }}}
   * Segments S1 and S2 will be merged into strict unstable S3. So we can't get case 2 from 2.a and
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
    cacheStable(getZippedSeq.firstSegment.lowerTruncation) match {
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
    cacheStable(getZippedSeq.lastSegment.upperTruncation) match {
      case s: ZSegmentTerminal[E, D, V] @unchecked => LazyTerminalSegment(this, s)
      case s: ZSegmentSingle[E, D, V] @unchecked => LazySingleSegment(this, s)
      case s => throwSegmentMustBeTerminalOrSingle(s) // just to remove warning
    }
}

object AbstractLazyTreapSegmentSeq { outer =>

  type LazySegment[E, D[X] <: Domain[X], V] =
    SegmentT[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]

  type LazyFirstSegment[E, D[X] <: Domain[X], V] =
    SegmentT.First[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]

  type LazyLastSegment[E, D[X] <: Domain[X], V] =
    SegmentT.Last[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]

  type LazyTruncation[E, D[X] <: Domain[X], V] =
    SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], LazySegment[E, D, V]]

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

    import ordset.core.segmentSeq.internal.lazySeq

    type ZSegment[E, D[X] <: Domain[X], V] = lazySeq.ZSegment[E, D, V] & Tag[Stable.type]

    type ZSegmentWithNext[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentWithNext[E, D, V] & Tag[Stable.type]

    type ZSegmentWithPrev[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentWithPrev[E, D, V] & Tag[Stable.type]

    type ZSegmentInitial[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentInitial[E, D, V] & Tag[Stable.type]

    type ZSegmentTerminal[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentTerminal[E, D, V] & Tag[Stable.type]

    type ZSegmentInner[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentInner[E, D, V] & Tag[Stable.type]

    type ZSegmentSingle[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentSingle[E, D, V] & Tag[Stable.type]

    /**
     * If input `segment` is stable marks it with [[Stable]] tag, otherwise throws exception.
     */
    @throws[AssertionError]("if input segment is not stable")
    def assert[E, D[X] <: Domain[X], V](segment: lazySeq.ZSegment[E, D, V]): stable.ZSegment[E, D, V] =
      if (segment.value._2.isStable) tagged(segment) else throwSegmentIsNotStable(segment)

    /**
     * Marks input segment with [[Stable]] tag. Stability check must be provided by caller.
     */
    def unsafe[E, D[X] <: Domain[X], V](segment: lazySeq.ZSegment[E, D, V]): stable.ZSegment[E, D, V] =
      tagged(segment)

    def throwSegmentIsNotStable[E, D[X] <: Domain[X], V](zsegment: lazySeq.ZSegment[E, D, V]): Nothing =
      throw AssertionError(s"Expected that segment $zsegment is stable.")

    // Private section ---------------------------------------------------------- //
    private def tagged[R](r: R): R with Tag[Stable.type] = r.asInstanceOf[R & Tag[Stable.type]]
  }

  /**
   * Types indicating that segment is strict, i.e. that its value will not change after any subsequent computations
   * of lazy values in sequence.
   *
   * <h3>Note</h3>
   *
   * Don't use tagged type in pattern matching:
   * {{{
   *  segment match {
   *    case s: Strict.ZSegmentInner[E, D, V] => ...
   *    case s: Strict.ZSegmentTerminal[E, D, V] => ...
   *    ...
   *  }
   * }}}
   * It fill always fail because segment doesn't have [[Tag]] trait in runtime.
   * We create only typelevel wrapper to mark strict segments (see [[Strict.tagged]]).
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
  object Strict { strict =>

    import ordset.core.segmentSeq.internal.lazySeq

    type ZSegment[E, D[X] <: Domain[X], V] = lazySeq.ZSegment[E, D, V] & Tag[Strict.type]

    type ZSegmentWithNext[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentWithNext[E, D, V] & Tag[Strict.type]

    type ZSegmentWithPrev[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentWithPrev[E, D, V] & Tag[Strict.type]

    type ZSegmentInitial[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentInitial[E, D, V] & Tag[Strict.type]

    type ZSegmentTerminal[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentTerminal[E, D, V] & Tag[Strict.type]

    type ZSegmentInner[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentInner[E, D, V] & Tag[Strict.type]

    type ZSegmentSingle[E, D[X] <: Domain[X], V] = lazySeq.ZSegmentSingle[E, D, V] & Tag[Strict.type]

    /**
     * If input `segment` is strict marks it with [[Strict]] tag, otherwise throws exception.
     */
    @throws[AssertionError]("if input segment is not strict")
    def assert[E, D[X] <: Domain[X], V](segment: lazySeq.ZSegment[E, D, V]): strict.ZSegment[E, D, V] =
      if (segment.value._2.isStrict) tagged(segment) else throwSegmentIsNotStrict(segment)

    /**
     * Marks input segment with [[Strict]] tag. Strictness check must be provided by caller.
     */
    def unsafe[E, D[X] <: Domain[X], V](segment: lazySeq.ZSegment[E, D, V]): strict.ZSegment[E, D, V] =
      tagged(segment)

    def throwSegmentIsNotStrict[E, D[X] <: Domain[X], V](zsegment: lazySeq.ZSegment[E, D, V]): Nothing =
      throw AssertionError(s"Expected that segment $zsegment is strict.")

    // Private section ---------------------------------------------------------- //
    private def tagged[R](r: R): R with Tag[Strict.type] = r.asInstanceOf[R & Tag[Strict.type]]
  }

  /**
   * Base trait for output segments of [[AbstractLazyTreapSegmentSeq]].
   *
   * Wraps [[ZSegment]] - segment with internal representation of lazy sequence and
   * provides api to interact with lazy sequence.
   */
  sealed trait LazySegmentBase[E, D[X] <: Domain[X], V] 
    extends MappedSegmentLikeT[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]] {

    // Inspection --------------------------------------------------------------- //

    /**
     * Lazy sequence that the current segment belongs to.
     */
    override def sequence: LazySegmentSeq[E, D, V]

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: LazyFirstSegment[E, D, V] = sequence.firstSegment

    override def moveToLast: LazyLastSegment[E, D, V] = sequence.lastSegment

    override def moveToBound(bound: Bound[E]): LazySegment[E, D, V] = sequence.getSegmentForBound(bound)

    override def moveToExtended(bound: ExtendedBound[E]): LazySegment[E, D, V] = sequence.getSegmentForExtended(bound)

    override def moveToElement(element: E): LazySegment[E, D, V] = sequence.getSegmentForElement(element)

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: LazySegmentSeq[E, D, V]

    override def takeBelow: LazySegmentSeq[E, D, V]

    override def slice: (LazySegmentSeq[E, D, V], LazySegmentSeq[E, D, V])

    override def prepend(other: SegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] = {
      // Default implementation for first segment. Must be overridden if segment has previous segment.
      sequence
    }

    override def append(other: SegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] = {
      // Default implementation for last segment. Must be overridden if segment has next segment.
      sequence
    }

    // Protected section -------------------------------------------------------- //
    final override protected val segmentMapFunc: Segment[E, D, ZValue[E, D, V]] => V = s => s.value._1

    /**
     * `this.sequence` - is the lazy sequence, that the current segment belongs to.
     * 
     * `this.sequence.zippedSeqRef` - is a reference to zipped sequence, that is a cache with computed values of 
     *                                lazy sequence.
     * 
     * `this.zsegmentRef` - is the reference to segment of cache sequence (zipped sequence inside 
     *                      `this.sequence.zippedSeqRef`).
     * 
     * Current segment wraps `zsegment` and provides client api for lazy sequence. `this.original` method is intended
     * to give convenient access to `zsegment`. 
     * 
     * There are several points to note:
     * 
     * 1. `this.sequence.zippedSeqRef` - is a reference to store updated versions of cache sequence (zipped sequence
     * itself is immutable). It's possible that at some point in time `this.sequence.zippedSeqRef` will be updated,
     * but `this.zsegmentRef` will remain the same. It will reference to `zsegment`, that is a part of old cache
     * sequence.
     * 
     * Outdated `zsegment` will not impact the correctness: it can't leak outside of `this` instance and is passed
     * only into methods of `this.sequence`. These methods can handle such cases properly: they compare sequence
     * of `zsegment` with the actual one and obtain new `zsegment` if needed.
     * 
     * On the other hand outdated `this.zsegmentRef` prevents from garbage collecting the whole outdated version
     * of cache sequence. So we should get rid of it as soon as possible to optimize memory usage. Actualization
     * of `this.zsegmentRef` is performed in method `this.getZsegment`, and implementations of `this.original` in
     * subclasses should always use it instead of direct access to `this.zsegmentRef`.
     *  
     * 2. Types of current segment and `zsegment` inside `this.zsegmentRef` are related: if `zsegment` is initial,
     * then `this` is also is initial, etc. Subtypes of `LazySegmentBase` can redefine type of `this.original
     * in the following way:
     * <tr>
     *  - subtype of `LazySegmentBase` declares constructor parameter for `zsegment` of some type
     *    `T <: Stable.ZSegment[E, D, V];
     * </tr>  
     * <tr>
     *  - `this.zsegmentRef` is initialized with specified constructor parameter (we can't redefine type of
     *    `this.zsegmentRef` due to invariance of atomic reference on its type parameter, so type of `zsegment`
     *    is widened in this moment);
     * </tr>
     * <tr>
     *  - subtype of `LazySegmentBase` overrides `this.original` with type `T` and can safely cast output of
     *    `this.getZsegment` into type `T` to provide its implementation.
     * </tr>
     */
    override protected def original: Stable.ZSegment[E, D, V]

    /**
     * Reference to segment of cache sequence (`this.sequence.zippedSeqRef`).
     * 
     * Warning! Don't use it directly in subclasses, use `this.getZsegment` instead. Otherwise you can get out of date
     * version of cache (see comments to `this.original`).
     */
    protected val zsegmentRef: AtomicReference[Stable.ZSegment[E, D, V]]

    /**
     * Returns actualized segment of cache sequence (`this.sequence.zippedSeqRef`).
     * 
     * If segment in `this.zsegmentRef` doesn't belong to cache sequence inside `this.sequence.zippedSeqRef`, then
     * new version of cache is available. In that case method receive new segment from  `this.sequence.zippedSeqRef`,
     * updates `this.zsegmentRef` and returns new segment. Otherwise, it just returns segment from `this.zsegmentRef`.
     * 
     * During check or update of `this.zsegmentRef` cache sequence in `this.sequence.zippedSeqRef` can be updated
     * itself. So there is a chance, that we end up with still outdated version of `zsegment`. However, this scenario
     * doesn't impact the correctness (see p.1 of comments to `this.original`). Outdated version of `zsegment` is
     * treated as a normal behavior, but should be updated as soon as possible.
     */
    final protected def getZsegment: Stable.ZSegment[E, D, V] = {
      import scala.language.unsafeNulls
      var zs: Stable.ZSegment[E, D, V] = null
      var success = false
      while (!success) {
        val seq = sequence.getZippedSeq
        zs = zsegmentRef.get.nn
        success = zs.sequence.eq(seq)
        if (!success) {
          val oldZs = zs
          zs = Stable.assert(seq.getSegmentForExtended(oldZs.upper))
          success = zsegmentRef.compareAndSet(oldZs, zs)
        }
      }
      zs
    }
  }

  object LazySegmentBase {

    trait TruncationBase[E, D[X] <: Domain[X], V] {
      self: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], LazySegment[E, D, V]] =>

      override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.prependBelowExtendedInternal(bound, getSegmentForPrepending.original, other)

      override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.appendAboveExtendedInternal(bound, getSegmentForAppending.original, other)
    }
  }

  /**
   * Lazy segment with next segment.
   */
  sealed trait LazySegmentWithNext[E, D[X] <: Domain[X], V] 
    extends MappedSegmentT.WithNext[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]] 
    with LazySegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazySegmentWithNext[E, D, V]

    override def upper: Bound.Upper[E] = original.upper

    // Navigation --------------------------------------------------------------- //
    override def moveNext: LazySegmentWithPrev[E, D, V] = {
      val nextSegment = original.moveNext
      sequence.makeSegmentWithPrev(nextSegment.lowerTruncation)
    }

    // Transformation ----------------------------------------------------------- //
    override def append(other: SegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] =
      sequence.appendAboveBoundInternal(upper, original, other)

    // Protected section -------------------------------------------------------- //
    protected override def original: Stable.ZSegmentWithNext[E, D, V]
  }

  /**
   * Lazy segment with previous segment.
   */
  sealed trait LazySegmentWithPrev[E, D[X] <: Domain[X], V] 
    extends MappedSegmentT.WithPrev[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazySegmentWithPrev[E, D, V]

    override def lower: Bound.Lower[E] = original.lower

    // Navigation --------------------------------------------------------------- //
    override def movePrev: LazySegmentWithNext[E, D, V] = {
      val prevSegment = original.movePrev
      sequence.makeSegmentWithNext(prevSegment.upperTruncation)
    }

    // Transformation ----------------------------------------------------------- //
    override def prepend(other: SegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] =
      sequence.prependBelowBoundInternal(lower, original, other)

    // Protected section -------------------------------------------------------- //
    protected override def original: Stable.ZSegmentWithPrev[E, D, V]
  }

  /**
   * Lazy initial segment.
   */
  final case class LazyInitialSegment[E, D[X] <: Domain[X], V] private (
    override val sequence: LazySegmentSeq[E, D, V],
    // Public constructor has more strict type to provide safe cast in implementation of `this.original`
    // (see p.2 of comments to `LazySegmentBase.original`).
    override protected val zsegmentRef: AtomicReference[Stable.ZSegment[E, D, V]]
  ) extends LazySegmentBase[E, D, V]
    with MappedSegmentT.Initial[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazyInitialSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: LazyInitialSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: LazySegmentSeq[E, D, V] = sequence

    override def takeBelow: LazySegmentSeq[E, D, V] = sequence.consUniform(value)

    override def slice: (LazySegmentSeq[E, D, V], LazySegmentSeq[E, D, V]) = (takeBelow, takeAbove)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      new LazyInitialSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      SegmentTruncationT.upperTruncation(this)

    // Protected section -------------------------------------------------------- //

    // Cast is safe (see p.2 of comments to `LazySegmentBase.original`).
    protected override def original: Stable.ZSegmentInitial[E, D, V] = getZsegment.asInstanceOf
  }

  object LazyInitialSegment {

    def apply[E, D[X] <: Domain[X], V](
      sequence: LazySegmentSeq[E, D, V],
      zsegment: Stable.ZSegmentInitial[E, D, V]
    ): LazyInitialSegment[E, D, V] = 
      new LazyInitialSegment(sequence, new AtomicReference(zsegment))

    final class Truncation[E, D[X] <: Domain[X], V, +Seg <: LazyInitialSegment[E, D, V]](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentT.Initial.Truncation[E, D, V, LazySegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with LazySegmentBase.TruncationBase[E, D, V]
  }

  /**
   * Lazy terminal segment.
   */
  final case class LazyTerminalSegment[E, D[X] <: Domain[X], V] private (
    override val sequence: LazySegmentSeq[E, D, V],
    // Public constructor has more strict type to provide safe cast in implementation of `this.original`
    // (see p.2 of comments to `LazySegmentBase.original`).
    override protected val zsegmentRef: AtomicReference[Stable.ZSegment[E, D, V]]
  ) extends LazySegmentBase[E, D, V]
    with MappedSegmentT.Terminal[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentWithPrev[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazyTerminalSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: LazyTerminalSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: LazySegmentSeq[E, D, V] = sequence.consUniform(value)

    override def takeBelow: LazySegmentSeq[E, D, V] = sequence

    override def slice: (LazySegmentSeq[E, D, V], LazySegmentSeq[E, D, V]) = (takeBelow, takeAbove)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      new LazyTerminalSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      SegmentTruncationT.upperTruncation(this)

    // Protected section -------------------------------------------------------- //

    // Cast is safe (see p.2 of comments to `LazySegmentBase.original`).
    protected override def original: Stable.ZSegmentTerminal[E, D, V] = getZsegment.asInstanceOf
  }

  object LazyTerminalSegment {

    def apply[E, D[X] <: Domain[X], V](
      sequence: LazySegmentSeq[E, D, V],
      zsegment: Stable.ZSegmentTerminal[E, D, V]
    ): LazyTerminalSegment[E, D, V] = 
      new LazyTerminalSegment(sequence, new AtomicReference(zsegment))

    final class Truncation[E, D[X] <: Domain[X], V, +Seg <: LazyTerminalSegment[E, D, V]](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentT.Terminal.Truncation[E, D, V, LazySegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with LazySegmentBase.TruncationBase[E, D, V]
  }

  /**
   * Lazy inner segment.
   */
  final case class LazyInnerSegment[E, D[X] <: Domain[X], V] private (
    override val sequence: LazySegmentSeq[E, D, V],
    // Public constructor has more strict type to provide safe cast in implementation of `this.original`
    // (see p.2 of comments to `LazySegmentBase.original`).
    override protected val zsegmentRef: AtomicReference[Stable.ZSegment[E, D, V]]
  ) extends LazySegmentBase[E, D, V]
    with MappedSegmentT.Inner[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentWithPrev[E, D, V]
    with LazySegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazyInnerSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: LazySegmentSeq[E, D, V] =
      // `original` is always stable => no correction of control values is required
      // (see `AbstractLazyTreapSegmentSeq.takeAboveBoundInternal`)
      SliceOps.takeAboveZSegmentWithoutCorrection(original, sequence)

    override def takeBelow: LazySegmentSeq[E, D, V] =
      // `original` is always stable => no correction of control values is required
      // (see `AbstractLazyTreapSegmentSeq.takeBelowBoundInternal`)
      SliceOps.takeBelowZSegmentWithoutCorrection(original, sequence)

    override def slice: (LazySegmentSeq[E, D, V], LazySegmentSeq[E, D, V]) =
      SliceOps.sliceZSegmentWithoutCorrection(original, sequence)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      new LazyInnerSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      SegmentTruncationT.upperTruncation(this)

    // Protected section -------------------------------------------------------- //

    // Cast is safe (see p.2 of comments to `LazySegmentBase.original`).
    protected override def original: Stable.ZSegmentInner[E, D, V] = getZsegment.asInstanceOf
  }

  object LazyInnerSegment {

    def apply[E, D[X] <: Domain[X], V](
      sequence: LazySegmentSeq[E, D, V],
      zsegment: Stable.ZSegmentInner[E, D, V]
    ): LazyInnerSegment[E, D, V] = 
      new LazyInnerSegment(sequence, new AtomicReference(zsegment))

    final class Truncation[E, D[X] <: Domain[X], V, +Seg <: LazyInnerSegment[E, D, V]](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentT.Inner.Truncation[E, D, V, LazySegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with LazySegmentBase.TruncationBase[E, D, V]
  }

  /**
   * Lazy single segment.
   */
  final case class LazySingleSegment[E, D[X] <: Domain[X], V] private (
    override val sequence: LazySegmentSeq[E, D, V],
    // Public constructor has more strict type to provide safe cast in implementation of `this.original`
    // (see p.2 of comments to `LazySegmentBase.original`).
    override protected val zsegmentRef: AtomicReference[Stable.ZSegment[E, D, V]]
  ) extends LazySegmentBase[E, D, V]
    with MappedSegmentT.Single[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazySingleSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: LazySingleSegment[E, D, V] = this

    override def moveToLast: LazySingleSegment[E, D, V] = this

    override def moveToBound(bound: Bound[E]): LazySingleSegment[E, D, V] = this

    override def moveToExtended(bound: ExtendedBound[E]): LazySingleSegment[E, D, V] = this

    override def moveToElement(element: E): LazySingleSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: LazySegmentSeq[E, D, V] = sequence

    override def takeBelow: LazySegmentSeq[E, D, V] = sequence

    override def slice: (LazySegmentSeq[E, D, V], LazySegmentSeq[E, D, V]) = (sequence, sequence)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      new LazySingleSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, LazySegmentBase[E, D, V], this.type] =
      SegmentTruncationT.upperTruncation(this)

    // Protected section -------------------------------------------------------- //

    // Cast is safe (see p.2 of comments to `LazySegmentBase.original`).
    protected override def original: Stable.ZSegmentSingle[E, D, V] = getZsegment.asInstanceOf
  }

  object LazySingleSegment {

    def apply[E, D[X] <: Domain[X], V](
      sequence: LazySegmentSeq[E, D, V],
      zsegment: Stable.ZSegmentSingle[E, D, V]
    ): LazySingleSegment[E, D, V] = 
      new LazySingleSegment(sequence, new AtomicReference(zsegment))

    final class Truncation[E, D[X] <: Domain[X], V, +Seg <: LazySingleSegment[E, D, V]](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentT.Single.Truncation[E, D, V, LazySegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with LazySegmentBase.TruncationBase[E, D, V]
  }

  // Protected section -------------------------------------------------------- //
  protected object SliceOps {

    /**
     * Same as `sequence.takeAboveExtended(zsegment.upper)` but doesn't apply any correction of
     * control values (see [[AbstractLazyTreapSegmentSeq.takeAboveBoundInternal]]).
     *
     * I.e. it just applies [[SegmentSeqT.takeAboveExtended]] to base and control sequences and builds new lazy
     * sequence from them.
     *
     * Preconditions:
     *
     * 1. `zsegment.sequence.eq(sequence.zippedSeq)`
     *
     * 2. `zsegment` and its adjacent segments are such that correction of control values isn't required.
     *    Otherwise method may produce lazy sequence in invalid state.
     */
    def takeAboveZSegmentWithoutCorrection[E, D[X] <: Domain[X], V](
      zsegment: ZSegment[E, D, V],
      sequence: LazySegmentSeq[E, D, V]
    ): LazySegmentSeq[E, D, V] = {
      val newBaseSeq = TreapSegmentSeqUtil.takeAboveSegment(zsegment.self.front.firstSeqSegment.self)
      val newControlSeq = TreapSegmentSeqUtil.takeAboveSegment(zsegment.self.front.secondSeqSegment.self)
      sequence.consLazy(sequence.makeZippedSeq(newBaseSeq, newControlSeq))
    }

    /**
     * Same as `sequence.takeBelowExtended(zsegment.lower)` but doesn't apply any correction of
     * control values (see [[AbstractLazyTreapSegmentSeq.takeBelowBoundInternal]]).
     *
     * I.e. it just applies [[SegmentSeqT.takeBelowExtended]] to base and control sequences and builds new lazy
     * sequence from them.
     *
     * Preconditions:
     *
     * 1. `zsegment.sequence.eq(sequence.zippedSeq)`
     *
     * 2. `zsegment` and its adjacent segments are such that correction of control values isn't required.
     *    Otherwise method may produce lazy sequence in invalid state.
     */
    def takeBelowZSegmentWithoutCorrection[E, D[X] <: Domain[X], V](
      zsegment: ZSegment[E, D, V],
      sequence: LazySegmentSeq[E, D, V]
    ): LazySegmentSeq[E, D, V] = {
      val newBaseSeq = TreapSegmentSeqUtil.takeBelowSegment(zsegment.self.back.firstSeqSegment.self)
      val newControlSeq = TreapSegmentSeqUtil.takeBelowSegment(zsegment.self.back.secondSeqSegment.self)
      sequence.consLazy(sequence.makeZippedSeq(newBaseSeq, newControlSeq))
    }

    def sliceZSegmentWithoutCorrection[E, D[X] <: Domain[X], V](
      zsegment: ZSegment[E, D, V],
      sequence: LazySegmentSeq[E, D, V]
    ): (LazySegmentSeq[E, D, V], LazySegmentSeq[E, D, V]) = {
      val back = zsegment.self.back
      val front = zsegment.self.front
      val baseSlice = sliceOriginalSeq(back.firstSeqSegment.self, front.firstSeqSegment.self)
      val controlSlice = sliceOriginalSeq(back.secondSeqSegment.self, front.secondSeqSegment.self)
      val leftSeq = sequence.consLazy(sequence.makeZippedSeq(baseSlice._1, controlSlice._1))
      val rightSeq = sequence.consLazy(sequence.makeZippedSeq(baseSlice._2, controlSlice._2))
      (leftSeq, rightSeq)
    }

    // Private section ---------------------------------------------------------- //
    private def sliceOriginalSeq[E, D[X] <: Domain[X], U](
      back: SegmentT[E, D, U, TreapSegmentBase[E, D, U]] with TreapSegmentBase[E, D, U],
      front: SegmentT[E, D, U, TreapSegmentBase[E, D, U]] with TreapSegmentBase[E, D, U]
    ): (TreapSegmentSeq[E, D, U], TreapSegmentSeq[E, D, U]) =
      // If `front` and `back` is the same segment we can do an optimization:
      // treap based original sequence can build both left and right slice parts simultaneously,
      // so we should use `segment.slice` whenever possible.
      if (front.domainOps.segments.upperOrd.eqv(back, front)) TreapSegmentSeqUtil.sliceSegment(front)
      else (TreapSegmentSeqUtil.takeBelowSegment(back), TreapSegmentSeqUtil.takeAboveSegment(front))
  }
}