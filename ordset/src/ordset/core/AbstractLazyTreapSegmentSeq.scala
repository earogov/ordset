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
/**
 * {{{
 *
 *   base sequence:
 *
 *              A               B               C            - base values
 *   X-------------------](----------](-------------------X
 *
 *   control sequence:
 *
 *        ?         u           s          u         ?       - control values
 *   X--------](---------](----------](--------](---------X
 * }}}
 * where
 *
 * ? - lazy segment: value hasn't been evaluated yet, evaluation will produce new segment sequence instead
 *     of given segment.
 *
 * u - unstable eager segment: value has been evaluated, but segment type is undefined (initial, inner, terminal or
 *     single) because of some adjacent segments are lazy.
 *
 * s - stable eager segment: value had been evaluated and segment type is defined, all adjacent segments are eager
 *     (stable or unstable).
 *
 */
abstract class AbstractLazyTreapSegmentSeq[E, D <: Domain[E], V]
  extends AbstractSegmentSeq[E, D, V, LazySegmentBase[E, D, V]] {

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = ???

  final override def isUniversal: Boolean = ???

  final override def isUniform: Boolean = ???

  final override def contains(bound: Bound[E]): Boolean = getSegment(bound).isIncluded

  final override def containsElement(element: E): Boolean = super.containsElement(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds

  final override lazy val firstSegment: LazyFirstSegment[E, D, V] = ???

  final override lazy val lastSegment: LazyLastSegment[E, D, V] = ???

  final override def getSegment(bound: Bound[E]): LazySegment[E, D, V] =
    makeSegment(zippedSeq.getSegment(bound))

  final override def getSegmentForElement(element: E): LazySegment[E, D, V] = super.getSegmentForElement(element)

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): TreapSegmentSeq[E, D, V] = sliced(bound)._2

  final override def takenBelow(bound: Bound[E]): TreapSegmentSeq[E, D, V] = sliced(bound)._1

  final override def sliced(bound: Bound[E]): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) = ???

  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = ???

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

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isValueIncluded(value: V): Boolean

  protected final def eval(zsegment: ZSegment[E, D, V]): ZSegment[E, D, V] =
    zsegment.value._2 match {
      case lazyValue: LazyValue[E, D, V] =>
        val seq = lazyValue.eval
        lock.synchronized {
          val newBaseSeq: TreapSegmentSeq[E, D, V] = patchBaseSeq(zsegment, seq)
          val newControlSeq: TreapSegmentSeq[E, D, ControlValue[E, D, V]] = patchControlSeq(zsegment, seq)
          val newZippedSeq: ZSegmentSeq[E, D, V] = ZippedOrderedMap.apply(
            newBaseSeq, newControlSeq, Tuple2.apply, _ => false, _ => false
          )(
            domainOps, zippedSeq.valueOps, rngManager
          )
          zippedSeq = newZippedSeq
        }
        SegmentSeqOps.getUpperBoundSegment(zsegment, zippedSeq)
      case _ =>
        zsegment
    }

  /**
   * Creates uniform control sequence with specified `value`.
   */
  protected final def makeUniformControlSeq(
    value: ControlValue[E, D, V]
  ): UniformOrderedMap[E, D, ControlValue[E, D, V]] =
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
   * @param baseSeq sequence that was evaluated for `zsegment` to patch corresponding base segment.
   */
  protected final def patchBaseSeq(
    zsegment: ZSegment[E, D, V],
    baseSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    // Performance note
    // Both original sequences of `zsegment` are treap based. The result of patch operation in most cases will
    // be also a treap based sequence. `convertMap` doesn't perform real conversion for them. Instead it just
    // returns unmodified input sequence.
    // The only exception - when uniform sequence is patched by some non-treap `baseSeq`. In such a case
    // patch operation may produce sequence of any type and `convertMap` will create new treap based instance.
    TreapOrderedMap.getFactory.convertMap(
      zsegment.self.patchedFirstSeq(baseSeq)
    )

  /**
   * Builds new control sequence for a given `zsegment` and applies patch operation to existing control sequence.
   * Note modifications of control sequence may lay outside of `zsegment`. In example below left adjacent segment
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
  ): TreapSegmentSeq[E, D, ControlValue[E, D, V]] = {

    // Let's call as patch sequence new control sequence that will replace existing one in some region around given
    // `zsegment`. Method builds patch sequence, inserts it into existing control sequence and returns new one.
    //
    // 1. Starting from `zsegment` (non inclusive) move backward until one of conditions is met:
    //    - either lazy or stable segment is found;
    //    - first segment of sequence is found.
    //
    // Let's denote lower bound of last visited unstable eager segment as `patchLowerBound`.
    // Similarly define `patchUpperBound`.
    //
    //   ?      u       u                         u       s
    // ----](------)[------](----------------](------)[--------   zipped seq
    //      ^                    zsegment            ^
    //   patchLowerBound                       patchUpperBound
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
    //      - get merged sequence as `leftPatchSequence.appended(rightPatchSequence)`;
    //    - apply received patch sequence to old control sequence within `patchLowerBound` and `patchUpperBound`
    //      and return the result.
    //
    //    Otherwise go to step 3.
    //
    //                    patch sequence
    //      +----------------------------------------+
    //   ?  |   u                    s               |    s
    // ----](------)[--------------------------------)[--------   new control seq
    //      ^                                        ^
    //   patchLowerBound                       patchUpperBound
    //
    // 3. If one or both conditions are `true`:
    //    - `patchLowerBound` == `zsegment.lowerBound`
    //    - `patchUpperBound` == `zsegment.upperBound`
    //    then get adjacent segments of `zsegment`.
    //
    //                 lower adjacent            upper adjacent
    //                    segment                   segment
    //   ?      u      u  /                        /    ?
    // ----](------)[------](----------------](----------------   zipped seq
    //      ^                    zsegment    ^
    //   patchLowerBound                patchUpperBound
    //
    //    Call method `makeControlSeq` with parameters:
    //    - `eagerLowerBound` = (lower adjacent segment is eager)*
    //    - `eagerUpperBound` = (upper adjacent segment is eager)*
    //    * parameter is also `true` if there is no corresponding adjacent segment.
    //
    //    In example above we get:
    //    - `eagerLowerBound` == `true`
    //    - `eagerUpperBound` == `false`
    //
    //    Let's denote received sequence as `innerPatchSequence`.
    //
    //    If both conditions are `true`:
    //    - `patchLowerBound` == `zsegment.lowerBound`
    //    - `patchUpperBound` == `zsegment.upperBound`
    //    then apply `innerPatchSequence` to old control sequence within `patchLowerBound` and `patchUpperBound`
    //    and return the result.
    //
    //    Otherwise go to step 4.
    //
    // 4. Merge `innerPatchSequence` with first or last segment of patch sequence obtained at step 2 (see 4.1 and 4.2).
    //
    //               patch sequence
    //      +--------------------------------+
    //      |                  inner patch   |
    //      |               +----------------+
    //   ?  |   u           | s           u  |        ?
    // ----](------)[-----------------)[-----](----------------   new control seq
    //      ^                                ^
    //   patchLowerBound                patchUpperBound
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
    // Then get merged sequence as `leftPatchSequence.appended(innerPatchSequence)`.
    //
    // 4.2 To merge last segment with `innerPatchSequence` build `rightPatchSequence`:
    //
    // 4.2.1
    //                 u
    //              (-----)            last patch segment
    //        s     |       u
    // X------------)[------------X    `rightPatchSequence`
    //
    // 4.2.2
    //                 s
    //              (-----)            last patch segment
    //              s
    // X--------------------------X    `rightPatchSequence`
    //
    // Then get merged sequence as `rightPatchSequence.prepended(innerPatchSequence)`.

    /**
     * Properties of either left or right side of patch sequence.
     */
    abstract sealed class PatchBoundInfo(
      /**
       * If class represents left side info:
       * <tr>contains `patchLowerBound` (see p.1) with corresponding segment of old control sequence;</tr>
       * <tr>`null` iff first segment of patch sequence is first segment of whole control sequence.</tr>
       *
       * If class represents right side info:
       * <tr>contains `patchUpperBound` (see p.1) with corresponding segment of old control sequence;</tr>
       * <tr>`null` iff last segment of patch sequence is last segment of whole control sequence.</tr>
       */
      val patchBoundTruncation: SegmentLikeT.Truncation[E, D, ControlValue[E, D, V], Any] | Null,
      /**
       * `true` if corresponding adjacent segment of `zsegment` is eager (see p.3).
       */
      val zsegmentBoundIsEager: Boolean,
    )

    /**
     * [[PatchBoundInfo]] for case when condition from p.2 isn't satisfied:
     * <tr>`patchLowerBound` == `zsegment.lowerBound` - for left side info;</tr>
     * <tr>`patchUpperBound` == `zsegment.upperBound` - for right side info.</tr>
     */
    case class NonShiftedPatchBoundInfo(
      override val patchBoundTruncation: SegmentLikeT.Truncation[E, D, ControlValue[E, D, V], Any] | Null,
      override val zsegmentBoundIsEager: Boolean,
    ) extends PatchBoundInfo(
      patchBoundTruncation,
      zsegmentBoundIsEager
    )

    /**
     * [[PatchBoundInfo]] for case when condition from p.2 is satisfied:
     * <tr>`patchLowerBound` != `zsegment.lowerBound` - for left side info;</tr>
     * <tr>`patchUpperBound` != `zsegment.upperBound` - for right side info.</tr>
     */
    case class ShiftedPatchBoundInfo(
      override val patchBoundTruncation: SegmentLikeT.Truncation[E, D, ControlValue[E, D, V], Any] | Null,
      override val zsegmentBoundIsEager: Boolean,
      /**
       * <tr>`leftPatchSequence` - if class represents left side info;</tr>
       * <tr>`rightPatchSequence` - if class represents right side info;</tr>
       * (see p.4.1 and 4.2)
       */
      patchBoundSequence: TreapSegmentSeq[E, D, ControlValue[E, D, V]]
    ) extends PatchBoundInfo(
      patchBoundTruncation,
      zsegmentBoundIsEager
    )

    object PatchBoundInfo {

      def getLeftSideInfo: PatchBoundInfo = makeInfo(LeftSideFactory)

      def getRightSideInfo: PatchBoundInfo = makeInfo(RightSideFactory)

      // Private section ---------------------------------------------------------- //

      private def makeInfo(factory: Factory): PatchBoundInfo = {
        var nextStep = true
        var eagerAdjacent = true
        var undefinedAdjacent = true
        var patchBoundIsShifted = false
        var patchBoundIsLazy = false
        var prevZsegment = zsegment
        val segmentIterator = factory.getIterable(zsegment).drop(1).iterator // skip `zsegment` itself
        while (segmentIterator.hasNext && nextStep) {
          val currZsegment = segmentIterator.next()
          val currControlValue = currZsegment.self.secondSeqSegment.value
          if (undefinedAdjacent) {
            undefinedAdjacent = false
            eagerAdjacent = currControlValue.isEager
          }
          nextStep = currControlValue.isEager && currControlValue.isUnstable
          if (nextStep) {
            patchBoundIsShifted = true
            prevZsegment = currZsegment
          } else {
            patchBoundIsLazy = currControlValue.isLazy
          }
        }
        factory.build(eagerAdjacent, patchBoundIsShifted, patchBoundIsLazy, prevZsegment)
      }

      private trait Factory {

        def build(
          eagerAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo

        def getIterable(zsegment: ZSegment[E, D, V]): Iterable[ZSegment[E, D, V]]
      }

      private object LeftSideFactory extends Factory {

        override def build(
          eagerAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo = {

          val patchBoundTruncation = patchBoundZsegment match {
            case s: ZippedSegmentWithPrev[E, D, V, ControlValue[E, D, V], ZValue[E, D, V], _, _] =>
              s.back.secondSeqSegment.truncation(s.lowerBound)
            case _ =>
              null
          }
          if (patchBoundIsShifted) {
            val patchBoundSequence: TreapSegmentSeq[E, D, ControlValue[E, D, V]] = patchBoundZsegment match {
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
            ShiftedPatchBoundInfo(patchBoundTruncation, eagerAdjacent, patchBoundSequence)
          } else {
            NonShiftedPatchBoundInfo(patchBoundTruncation, eagerAdjacent)
          }
        }

        override def getIterable(zsegment: ZSegment[E, D, V]): Iterable[ZSegment[E, D, V]] = zsegment.backwardIterable
      }

      private object RightSideFactory extends Factory {

        override def build(
          eagerAdjacent: Boolean,
          patchBoundIsShifted: Boolean,
          patchBoundIsLazy: Boolean,
          patchBoundZsegment: ZSegment[E, D, V]
        ): PatchBoundInfo = {

          val patchBoundTruncation = patchBoundZsegment match {
            case s: ZippedSegmentWithNext[E, D, V, ControlValue[E, D, V], ZValue[E, D, V], _, _] =>
              s.secondSeqSegment.truncation(s.upperBound)
            case _ =>
              null
          }
          if (patchBoundIsShifted) {
            val patchBoundSequence: TreapSegmentSeq[E, D, ControlValue[E, D, V]] = patchBoundZsegment match {
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
            ShiftedPatchBoundInfo(patchBoundTruncation, eagerAdjacent, patchBoundSequence)
          } else {
            NonShiftedPatchBoundInfo(patchBoundTruncation, eagerAdjacent)
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
        li.patchBoundSequence.appended(ri.patchBoundSequence)
      // See p.3 for all cases below.
      case (li: ShiftedPatchBoundInfo, ri: NonShiftedPatchBoundInfo) =>
        val innerPatchSequence = makeControlSeq(zsegment, baseSeq, li.zsegmentBoundIsEager, ri.zsegmentBoundIsEager)
        li.patchBoundSequence.appended(innerPatchSequence)
      case (li: NonShiftedPatchBoundInfo, ri: ShiftedPatchBoundInfo) =>
        val innerPatchSequence = makeControlSeq(zsegment, baseSeq, li.zsegmentBoundIsEager, ri.zsegmentBoundIsEager)
        ri.patchBoundSequence.prepended(innerPatchSequence)
      case (li: NonShiftedPatchBoundInfo, ri: NonShiftedPatchBoundInfo) =>
        makeControlSeq(zsegment, baseSeq, li.zsegmentBoundIsEager, ri.zsegmentBoundIsEager)
    }
    val tmpSeq =
      if (leftSideInfo.patchBoundTruncation != null) {
        val tr: SegmentLikeT.Truncation[E, D, ControlValue[E, D, V], Any] = leftSideInfo.patchBoundTruncation
        tr.appended(patchSequence)
      } else {
        patchSequence
      }
    val newSeq =
      if (rightSideInfo.patchBoundTruncation != null) {
        val tr: SegmentLikeT.Truncation[E, D, ControlValue[E, D, V], Any] = rightSideInfo.patchBoundTruncation
        tr.prepended(tmpSeq)
      }
      else {
        tmpSeq
      }
    // Performance note
    // `newSeq` is always treap based. `convertMap` doesn't perform real conversion for it.
    // Instead it just returns unmodified input sequence.
    TreapOrderedMap.getFactory.convertMap(newSeq)
  }

  /**
   * Builds control sequence from a new sequence `baseSeq` that was evaluated for a given `zsegment`.
   *
   * `eagerLowerBound` and `eagerUpperBound` input arguments define bound conditions:
   * <tr>- if indicator is `false` then corresponding adjacent segment of `zsegment` is lazy.</tr>
   *
   * Let's denote:
   * {{{
   *               zsegment
   *       !(------------------]!
   *      /                      \
   *   eagerLowerBound == true    eagerUpperBound == true
   *
   *               zsegment
   *       ?(-------------------]?
   *      /                       \
   *   eagerLowerBound == false    eagerUpperBound == false
   * }}}
   *
   * Segments of `baseSeq` are mapped to the segments of output control sequence. Output segment may get either
   * `stable` or `unstable` control value. Segment is stable iff all its adjacent segments are eager (non-lazy).
   *
   * Corollaries.
   *
   * 1. If some side is lazy then corresponding bound segment of new control sequence MUST be unstable.
   * {{{
   *               zsegment
   *       ?(-------------------
   *
   *   output:
   *   ------------)[-----------
   *     unstable
   * }}}
   *
   * 2. If some side is eager than bound segment MAY be stable (depending on the other adjacent segment).
   * {{{
   *             zsegment
   *       !(--------------]?
   *
   *   output:
   *   -----------)[-------------
   *      stable      unstable
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
   * @param baseSeq sequence that was evaluated for `zsegment` to patch corresponding base segment.
   * @param eagerLowerBound `true` if `zsegment` is first segment of sequence or if previous segment has eager control
   *                        value.
   * @param eagerUpperBound `true` if `zsegment` is last segment of sequence or if next segment has eager control value.
   */
  protected final def makeControlSeq(
    zsegment: ZSegment[E, D, V],
    baseSeq: SegmentSeq[E, D, V],
    eagerLowerBound: Boolean,
    eagerUpperBound: Boolean
  ): TreapSegmentSeq[E, D, ControlValue[E, D, V]] = {
    //
    //       !(-----------]!       - zsegment
    // X------------------------X  - output
    //           stable
    if (eagerLowerBound && eagerUpperBound) {
      makeUniformControlSeq(EagerValue.stable)

    } else {
      val boundSegments = SegmentSeqOps.getBoundSegments(zsegment, baseSeq)

      //          (---]            - zsegment
      //  X--)[----------)[----X   - baseSeq
      //  X--------------------X   - output
      //            \
      //        stable if both `eagerLowerBound` and `eagerUpperBound` are `true`
      if (domainOps.segmentUpperOrd.eqv(boundSegments._1, boundSegments._2)) {
        makeUniformControlSeq(EagerValue.cons(isStable = eagerLowerBound && eagerUpperBound))

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
            if (eagerLowerBound == eagerUpperBound) {
              makeUniformControlSeq(EagerValue.stable)

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
                EagerValue.cons(eagerLowerBound),
                EagerValue.cons(eagerUpperBound),
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
            if (!eagerLowerBound) {
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
            if (!eagerUpperBound) {
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
                makeNonuniformControlSeq(root, EagerValue.cons(eagerUpperBound))
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
  }

  /**
   * @return segment of lazy sequence that corresponds to input `zsegment`.
   */
  protected final def makeSegment(segment: ZSegment[E, D, V]): LazySegment[E, D, V] =
    eval(segment) match {
      case s: ZSegmentInner[E, D, V] => LazyInnerSegment(this, s)
      case s: ZSegmentInitial[E, D, V] => LazyInitialSegment(this, s)
      case s: ZSegmentTerminal[E, D, V] => LazyTerminalSegment(this, s)
      case s: ZSegmentSingle[E, D, V] => LazySingleSegment(this, s)
      case _ => throwSegmentMustHaveOneOfBaseTypes(segment) // just to remove warning
    }

  /**
   * @return segment of lazy sequence that has previous segment and corresponds to input `zsegment`.
   */
  protected final def makeSegmentWithPrev(segment: ZSegmentWithPrev[E, D, V]): LazySegmentWithPrev[E, D, V] =
    eval(segment) match {
      case s: ZSegmentInner[E, D, V] => LazyInnerSegment(this, s)
      case s: ZSegmentTerminal[E, D, V] => LazyTerminalSegment(this, s)
      case _ => throwSegmentMustBeLastOrWithNext(segment) // just to remove warning
    }

  /**
   * @return segment of lazy sequence that has next segment and corresponds to input `zsegment`.
   */
  protected final def makeSegmentWithNext(segment: ZSegmentWithNext[E, D, V]): LazySegmentWithNext[E, D, V] =
    eval(segment) match {
      case s: ZSegmentInner[E, D, V] => LazyInnerSegment(this, s)
      case s: ZSegmentInitial[E, D, V] => LazyInitialSegment(this, s)
      case _ => throwSegmentMustBeFirstOrWithPrev(segment) // just to remove warning
    }

  protected final def throwSegmentMustHaveOneOfBaseTypes(segment: Segment[E, D, ?]): Nothing = {
    val segmentStr = SetBuilderFormat.segment(segment, (e: E) => e.toString, (v: Any) => v.toString)
    throw new AssertionError(
      s"Expected that segment $segmentStr has one of base types: inner, initial, terminal or single."
    )
  }

  protected final def throwSegmentMustBeLastOrWithNext(segment: Segment[E, D, ?]): Nothing = {
    val segmentStr = SetBuilderFormat.segment(segment, (e: E) => e.toString, (v: Any) => v.toString)
    throw new AssertionError(
      s"Expected that segment $segmentStr is either last or has next segment."
    )
  }

  protected final def throwSegmentMustBeFirstOrWithPrev(segment: Segment[E, D, ?]): Nothing = {
    val segmentStr = SetBuilderFormat.segment(segment, (e: E) => e.toString, (v: Any) => v.toString)
    throw new AssertionError(
      s"Expected that segment $segmentStr is either first or has previous segment."
    )
  }
}

object AbstractLazyTreapSegmentSeq {

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

  type BaseSegmentSeq[E, D <: Domain[E], V] =
    SegmentSeqT[
      E,
      D,
      V,
      TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V]
    ]

  type ControlSegment[E, D <: Domain[E], V] =
    SegmentT[
      E,
      D,
      ControlValue[E, D, V],
      TreapSegmentBase[E, D, ControlValue[E, D, V]] | UniformSingleSegment[E, D, ControlValue[E, D, V]]
    ]

  type ControlSegmentSeq[E, D <: Domain[E], V] =
    SegmentSeqT[
      E,
      D,
      ControlValue[E, D, V],
      TreapSegmentBase[E, D, ControlValue[E, D, V]] | UniformSingleSegment[E, D, ControlValue[E, D, V]]
    ]

  type LazySegment[E, D <: Domain[E], V] =
    SegmentT[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]

  type LazyFirstSegment[E, D <: Domain[E], V] =
    SegmentT.First[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]

  type LazyLastSegment[E, D <: Domain[E], V] =
    SegmentT.Last[E, D, V, LazySegmentBase[E, D, V]] with LazySegmentBase[E, D, V]


  sealed trait ControlValue[E, D <: Domain[E], V] {

    def isStable: Boolean

    def isUnstable: Boolean

    def isLazy: Boolean

    def isEager: Boolean
  }

  final case class LazyValue[E, D <: Domain[E], V](
    private val seqFunc: () => SegmentSeq[E, D, V]
  ) extends ControlValue[E, D, V] {

    override def isStable: Boolean = false

    override def isUnstable: Boolean = true

    override def isLazy: Boolean = true

    override def isEager: Boolean = false

    def eval: SegmentSeq[E, D, V] = seqFunc.apply()
  }

  final case class EagerValue[E, D <: Domain[E], V] private (
    private val stable: Boolean
  ) extends ControlValue[E, D, V] {

    override def isStable: Boolean = stable

    override def isUnstable: Boolean = !stable

    override def isLazy: Boolean = false

    override def isEager: Boolean = true
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

  object ControlTupleOps {

    def get[E, D <: Domain[E], V](valueOps: ValueOps[V]): ValueOps[(V, ControlValue[E, D, V])] =
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

    override def moveTo(bound: Bound[E]): LazySegment[E, D, V] = sequence.getSegment(bound)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: SegmentSeq[E, D, V] = ???

    override def takenBelow: SegmentSeq[E, D, V] = ???

    override def sliced: (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = ???

    override def prepended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

    override def appended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

    override def truncation(bound: Bound[E]): SegmentLikeT.Truncation[E, D, V, LazySegmentBase[E, D, V]] = ???

    // Protected section -------------------------------------------------------- //
    override protected def original: ZSegment[E, D, V]

    final override protected val mapFunc: ZValue[E, D, V] => V = t => t._1

    final override protected def cons(
      segment: SegmentT[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]
    ): LazySegment[E, D, V] =
      sequence.makeSegment(segment)

    final override protected def consWithNext(
      segment: SegmentT.WithNext[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]
    ): LazySegmentWithNext[E, D, V] =
      sequence.makeSegmentWithNext(segment)

    final override protected def consWithPrev(
      segment: SegmentT.WithPrev[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]
    ): LazySegmentWithPrev[E, D, V] =
      sequence.makeSegmentWithPrev(segment)
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
    override def moveNext: LazySegmentWithPrev[E, D, V] = sequence.makeSegmentWithPrev(original.moveNext)

    // Protected section -------------------------------------------------------- //
    protected override def original: ZSegmentWithNext[E, D, V]
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
    override def movePrev: LazySegmentWithNext[E, D, V] = sequence.makeSegmentWithNext(original.movePrev)

    // Protected section -------------------------------------------------------- //
    protected override def original: ZSegmentWithPrev[E, D, V]
  }

  /**
   * Lazy initial segment.
   */
  final case class LazyInitialSegment[E, D <: Domain[E], V](
    override val sequence: LazySegmentSeq[E, D, V],
    override protected val original: ZSegmentInitial[E, D, V]
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
    override protected val original: ZSegmentTerminal[E, D, V]
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
    override protected val original: ZSegmentInner[E, D, V]
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
    override protected val original: ZSegmentSingle[E, D, V]
  ) extends MappedSegmentT.Single[E, D, ZValue[E, D, V], V, ZSegmentBase[E, D, V], LazySegmentBase[E, D, V]]
    with LazySegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: LazySingleSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: LazySingleSegment[E, D, V] = this

    override def moveToLast: LazySingleSegment[E, D, V] = this

    override def moveTo(bound: Bound[E]): LazySingleSegment[E, D, V] = this
  }
}