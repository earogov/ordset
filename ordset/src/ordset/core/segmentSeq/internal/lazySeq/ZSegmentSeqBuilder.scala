package ordset.core.segmentSeq.internal.lazySeq

import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.*
import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.internal.SegmentSeqExceptionUtil.*
import ordset.core.segmentSeq.internal.mappedSeq.NonMergingMappedValueOrderedMap
import ordset.core.segmentSeq.internal.lazySeq.ControlValue.*
import ordset.core.segmentSeq.map.{TreapOrderedMap, UniformOrderedMap, ZippedOrderedMap}
import ordset.core.segmentSeq.util.{TreapSegmentSeqBuilder, TreapSegmentSeqUtil}
import ordset.random.RngManager
import ordset.tree.treap.mutable.MutableTreap
import ordset.util.{OptionUtil, NowarnFilter}

import scala.annotation.tailrec

protected[ordset] object ZSegmentSeqBuilder {

  /**
   * Creates lazy segment sequence (*1) using two input sequences:
   * <div>1. `baseSeq` - segment sequence with base values;</div>
   * <div>2. `supplierSeq` - segment sequence with optional lazy values.</div>
   * <div></div>
   * (*1) - Actually method returns zipped sequence [[ZSegmentSeq]]. But it is used as internal representation
   *        of [[AbstractLazyTreapSegmentSeq]]. So we can use [[ZSegmentSeq]] and lazy segment sequence as synonyms.
   * 
   * <div>
   *   If segment of `supplierSeq` has [[None]] value then corresponding segments of output sequence have the same
   *   values as `baseSeq`.
   * </div>
   * <div>
   *   If segment of `supplierSeq` has [[Some]] value with a function F: `() => segmentSeqF`, then corresponding
   *   segments of output sequence are lazy. Their values are completely defined by sequence `segmentSeqF` and
   *   corresponding values of `baseSeq` are ignored.
   * </div>
   * {{{
   *
   *         A             B              C
   * X------------](-------------)[-------------X  `baseSeq`
   *
   *   None     Some(() => segmentSeqF)    None
   * X------](-------------------------)[-------X  `supplierSeq`
   *
   *   D        E         F           G       H
   * X---](---------](---------)[----------](---X  `segmentSeqF`
   *
   *     A       E         F        G        C
   * X------](------](---------)[------)[-------X  output sequence after
   *                                               lazy value computation
   * }}}
   */
  final def build[E, D[X] <: Domain[X], V](
    baseSeq: SegmentSeq[E, D, V],
    supplierSeq: SupplierSegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZSegmentSeq[E, D, V] = 
    baseSeq match {
      case baseSeq: LazySegmentSeq[E, D, V] => buildFromLazyBaseSeq(baseSeq, supplierSeq)
      case _ => buildFromNonLazyBaseSeq(baseSeq, supplierSeq)
    }

  /**
   * Prepends lazy sequence `leftSeq` to lazy sequence `rightZtruncation.sequence` before bound `rightZtruncation.bound`
   * (see [[SegmentSeqT.prependBeforeBound]]). 
   */ 
  final def prependZippedSeq[E, D[X] <: Domain[X], V](
    rightZtruncation: ZTruncation[E, D, V],
    leftSeq: ZSegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZSegmentSeq[E, D, V] = 
    rightZtruncation.bound match {
      case b: Bound[E] => 
        val leftZsegment = leftSeq.getSegmentForBound(b.provideUpper)
        appendZippedSegment(b, leftZsegment, rightZtruncation.segment)
      case ExtendedBound.BelowAll =>
        rightZtruncation.segment.self.sequence
      case ExtendedBound.AboveAll =>
        leftSeq
    }

  /**
   * Appends lazy sequence `rightSeq` to lazy sequence `leftZtruncation.sequence` above bound `leftZtruncation.bound`
   * (see [[SegmentSeqT.appendAboveBound]]). 
   */ 
  final def appendZippedSeq[E, D[X] <: Domain[X], V](
    leftZtruncation: ZTruncation[E, D, V],
    rightSeq: ZSegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZSegmentSeq[E, D, V] =
    leftZtruncation.bound match {
      case b: Bound[E] => 
        val rightZsegment = rightSeq.getSegmentForBound(b.provideLower)
        appendZippedSegment(b, leftZtruncation.segment, rightZsegment)
      case ExtendedBound.BelowAll =>
        rightSeq
      case ExtendedBound.AboveAll =>
        leftZtruncation.segment.self.sequence
    }

  /**
   * Appends lazy sequence `rightZsegment.sequence` to lazy sequence `leftZsegment.sequence` above specified `bound`
   * (see [[SegmentSeqT.appendAboveBound]]). 
   *
   * Input `leftZsegment` must satisfy precondition:
   * {{{
   *   leftZsegment.containsBound(bound.provideUpper) == true    (1)
   * }}}
   * Input `rightZsegment` must satisfy precondition:
   * {{{
   *   rightZsegment.containsBound(bound.provideLower) == true       (2)
   * }}}
   * Having segments as input parameters allows to avoid their repeated search if they are already known before
   * method call.
   *
   * Note, if preconditions 1 and 2 are violated, the behavior of method is undefined.
   * {{{
   *
   *  left sequence:
   *
   *               bound   leftZsegment
   *                 ]     /
   *  X-------](--------------)[---------X
   *   (A, s)       (B, s)       (C, s)
   *
   *  right sequence:    
   *                      rightZsegment
   *                     /
   *  X---------)[----------](-----------X
   *    (D, u)      (-, ?)      (E, u)
   *
   *  output sequence:
   *
   *  X-------](-----](-----](-----------X
   *   (A, s)  (B, u)  (-, ?)   (E, u)
   *
   *  where
   *  (A, ...) - value of base sequence;
   *  (..., u) - control value: s - strict stable, u - strict unstable, ? - lazy.
   * }}}
   */
  final def appendZippedSegment[E, D[X] <: Domain[X], V](
    bound: Bound[E],
    leftZsegment: ZSegment[E, D, V],
    rightZsegment: ZSegment[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZSegmentSeq[E, D, V] = {

    //             bound
    //               ]
    //  --)[--------------](--------    left sequence
    //             s
    //
    //  ------)[--------------](----    right sequence
    //                u
    //
    // Depending on control values of adjacent segments after merge next cases are possible:
    //
    //      s    u    ?
    //  s   +    -    -
    //  u   -    -    +
    //  ?   -    +    +
    //
    // where: `+` means that no additional correction required; `-` means correction.
    //
    // 1) ?-s and s-? cases
    //
    // Each stable segment becomes unstable.
    //
    //             bound
    //               ]
    //  --)[--------------](--------    left sequence
    //  u          ?           u
    //
    //  ------)[--------------](----    right sequence
    //     s          s          s
    //
    //  --)[---------](-------](----    output sequence
    //  u       ?         u      s
    //
    // 2) u-s, s-u and u-u cases
    //
    // 2.1) Each unstable segment may become stable.
    //
    //             bound
    //               ]
    //  --)[--------------](--------    left sequence
    //  s          u           ?
    //
    //  ------)[--------------](----    right sequence
    //     ?          u          s
    //
    //  --)[---------](-------](----    output sequence
    //  s       s         s      s
    //
    // 2.2) Each stable segment may become unstable.
    //
    //  Assume segments S1 and S2 have the same value.
    //
    //             bound
    //         S1    ]
    //  --)[--------------](--------    left sequence
    //  s          s           s
    //                 S2
    //  ------)[--------------](----    right sequence
    //     s          u          ?
    //
    //  --)[------------------](----    output sequence
    //  s             u          ?

    def takeAboveAndPrependControlValue(
      zsegment: ZSegment[E, D, V],
      value: ControlValue[E, D, V]
    ): SegmentSeq[E, D, ControlValue[E, D, V]] =
      zsegment.self.secondSeqUpperTruncation.prepend(makeUniformControlSeq(value))

    def takeBelowAndAppendControlValue(
      zsegment: ZSegment[E, D, V],
      value: ControlValue[E, D, V]
    ): SegmentSeq[E, D,  ControlValue[E, D, V]] =
      zsegment.self.secondSeqLowerTruncation.append(makeUniformControlSeq(value))

    def controlSeqTruncation(
      zsegment: ZSegment[E, D, V],
      bound: ExtendedBound[E]
    ): ControlTruncation[E, D, V] =
      zsegment.self.secondSeqSegment.truncation(bound)

    val leftControlValue = leftZsegment.value._2
    val rightControlValue = rightZsegment.value._2

    val rightZippedSeq = rightZsegment.self.sequence

    val newBaseSeq = TreapSegmentSeqUtil.appendAboveTruncation(
      leftZsegment.self.firstSeqSegment.truncation(bound),
      rightZippedSeq.firstSeq
    )

    val newControlSeq = {
      // s-s, u-?, ?-u, ?-? => no correction
      if (
        leftControlValue.isStable && rightControlValue.isStable ||
        leftControlValue.isLazy && rightControlValue.isUnstable ||  // note, lazy value is also unstable
        leftControlValue.isUnstable && rightControlValue.isLazy
      ) {
        TreapSegmentSeqUtil.appendAboveTruncation(
          controlSeqTruncation(leftZsegment, bound),
          rightZippedSeq.secondSeq
        )
      // ?-s => stable becomes unstable
      } else if (leftControlValue.isLazy) {
        TreapSegmentSeqUtil.appendAboveTruncation(
          controlSeqTruncation(leftZsegment, bound),
          takeAboveAndPrependControlValue(rightZsegment, StrictValue.unstable)
        )
      // s-? => stable becomes unstable
      } else if (rightControlValue.isLazy) {
        TreapSegmentSeqUtil.prependBelowTruncation(
          controlSeqTruncation(rightZsegment, bound),
          takeBelowAndAppendControlValue(leftZsegment, StrictValue.unstable)
        )
      // s-u
      } else if (leftControlValue.isStable) {
        // s-u, there is no lazy segments after unstable =>
        // unstable becomes stable (both control segments are merged)
        if (!rightZsegment.hasNextSuchThat(_.value._2.isLazy)) {
          TreapSegmentSeqUtil.appendAboveTruncation(
            leftZsegment.self.secondSeqLowerTruncation,
            takeAboveAndPrependControlValue(rightZsegment, StrictValue.stable)
          )
        // s-u, same base values, there is lazy segment after unstable =>
        // stable becomes unstable (both control segments are merged)
        } else if (valueOps.eqv(leftZsegment.value._1, rightZsegment.value._1)) {
          TreapSegmentSeqUtil.appendAboveTruncation(
            leftZsegment.self.secondSeqLowerTruncation,
            takeAboveAndPrependControlValue(rightZsegment, StrictValue.unstable)
          )
        // s-u, different base values, there is lazy segment after unstable =>
        // no correction
        } else {
          TreapSegmentSeqUtil.appendAboveTruncation(
            controlSeqTruncation(leftZsegment, bound),
            rightZippedSeq.secondSeq
          )
        }
      // u-s
      } else if (rightControlValue.isStable) {
        // u-s, there is no lazy segments before unstable =>
        // unstable becomes stable (both control segments are merged)
        if (!rightZsegment.hasPrevSuchThat(_.value._2.isLazy)) {
          TreapSegmentSeqUtil.appendAboveTruncation(
            leftZsegment.self.secondSeqLowerTruncation,
            takeAboveAndPrependControlValue(rightZsegment, StrictValue.stable)
          )
        // u-s, same base values, there is lazy segment before unstable =>
        // stable becomes unstable (both control segments are merged)
        } else if (valueOps.eqv(leftZsegment.value._1, rightZsegment.value._1)) {
          TreapSegmentSeqUtil.appendAboveTruncation(
            leftZsegment.self.secondSeqLowerTruncation,
            takeAboveAndPrependControlValue(rightZsegment, StrictValue.unstable)
          )
        // u-s, different base values, there is lazy segment before unstable =>
        // no correction
        } else {
          TreapSegmentSeqUtil.appendAboveTruncation(
            controlSeqTruncation(leftZsegment, bound),
            rightZippedSeq.secondSeq
          )
        }
      // u-u
      } else {
        val leftIsLazy = leftZsegment.hasPrevSuchThat(_.value._2.isLazy)
        val rightIsLazy = rightZsegment.hasNextSuchThat(_.value._2.isLazy)
        if (leftIsLazy == rightIsLazy) {
          // u-u, surrounded by lazy segments =>
          // both segments remain unstable and are merged
          if (leftIsLazy) {
            TreapSegmentSeqUtil.appendAboveTruncation(
              leftZsegment.self.secondSeqLowerTruncation,
              rightZsegment.self.secondSeqUpperTruncation.segment.takeAbove
            )
          // u-u, no one side has adjacent lazy segment =>
          // both segments become stable and are merged
          } else {
            TreapSegmentSeqUtil.appendAboveTruncation(
              leftZsegment.self.secondSeqLowerTruncation,
              takeAboveAndPrependControlValue(rightZsegment, StrictValue.stable)
            )
          }
        } else if (valueOps.eqv(leftZsegment.value._1, rightZsegment.value._1)) {
          // u-u, same base values, there is lazy segment before left unstable or
          // there is lazy segment after right unstable => both segments remain unstable and are merged
          TreapSegmentSeqUtil.appendAboveTruncation(
            leftZsegment.self.secondSeqLowerTruncation,
            rightZsegment.self.secondSeqUpperTruncation.segment.takeAbove
          )
        } else {
          // u-u, different base values, there is lazy segment before left unstable,
          // there is no lazy segments after right unstable => right unstable becomes stable
          if (leftIsLazy) {
            TreapSegmentSeqUtil.appendAboveTruncation(
              controlSeqTruncation(leftZsegment, bound),
              takeAboveAndPrependControlValue(rightZsegment, StrictValue.stable)
            )
          // u-u, different base values, there is no lazy segments before left unstable,
          // there is lazy segment after right unstable => left unstable becomes stable
          } else {
            TreapSegmentSeqUtil.prependBelowTruncation(
              controlSeqTruncation(rightZsegment, bound),
              takeBelowAndAppendControlValue(leftZsegment, StrictValue.stable)
            )
          }
        }
      }
    }
    makeZippedSeq(newBaseSeq, newControlSeq)
  }

  /**
   * Takes zipped sequence in arbitrary state and builds equivalent strict zipped sequence. Thus, all segments in
   * the result sequence are strict stable.
   * {{{
   * 
   * input zipped sequence:
   * 
   * X----------](---------)[----------)[----------X
   *     (A, u)     (-, ?)     (B, s)      (-, ?)
   * 
   *                        VVV
   * 
   * output zipped sequence:
   * 
   * X----------](---](----)[-----------)[---)[----X
   *    (A, s)     /     \      (B, s)     /     \
   *           (C, s)   (D, s)         (E, s)   (F, s)
   * 
   *  where
   *  (A, ...) - value of base sequence;
   *  (..., u) - control value: s - strict stable, u - strict unstable, ? - lazy.
   * }}}
   * 
   * <h3>Complexity analysis</h3>
   * 
   * To build strict sequence one needs to evaluate all lazy segments and merge received sequences into common treap
   * data structure. The overall time cost depends on:
   * <div>- the number of nodes in the initial base sequence;</div>
   * <div>- the number of lazy segments to evaluate;</div>
   * <div>- the number of nodes in each lazy sequence;</div>
   * <div>- the underlying data structure of each lazy sequence.</div>
   * 
   * For simplicity let's consider idealized case of merging S segments with N nodes in each.
   * {{{
   * 
   * initial base sequence:
   * 
   * X----------------------------------------X
   * 
   * initial control sequence (S lazy segments):
   * 
   *      lazy          lazy          lazy
   * X-----------)[-------------](------------X
   *        /            |               \
   *   () => seq1     () => seq2       () => seq3
   * 
   * seq1:
   *    N nodes
   * X...........)[---------------------------X
   * 
   * seq2:
   *                  N nodes
   * X-----------)[.............](------------X
   * 
   * seq3:
   *                                N nodes
   * X--------------------------](............X
   * }}}
   * 
   * Case I.
   * 
   * Assume we have S non-treap data structures, thus we have to build treap with N*S sorted keys. This can be done
   * with approximately 2*N*S operations. But for each new segment sequence we are going to append we also need to 
   * to find node at lower bound of lazy segment (from which we will start appending). Assuming that all segment
   * sequences provide effective logarithmic search we get the following total cost for case I:
   *
   * C,,I,, = 2*N*S + (S-1)log2(N)
   * 
   * Case II.
   * 
   * Now consider the case when there is S treap data structures. To merge each pair of treaps at bound `b` we need to:
   * <div>1. split left tree at bound `b`;</div>
   * <div>2. split right tree at bound `b`;</div>
   * <div>3. merge corresponding splitted parts.</div>
   * 
   * Each split requires to traverse tree from the root to one of the leaves. Average cost of this operation is:
   *  
   * C,,split,, = log2(N)
   * 
   * To merge treaps we need to traverse in reversed order all nodes visited at steps 1 and 2 and rearrange them.
   * So the cost is:
   *
   * C,,merge,, = 2log2(N)
   * 
   * The total cost of steps 1, 2, 3:
   * 
   * C,,1,, = 4log2(N) = 2log2(N^2)
   * 
   * Since we have S lazy segments, we need to perform (S-1) such merges. But after each step size of the left tree
   * will increase by N nodes. So the cost of the 2-nd merge is:
   * 
   * C,,2,, = log2(2N) + log2(N) + log2(2N) + log2(N) = 2log2(2 * N^2)
   *              |          |          \     /
   *          split of     split of      merge
   *         left tree    right tree
   * 
   * For i-th merge we get:
   * 
   * C,,i,, = 2log2(i * N^2)
   * 
   * And sum of (S-1) such terms will be:
   *
   * C,,II,, = 2log2(1 * N^2) + 2log2(2 * N^2) + ... + 2log2((S-1) * N^2) = 2log2((S-1)! * N^(2(S - 1)))
   * 
   * C,,II,, = 2log2((S-1)!) + 4(S-1)log2(N)
   * 
   * Alternatively we could build the result treap just like in case I appending N*S sorted keys. So let's compare
   * the costs:
   *
   * Diff = C,,I,, - C,,II,, = N*S - log2((S-1)!) - 3/2*(S-1)log2(N)
   * 
   * Function is positive in the regions of parametric space (N, S) where merging of treaps is more efficient than 
   * building new one from scratch. The bound is defined by equation:
   * 
   * Diff = 0
   * 
   * For large N we can drop logarithmic term 3/2*(S-1)log2(N) compared to N*S and get:
   *
   * N*S - log2((S-1)!) = 0
   * 
   * N = 1/S*log2((S-1)!)
   * 
   * Despite the factorial this function increases rather slowly. And even for exact equation Diff = 0 we get,
   * for instance, N ≈ 15 for S = 1000. So if we have 1000 lazy segments and more than 15 nodes in each, then
   * algorithm II is more optimal. The fewer lazy segments we have, the lower the threshold for N. In other words,
   * algorithm I is only good for cases, when we have a lot of small lazy sequences to merge.
   * 
   * Common case.
   * 
   * In the common case we will have some mix of treap and non-treap data structures to merge. We will prefer
   * algorithm II whenever possible and fallback to I for non-treap data structures. Also logarithmic overhead is 
   * added for case of non-treap data structures due to switch of algorithms. So the total cost is in range:
   *
   * C,,common,, = min(C,,I',, + , C,,II,,) ... max(C,,I',,, C,,II,,)
   * 
   * where C,,I',, = 2*N*S + 2(S-1)log2(N) + log2((S-1)!)
   */
  final def strictZippedSeq[E, D[X] <: Domain[X], V](
    zippedSeq: ZSegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZSegmentSeq[E, D, V] =
    if (ZSegmentSeqUtil.isTotallyStrict(zippedSeq)) zippedSeq
    else {
      val oldBaseSeq = zippedSeq.firstSeq
      val newBaseSeq = zippedSeq.segments
        .foldLeft(makeUniformBaseSeq: TreapSegmentSeq[E, D, V]) { (leftSeq, zsegment) =>
          val rightSeq = zsegment.value._2 match {
            case value: StrictValue[E, D, V] => oldBaseSeq
            case value: LazyValue[E, D, V] => value.compute
          }
          val bound = zsegment.lower
          val truncation = leftSeq.getSegmentForExtended(bound).truncation(bound)
          TreapSegmentSeqUtil.appendAboveTruncation(truncation, rightSeq)
        }
      val newControlSeq = makeUniformControlSeq[E, D, V](StrictValue.stable)
      makeZippedSeq(newBaseSeq, newControlSeq)
    }

  // Private section ---------------------------------------------------------- //
  private type ControlBuffer[E, D[X] <: Domain[X], V] = List[MutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]]]

  private type ControlSeqBuilder[E, D[X] <: Domain[X], V] = TreapSegmentSeqBuilder.Mutable[E, D, ControlValue[E, D, V]]


  private type ControlGenSegment[E, D[X] <: Domain[X], V] = Segment[E, D, ControlValue[E, D, V]]

  private type ControlGenSegmentWithNext[E, D[X] <: Domain[X], V] = Segment.WithNext[E, D, ControlValue[E, D, V]]

  private type ControlGenSegmentWithPrev[E, D[X] <: Domain[X], V] = Segment.WithPrev[E, D, ControlValue[E, D, V]]

  private type ControlGenSegmentSeq[E, D[X] <: Domain[X], V] = SegmentSeq[E, D, ControlValue[E, D, V]]


  private type LazyMaskSegment[E, D[X] <: Domain[X], V] =
    MappedSegment[E, D, SeqSupplier[E, D, V], Boolean, Any]

  private type LazyMaskSegmentWithPrev[E, D[X] <: Domain[X], V] =
    MappedSegmentWithPrev[E, D, SeqSupplier[E, D, V], Boolean, Any]

  private type LazyMaskSegmentWithNext[E, D[X] <: Domain[X], V] =
    MappedSegmentWithNext[E, D, SeqSupplier[E, D, V], Boolean, Any]


  private type InitialZSegment[E, D[X] <: Domain[X], V] =
    ZippedTupleSegment[
      E,
      D,
      Boolean,
      V,
      MappedSegmentBase[E, D, SeqSupplier[E, D, V], Boolean, Any],
      Any
    ]

  private type InitialZSegmentWithPrev[E, D[X] <: Domain[X], V] =
    ZippedTupleSegmentWithPrev[
      E,
      D,
      Boolean,
      V,
      MappedSegmentBase[E, D, SeqSupplier[E, D, V], Boolean, Any],
      Any
    ]

  private type InitialZSegmentWithNext[E, D[X] <: Domain[X], V] =
    ZippedTupleSegmentWithNext[
      E,
      D,
      Boolean,
      V,
      MappedSegmentBase[E, D, SeqSupplier[E, D, V], Boolean, Any],
      Any
    ]

  private final def buildFromNonLazyBaseSeq[E, D[X] <: Domain[X], V](
    baseSeq: SegmentSeq[E, D, V],
    supplierSeq: SupplierSegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZSegmentSeq[E, D, V] = {

    // Implementation
    //
    // To create lazy treap sequence we need base and control sequences. `baseSeq` already has proper type,
    // so we need only to convert `supplierSeq` into control sequence.
    //
    // 1. Build lazy mask sequence from `supplierSeq` which merges adjacent lazy segments.
    //
    //      None       Some(f1)   Some(f2)   None
    // X-------------)[--------)[--------)[--------X - supplier sequence
    //
    //      false             true           false
    // X-------------)[------------------)[--------X - lazy mask sequence
    //
    // 2. Iterate over lazy mask sequence. If current segment `S` has `true` value then:
    //
    // 2.1. Zip lazy mask sequence with `baseSeq`. Get previous zipped segment `ZP` for segment `S`.
    //      Add to control sequence unstable segment with the same bounds as `ZP` (if there is no
    //      segment with such bounds).
    //
    // 2.2. Add lazy segments to control sequence for each segment of `supplierSeq` that is inside
    //      segment `S`.
    //
    // 2.3. Zip lazy mask sequence with `baseSeq`. Get next zipped segment `ZN` for segment `S`.
    //      Add to control sequence unstable segment with the same bound as `ZN`.
    //
    //     A         B          C         D      E
    // X--------)[--------)[--------)[--------)[---X - base sequence
    //
    //      None       Some(f1)   Some(f2)    None
    // X-------------)[--------)[--------)[--------X - supplier sequence
    //
    //      false              true         false
    // X-------------)[------------------)[--------X - lazy mask sequence
    //                         S
    //
    //                        VVV
    //
    //      s      u       ?         ?      u    s
    // X--------)[---)[--------)[--------)[---)[---X - control sequence

    def addLeftUnstableSegment(
      maskSegment: LazyMaskSegmentWithPrev[E, D, V],
      builder: ControlSeqBuilder[E, D, V],
    ): ControlSeqBuilder[E, D, V] = {
      val maskTruncation = maskSegment.movePrev.upperTruncation
      val zippedTruncation = maskTruncation.zipIntoTuple(baseSeq)
      val zippedSegment = zippedTruncation.segment
      (zippedSegment: InitialZSegment[E, D, V]) match {
        case zippedSegment: InitialZSegmentWithNext[E, D, V] =>
          if (builder.hasLastBound(zippedSegment.upper))
            // Bound had been already added => nothing to do.
            builder
          else {
            zippedSegment match {
              case zippedSegment: InitialZSegmentWithPrev[E, D, V] =>
                builder.addBound(zippedSegment.lower.flipLower, StrictValue.stable)
              case _ => // nothing to do
            }
            builder.addBound(zippedSegment.upper, StrictValue.unstable)
          }
        case _ =>
          // `zippedSegment` contains previous segment of `maskSegment` => `zippedSegment` has next segment.
          throwNoNextSegment(zippedSegment)
      }: @annotation.nowarn(NowarnFilter.unreachableCase)
    }

    @tailrec
    def addLazySegments(
      supplierSegment: SupplierSegment[E, D, V],
      builder: ControlSeqBuilder[E, D, V],
    ): ControlSeqBuilder[E, D, V] =
      supplierSegment.value match {
        case Some(f) =>
          supplierSegment match {
            case s: SupplierSegmentWithNext[E, D, V] =>
              builder.addBound(s.upper, LazyValue.Default(f)(domainOps.domain))
              addLazySegments(s.moveNext, builder)
            case _ =>
              builder.setLastValue(LazyValue.Default(f)(domainOps.domain))
          }
        case _ => builder
      }

    def addRightUnstableSegment(
      maskSegment: LazyMaskSegmentWithNext[E, D, V],
      builder: ControlSeqBuilder[E, D, V]
    ): ControlSeqBuilder[E, D, V] = {
      val maskTruncation = maskSegment.moveNext.lowerTruncation
      val zippedTruncation = maskTruncation.zipIntoTuple(baseSeq)
      (zippedTruncation.segment: InitialZSegment[E, D, V]) match {
        case zippedSegment: InitialZSegmentWithNext[E, D, V] =>
          builder.addBound(zippedSegment.upper, StrictValue.unstable)
        case _ =>
          builder
      }: @annotation.nowarn(NowarnFilter.unreachableCase)
    }

    @tailrec
    def traverseMaskSeq(
      maskSegment: LazyMaskSegment[E, D, V],
      builder: ControlSeqBuilder[E, D, V]
    ): ControlSeqBuilder[E, D, V] =
      // `lazySeq` contains lazy value.
      if (maskSegment.value) {
        // p.2.1.
        maskSegment match {
          case maskSegment: LazyMaskSegmentWithPrev[E, D, V] => addLeftUnstableSegment(maskSegment, builder)
          case _ => // nothing to do
        }: @annotation.nowarn(NowarnFilter.unreachableCase)
        // p.2.2.
        addLazySegments(maskSegment.back, builder)

        if (builder.hasLastValue) builder
        else maskSegment match {
          case maskSegment: LazyMaskSegmentWithNext[E, D, V] =>
            // p.2.3.
            addRightUnstableSegment(maskSegment, builder)

            if (builder.hasLastValue) builder
            else traverseMaskSeq(maskSegment.moveNext, builder)
          case _ =>
            // Builder doesn't have last value => `addLazySegments` haven't found last segment =>
            // there must be next segment after `maskSegment`.
            throwNoNextSegment(maskSegment)
        }: @annotation.nowarn(NowarnFilter.unreachableCase)
      // `lazySeq` contains `None`.
      } else maskSegment match {
        case s: LazyMaskSegmentWithNext[E, D, V] => traverseMaskSeq(s.moveNext, builder)
        case _ => builder.setLastValue(StrictValue.stable)
      }: @annotation.nowarn(NowarnFilter.unreachableCase)

    val lazyMaskSeq = supplierSeq.map(_.isDefined)(ValueOps.booleanValueOps)

    val controlSeqBuilder = new TreapSegmentSeqBuilder.Mutable[E, D, ControlValue[E, D, V]](
      domainOps, ControlValueOps.get, rngManager
    )
    val controlSeq = traverseMaskSeq(lazyMaskSeq.firstSegment, controlSeqBuilder).buildSeq

    makeZippedSeq(TreapOrderedMap.getFactory.convertMap(baseSeq), controlSeq)
  }

  private final def buildFromLazyBaseSeq[E, D[X] <: Domain[X], V](
    baseSeq: LazySegmentSeq[E, D, V],
    supplierSeq: SupplierSegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZSegmentSeq[E, D, V] = {

    // Implementation
    //
    // Here `baseSeq` is itself lazy sequence and we need to build new one with respect of `supplierSeq`,
    // i.e. replace some regions of `baseSeq` with segments defined by `supplierSeq`.
    // Each lazy sequence internally is represented by zipped sequence [[ZSegmentSeq]]. We could just use `baseSeq`
    // directly as base sequence of new lazy sequence. But this will add redundant nested level:
    //  
    // new lazy seq
    //       |
    //      zip ---- control seq defined by `supplierSeq` (1)
    //       |
    //     `baseSeq` 
    //          |
    //         zip ---- control seq of `baseSeq` (2)
    //          |
    //        base seq of `baseSeq`
    //
    // The better way is to build control sequence that corresponds to `supplierSeq` (1 on the diagram above) 
    // and merge it with control sequence of `baseSeq` (2). Another advantage is that each lazy value of `baseSeq`
    // will remain non-computed, so we will preserve maximal laziness.
    //
    // new lazy seq
    //       |
    //      zip ---- control seq defined by `supplierSeq` (+) control seq of `baseSeq`
    //       |
    //      base seq of `baseSeq`
    //
    // The basic algorithm is following:
    //
    // 1. Map values of `supplierSeq`: segments with `Some(f)` should be converted into [[LazyValue]],
    //    with `None` -  into [[StrictValue.unsatble]].
    //    
    //      None       Some(f1)   Some(f2)   None
    // X-------------)[--------)[--------)[--------X - supplier sequence
    //
    //        u         ? (f1)    ? (f2)       u
    // X-------------)[--------)[--------)[--------X - mapped lazy sequence
    //
    // 2. For each group of adjacent segments with [[LazyValue]]:
    //
    // 2.1. Build patch for control sequence of `baseSeq`: collect all adjacent lazy segments into new treap based
    //      control sequence (see [[buildControlSeqPatch]]).
    //
    //        u         ? (f1)    ? (f2)       u
    // X-------------)[--------)[--------)[--------X - mapped lazy sequence
    //
    //           ? (f1)                 ? (f2)
    // X-----------------------)[------------------X - control sequence patch
    //
    // 2.2. Build patch for zipped sequence of `baseSeq`: zip received control sequence with uniform sequence
    //      that has any value of type `V`.
    //
    // 2.3. Apply patch to zipped sequence of `baseSeq`.
    //
    //   patch lower     current group      patch upper
    //      bound    \  of lazy segments  /    bound
    //                +------------------+
    //        u       | ? (f1)    ? (f2) |     u
    // X-------------)[--------)[--------)[--------X - mapped supplier sequence
    //                |                  |
    //  (A, u)    (-, ?)     (B, u)    (C, s) (D, s)
    // X------)[----------](--------)[--------](---X - zipped sequence of `baseSeq`
    //                |                  |
    //  (A, u)  (-, ?)|                  | (C, s) 
    // X------)[-----)[--------)[--------)[---](---X - output zipped sequence
    //                    /           \          \
    //               (-, ? (f1))   (-, ? (f2))  (D, s) 

    /**
     * Starting from `startSegment` iterates over corresponding sequence and builds new control sequence.
     * Iteration continues until first non-lazy segment is met (or until last segment of sequence).
     * <div></div>
     * 
     * Returns tuple of:
     * <div>- first non-lazy segment that was met, or [[None]] if there was no such segment;</div>
     * <div>- new control sequence.</div>
     * {{{
     * 
     *     u      ? (f1)    ? (f2)       u
     * X-------)[--------](--------)[--------X - mapped supplier sequence
     *              |                    |
     *         startSegment         outputSegment
     * 
     *        ? (f1)              ? (f2)
     * X-----------------](------------------X - output control sequence
     * }}}
     * <div></div>
     * 
     * Output control sequence contains only bounds between lazy segments. Lower bound of `startSegment` and
     * upper bound of last lazy segment are not included. Lazy values are moved to new sequence without any
     * changes.
     * <div></div>
     * 
     * If `startSegment` is non-lazy then returns uniform sequence with value of `startSegment`.
     */
    def buildControlSeqPatch(
      startSegment: ControlGenSegment[E, D, V]
    ): (Option[ControlGenSegment[E, D, V]], ControlSegmentSeq[E, D, V]) = {
      val builder = new TreapSegmentSeqBuilder.Mutable[E, D, ControlValue[E, D, V]](
        domainOps, ControlValueOps.get, rngManager
      )
      var run = true
      var segment = startSegment
      var nextSegment: ControlGenSegment[E, D, V] | Null = null
      while (run) {
        segment match {
          case s: ControlGenSegmentWithNext[E, D, V] => 
            val n = s.moveNext
            n.value match {
              case _: LazyValue[E, D, V] => 
                builder.addBound(s.upper, s.value)
                // Save next lazy segment as current and execute one more step.
                segment = n
              case _ => 
                builder.setLastValue(s.value)
                // Save next non-lazy segment and exit loop.
                nextSegment = n
                run = false
            }
          case _ => 
            builder.setLastValue(segment.value)
            // There is no next segment, exit loop.
            run = false
        }
      }
      (OptionUtil.optionOfNullable(nextSegment), builder.buildSeq)
    }

    /**
     * Builds patch sequence for group of adjacent lazy segments starting from `startSegment` and applies it to 
     * `zippedSeq` (see p.2.).
     * <div></div>
     * 
     * Returns tuple of:
     * <div>
     *   - first non-lazy segment that follows after group of lazy segments, or [[None]] if there is no such segment;
     * </div>
     * <div>
     *   - new zipped sequence with applied patch.
     * </div>
     * 
     * If `startSegment` is non-lazy then returns input `zippedSeq`.
     */ 
    def patchZippedSeq(
      startSegment: ControlGenSegment[E, D, V],
      zippedSeq: ZSegmentSeq[E, D, V]
    ): (Option[ControlGenSegment[E, D, V]], ZSegmentSeq[E, D, V]) = {
      if (startSegment.value.isStrict) (None, zippedSeq)
      else {
        val (endSegmentOpt, controlSeqPatch) = buildControlSeqPatch(startSegment)

        val patchLowerBound = startSegment.lower
        val patchUpperBound = endSegmentOpt.map(_.lower.flipLimited).getOrElse(ExtendedBound.AboveAll)

        val baseSeqPatch = makeUniformBaseSeq[E, D, V]

        val patchStartTruncation = controlSeqPatch.firstSegment
          .truncation(patchLowerBound)
          .zipIntoSwappedTuple[V, ControlSegmentBase[E, D, V], BaseSegmentBase[E, D, V]](baseSeqPatch)

        val tmpZippedSeq = prependZippedSeq(patchStartTruncation, zippedSeq)
        val tmpControlSeq = tmpZippedSeq.secondSeq

        val patchEndTruncation = tmpZippedSeq.secondSeq.lastSegment
          .truncation(patchUpperBound)
          .zipIntoSwappedTuple[V, ControlSegmentBase[E, D, V], BaseSegmentBase[E, D, V]](baseSeqPatch)

        val newZippedSeq = appendZippedSeq(patchEndTruncation, zippedSeq)

        (endSegmentOpt, newZippedSeq)
      }
    }

    @tailrec
    def traversePatchSeq(
      segment: ControlGenSegment[E, D, V],
      zippedSeq: ZSegmentSeq[E, D, V]
    ): ZSegmentSeq[E, D, V] = 
      segment.value match {
        case v: LazyValue[E, D, V] =>
          val (nextSegmentOpt, newZippedSeq) = patchZippedSeq(segment, zippedSeq)
          nextSegmentOpt match {
            case Some(nextSegment) => traversePatchSeq(nextSegment, newZippedSeq)
            case _ => newZippedSeq
          }
        case _ => segment match {
          case s: ControlGenSegmentWithNext[E, D, V] => traversePatchSeq(s.moveNext, zippedSeq)
          case _ => zippedSeq
        }
      }

    val patchControlSeq = NonMergingMappedValueOrderedMap.apply(
      supplierSeq,
      v => v match {
        case Some(f) => LazyValue.Default(f)(domainOps.domain)
        case _ => StrictValue.unstable[E, D, V]
      }
    )(
      domainOps,
      ControlValueOps.get,
      rngManager
    )

    traversePatchSeq(patchControlSeq.firstSegment, baseSeq.getZippedSeq)
  }

  /**
   * Creates uniform base sequence with value [[ValueOps.unit]].
   */ 
  private final def makeUniformBaseSeq[E, D[X] <: Domain[X], V](
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, V] =
    UniformOrderedMap.apply(
      valueOps.unit, 
      TreapOrderedMap.getFactory
    )(
      domainOps, 
      valueOps, 
      rngManager
    )

  /**
   * Creates uniform control sequence with specified `value`.
   */ 
  private final def makeUniformControlSeq[E, D[X] <: Domain[X], V](
    value: ControlValue[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): UniformSegmentSeq[E, D, ControlValue[E, D, V]] =
    UniformOrderedMap.apply(
      value, 
      TreapOrderedMap.getFactory
    )(
      domainOps, 
      ControlValueOps.get, 
      rngManager
    )

  /**
   * Creates zipped segment sequence with specified sequences.
   */ 
  private final def makeZippedSeq[E, D[X] <: Domain[X], V](
    baseSeq: BaseSegmentSeq[E, D, V],
    controlSeq: ControlSegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZSegmentSeq[E, D, V] =
    ZippedOrderedMap.apply(
      baseSeq,
      controlSeq,
      ZValue.operator(valueOps),
      ZValue.baseInvariant,
      ZValue.controlInvariant
    )(
      domainOps,
      ZValueOps.get(valueOps),
      rngManager
    )
}
