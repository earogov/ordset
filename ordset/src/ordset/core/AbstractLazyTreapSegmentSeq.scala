package ordset.core

import ordset.{Hash, util}
import ordset.core.AbstractZippedSegmentSeq.ZippedSegment
import ordset.core.domain.Domain
import ordset.core.map.{TreapOrderedMap, UniformOrderedMap, ZippedOrderedMap}
import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

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
 * ? - undefined segment: lazy value is not evaluated, evaluation will produce new segment sequence instead
 *     of given segment.
 *
 * u - unstable segment: value had been evaluated, but segment type is undefined (initial, inner, terminal or
 *     single) due some of adjacent segments are undefined.
 *
 * s - stable segment: value had been evaluated and segment type is defined, all adjacent segments are stable.
 *
 */
abstract class AbstractLazyTreapSegmentSeq[E, D <: Domain[E], V]
  extends AbstractSegmentSeq[E, D, V, AbstractTreapSegmentSeq.TreapSegmentBase[E, D, V]] {
  seq =>

  import AbstractLazyTreapSegmentSeq._
  import AbstractTreapSegmentSeq._
  import AbstractZippedSegmentSeq._

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = ???

  final override def isUniversal: Boolean = ???

  final override def isUniform: Boolean = ???

  final override def contains(bound: Bound[E]): Boolean = getSegment(bound).isIncluded

  final override def containsElement(element: E): Boolean = super.containsElement(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds

  final override lazy val firstSegment: TreapInitialSegment[E, D, V] = ???

  final override lazy val lastSegment: TreapTerminalSegment[E, D, V] = ???

  final override def getSegment(bound: Bound[E]): TreapSegment[E, D, V] = ???
//  {
//    var zsegment = zippedSeq.getSegment(bound)
//    if (zsegment.value._2.isStable) {
//      zsegment.firstSeqSegment
//    } else {
//      eval(zsegment)
//      zsegment = zippedSeq.getSegment(bound)
//      zsegment.firstSeqSegment
////      if (zsegment.value._2.isStable) {
////        zsegment.value._1
////      } else {
////        zsegment match {
////        case zs: Segment.Inner[E, D, V] =>
////        case zs: Segment.
////      }
//    }
//  }

  final override def getSegmentForElement(element: E): TreapSegment[E, D, V] = super.getSegmentForElement(element)

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): TreapSegmentSeq[E, D, V] = sliced(bound)._2

  final override def takenBelow(bound: Bound[E]): TreapSegmentSeq[E, D, V] = sliced(bound)._1

  final override def sliced(bound: Bound[E]): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) = ???

  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = ???

  // Protected section -------------------------------------------------------- //
  @volatile
  protected var baseSeq: TreapSegmentSeq[E, D, V]

  @volatile
  protected var controlSeq: TreapSegmentSeq[E, D, ControlValue[E, D, V]]

  @volatile
  protected var zippedSeq: ZippedSegmentSeq[E, D, V, ControlValue[E, D, V], (V, ControlValue[E, D, V])]

  protected val controlTupleOps: ValueOps[(V, ControlValue[E, D, V])] = ControlTupleOps.get(valueOps)

  protected final def eval(zsegment: ZSegment[E, D, V]): Unit = ???
//    zsegment.value._2 match {
//    case lazyValue: LazyValue[E, D, V] =>
//      val seq = lazyValue.eval
//      controlSeq.synchronized {
//        val newBaseSeq = zsegment.firstSeqSegment.patched(seq)
//        val newControlSeq = zsegment.secondSeqSegment.patched(makeControlSeq(zsegment, seq))
//        val newZippedSeq = ZippedOrderedMap.apply[E, D, V, ControlValue[E, D, V], (V, ControlValue[E, D, V])](
//          newBaseSeq, newControlSeq, Tuple2.apply, _ => false, _ => false
//        )(
//          domainOps, controlTupleOps, rngManager
//        )
//        baseSeq = newBaseSeq
//        controlSeq = newControlSeq
//        zippedSeq = newZippedSeq
//      }
//    case _ => // nothing to do
//  }

  /**
   * Builds control sequence for a given sequence `seq` that was evaluated for `zsegment`.
   * Segments of `seq` that are completely inside input `zsegment` are mapped to stable control values,
   * others - to unstable (see class description).
   *
   * {{{
   *   segment:
   *
   *                (----------------------]
   *
   *   seq:
   *
   *        A         B           C          D         E       - values
   *   X--------](---------](----------](--------](---------X
   *
   *   output:
   *
   *               u              s                u           - control values
   *   X-------------------](----------](-------------------X
   * }}}
   * where u - unstable segment; s - stable segment.
   *
   * @param zsegment zipped segment (contains base segment and control segment).
   * @param seq sequence that was evaluated for `zsegment` to patch corresponding base segment.
   */
  protected final def makeControlSeq(
    zsegment: ZSegment[E, D, V],
    seq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, ControlValue[E, D, V]] = {
    val boundSegments = SegmentSeqOps.getBoundSegments(zsegment, seq)

    //          (---]            - segment
    //  X--)[----------)[----X   - seq
    //  X--------------------X   - output
    //         unstable
    if (domainOps.segmentUpperOrd.eqv(boundSegments._1, boundSegments._2)) {
      UniformOrderedMap.apply(
        EagerValue.unstable
      )(
        domainOps, ControlValueOps.get, rngManager
      )
    } else boundSegments match {
      case (lowerSegment: Segment.WithNext[E, D, V], upperSegment: Segment.WithPrev[E, D, V]) =>
        val boundOrd = domainOps.boundOrd
        val rng = rngManager.newUnsafeUniformRng()

        var buffer =
          BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], ControlValue[E, D, V]](
            List.empty[MutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]]],
            lowerSegment.upperBound,
            rng.nextInt(),
            EagerValue.unstable
          )(
            boundOrd
          )

        val nextSegment = lowerSegment.moveNext

        //   (----------------]      - segment
        //  X--)[----------)[----X   - seq
        //  X--)[----------)[----X   - output
        //    |     stable     |
        //  unstable       unstable
        if (!domainOps.segmentUpperOrd.eqv(nextSegment, upperSegment)) {
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
        // else:
        //            (-------]      - segment
        //  X--)[----------)[----X   - seq
        //  X--------------)[----X   - output
        //      unstable   unstable

        val root = BuildAsc.finalizeBuffer(buffer)
        root match {
          case root: ImmutableTreap.Node[Bound.Upper[E], ControlValue[E, D, V]] =>
            TreapOrderedMap.unchecked(
              root, EagerValue.unstable
            )(
              domainOps, ControlValueOps.get, rngManager
            )
          case _ =>
            throw new AssertionError(s"Expected nonempty tree $root for control sequence.")
        }
      case _ =>
        // boundSegments._1 != boundSegments._2 =>
        // boundSegments._2 follows after boundSegments._1 =>
        // boundSegments._1 has next segment and boundSegments._2 has previous segment.
        throw new AssertionError(
          s"Expected segment ${boundSegments._1} has next segment and " +
            s"segment ${boundSegments._2} has upper segment."
        )
    }
  }
}

object AbstractLazyTreapSegmentSeq {

  protected type ZSegment[E, D <: Domain[E], V] =
    ZippedSegment[E, D, V, ControlValue[E, D, V], (V, ControlValue[E, D, V])]

  protected sealed trait ControlValue[E, D <: Domain[E], V] {

    def isStable: Boolean

    def isUnstable: Boolean
  }
  
  protected final case class LazyValue[E, D <: Domain[E], V](
    private val seqFunc: () => SegmentSeq[E, D, V]
  ) extends ControlValue[E, D, V] {

    override def isStable: Boolean = false

    override def isUnstable: Boolean = true

    def eval: SegmentSeq[E, D, V] = seqFunc.apply()
  }
  
  protected final case class EagerValue[E, D <: Domain[E], V] private (
    private val stable: Boolean
  ) extends ControlValue[E, D, V] {
    
    override def isStable: Boolean = stable
    
    override def isUnstable: Boolean = !stable
  }

  protected object EagerValue {

    def stable[E, D <: Domain[E], V]: EagerValue[E, D, V] = stableInstance.asInstanceOf

    def unstable[E, D <: Domain[E], V]: EagerValue[E, D, V] = unstableInstance.asInstanceOf

    private lazy val stableInstance: EagerValue[Any, Domain[Any], Any] = new EagerValue(true)

    private lazy val unstableInstance: EagerValue[Any, Domain[Any], Any] = new EagerValue(true)
  }

  protected final class ControlValueHash[E, D <: Domain[E], V]
    extends Hash[ControlValue[E, D, V]] {

    import util.HashUtil._

    override def hash(x: ControlValue[E, D, V]): Int = x match {
      case x: LazyValue[E, D, V] => System.identityHashCode(x)
      case x: EagerValue[E, D, V] => product1Hash(x.isStable.##)
    }

    override def eqv(x: ControlValue[E, D, V], y: ControlValue[E, D, V]): Boolean =
      (x, y) match {
        case (x: LazyValue[E, D, V], y: LazyValue[E, D, V]) => x.eq(y)
        case (x: EagerValue[E, D, V], y: EagerValue[E, D, V]) => x.isStable == y.isStable
        case _ => false
      }
  }

  object ControlValueHash {

    def get[E, D <: Domain[E], V]: Hash[ControlValue[E, D, V]] = instance.asInstanceOf

    private lazy val instance: ControlValueHash[Any, Domain[Any], Any] = new ControlValueHash
  }

  protected final class ControlValueOps[E, D <: Domain[E], V](
    override val unit: ControlValue[E, D, V] = EagerValue.stable[E, D, V],
    override val valueHash: Hash[ControlValue[E, D, V]] = ControlValueHash.get[E, D, V],
    override val valueIncl: InclusionPredicate[ControlValue[E, D, V]] = InclusionPredicate.alwaysIncluded
  ) extends ValueOps[ControlValue[E, D, V]]

  protected object ControlValueOps {

    def get[E, D <: Domain[E], V]: ValueOps[ControlValue[E, D, V]] = instance.asInstanceOf

    private lazy val instance: ControlValueOps[Any, Domain[Any], Any] = new ControlValueOps()
  }

  protected object ControlTupleOps {

    def get[E, D <: Domain[E], V](valueOps: ValueOps[V]): ValueOps[(V, ControlValue[E, D, V])] =
      new ValueOps.Tuple2Impl[V, ControlValue[E, D, V]](
        InclusionPredicate.alwaysIncluded,
        valueOps,
        ControlValueOps.get
      )
  }
}