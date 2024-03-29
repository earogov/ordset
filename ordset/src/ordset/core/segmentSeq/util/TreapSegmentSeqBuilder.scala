package ordset.core.segmentSeq.util

import ordset.Order
import ordset.core.Bound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.segmentSeq.{SegmentSeq, TreapSegmentSeq}
import ordset.core.segmentSeq.map.TreapOrderedMap
import ordset.util.types.Undefined
import ordset.random.{RngManager, UnsafeUniformRng}
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

object TreapSegmentSeqBuilder {

  type TreapBuffer[E, V] = List[MutableTreap.Node[Bound.Upper[E], V]]

  /**
   * Mutable builder of [[TreapSegmentSeq]].
   *
   * Usage:
   * <div>
   *   1. Add tuples (segment upper bound, segment value) to internal buffer (see [[addBound]]).
   *      Bounds must follow in ascending order according to `domainOps.boundOrd`.
   * </div>
   * <div>
   *   2. Set value of last segment (see [[setLastValue]]).
   * </div>
   * <div>
   *   3. Build treap segment sequence (see [[buildSeq]]).
   * </div>
   * <div></div>
   *
   * Implementation is <u>tread unsafe</u>.
   */
  class Mutable[E, D[X] <: Domain[X], V](
    val domainOps: DomainOps[E, D],
    val valueOps: ValueOps[V],
    val rngManager: RngManager
  ) {

    /**
     * Add segment upper bound and value to internal buffer.
     *
     * Bounds must follow in ascending order according to `domainOps.boundOrd`.
     */
    def addBound(bound: Bound.Upper[E], value: V): TreapSegmentSeqBuilder.Mutable[E, D, V] = {
      buffer = BuildAsc.addToBuffer(buffer, bound, rng.nextInt(), value)(boundOrd)
      this
    }

    /**
     * Returns `true` if last added bound in buffer equals to specified `bound`.
     */
    def hasLastBound(bound: Bound.Upper[E]): Boolean = buffer match {
      case head :: _ => boundOrd.eqv(head.key, bound)
      case _ => false
    }

    /**
     * Specify value of last segment.
     */
    def setLastValue(value: V): TreapSegmentSeqBuilder.Mutable[E, D, V] = {
      lastValue = value
      this
    }

    /**
     * Returns `true` if value of last segment is defined.
     */
    def hasLastValue: Boolean = lastValue != null

    /**
     * Builds [[TreapSegmentSeq]].
     *
     * Value of last segment must be specified (see [[setLastValue]]).
     */
    @throws[AssertionError]("if last value of sequence is not defined")
    def buildSeq: TreapSegmentSeq[E, D, V] = {
      val lv = lastValue
      lv match {
        case Undefined => 
          throw new AssertionError("Last value of sequence is not defined")
        case lv: V @unchecked => 
          val root = BuildAsc.finalizeBuffer(buffer)
          TreapOrderedMap.unchecked[E, D, V](root, lv)(domainOps, valueOps, rngManager)
      }
    }

    // Private section ---------------------------------------------------------- //
    private val rng: UnsafeUniformRng = rngManager.newUnsafeUniformRng()

    private val boundOrd: Order[Bound[E]] = domainOps.boundOrd

    private var buffer: TreapBuffer[E, V] = List.empty

    private var lastValue: Undefined[V] = Undefined
  }
}
