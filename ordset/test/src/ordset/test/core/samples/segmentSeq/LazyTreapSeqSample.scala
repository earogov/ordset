package ordset.test.core.samples.segmentSeq

import ordset.{Hash, util}
import ordset.core.internal.lazySeq.*
import ordset.core.internal.lazySeq.ControlValue.*
import ordset.core.{AbstractLazyTreapSegmentSeq, ExtendedBound, LazySegmentSeq, LazySegmentBase, SegmentSeq}
import ordset.core.interval.IntervalRelation
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{OrderedMapCommons, TreapOrderedMap, UniformOrderedMap, ZippedOrderedMap}
import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.random.RngManager
import ordset.util.HashUtil.product1Hash
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.implementations.segmentSeq.lazyTreap.{LazyTreapSegmentSeq, LazyTreapSeqUtil}
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq.TestZValueOps

abstract class LazyTreapSeqSample[E, D <: Domain[E], V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[E]
) extends SegmentSeqSample[E, D, V, LazyTreapSegmentSeq[E, D, V]] {

  /**
   * Implementation of [[ValueOps]] for [[ZValue]] for tests:
   * <tr>- all lazy control values are considered equals and have same hash code;</tr>
   * <tr>- all other control values are compared according to `valueOps` of sequence.</tr>
   */
  lazy val testZvalueOps: ValueOps[ZValue[E, D, V]] = TestZValueOps.get(sequence.getZippedSeq.valueOps)

  /**
   * Implementation of lazy value for test.
   *
   * Should be used only for equality checks of lazy values ([[testZvalueOps]] considers all lazy values equal).
   * When called throws [[UnsupportedOperationException]].
   */
  lazy val someLazyValue: () => SegmentSeq[E, D, V] = () =>
    throw new UnsupportedOperationException(
      "Function should be never called, it's intended to use only for equality checks of lazy values."
    )

  /**
   * Implementation of lazy [[ZValue]] for test.
   *
   * Should be used for equality checks only ([[testZvalueOps]] considers all lazy values equal).
   * When called throws [[UnsupportedOperationException]].
   */
  lazy val someLazyZvalue: ZValue[E, D, V] = (valueOps.unit, new LazyValue.Unbounded(someLazyValue))

  /**
   * Reference list of interval relations that equivalent to [[lazySeq.getZippedSeq]] after all lazy value are computed.
   */
  lazy val zippedReference: Seq[IntervalRelation[E, D, ZValue[E, D, V]]] =
    reference.map(_.mapValue((_, EagerValue.stable)))
}

object LazyTreapSeqSample {

  /**
   * Sample with lazy sequence which internal state is modified during access (lazy values are computed and cached).
   * [[restoreSequence]] can be used to rollback to the initial state.
   */
  abstract class Restorable[E, D <: Domain[E], V](
    implicit
    override val domainOps: DomainOps[E, D],
    override val rngManager: RngManager,
    override val boundSelector: BoundSelector[E]
  ) extends LazyTreapSeqSample[E, D, V] {

    /**
     * Returns lazy sequence.
     *
     * Note, during tests state of sequence may be changed (lazy values are computed and cached).
     * Use [[restoreSequence]] to rollback to the initial state.
     */
    override def sequence: LazyTreapSegmentSeq[E, D, V] = lazySeq

    /**
     * Restores initial state of lazy sequence.
     *
     * Creates sequence with [[initializeSequence]] and saves it into [[lazySeq]] field.
     * Update is synchronized with [[lock]].
     */
    def restoreSequence: LazyTreapSegmentSeq[E, D, V] = lock.synchronized {
      lazySeq = initializeSequence
      lazySeq
    }

    // Protected section -------------------------------------------------------- //
    @volatile
    protected var lazySeq: LazyTreapSegmentSeq[E, D, V] = initializeSequence

    protected final val lock = new Object

    /**
     * Creates initial lazy sequence.
     */
    protected def initializeSequence: LazyTreapSegmentSeq[E, D, V]
  }

  /**
   * Sample with lazy sequence which internal state is kept fixed during access. Each time [[sequence]] is called
   * it returns sequence with exactly the same lazy and eager parts.
   *
   * If `shuffled` == `false` then sequence is returned in totally lazy state. Otherwise state is randomized
   * to provide some parts to be eager.
   */
  abstract class Fixed[E, D <: Domain[E], V](
    shuffled: Boolean
  )(
    implicit
    override val domainOps: DomainOps[E, D],
    override val rngManager: RngManager,
    override val boundSelector: BoundSelector[E]
  ) extends LazyTreapSeqSample[E, D, V] {

    /**
     * Returns lazy sequence.
     *
     * Note, each method call returns copy of sequence. So during tests state of original sequence never changes
     * (lazy and eager parts stay the same).
     */
    override def sequence: LazyTreapSegmentSeq[E, D, V] = LazyTreapSegmentSeq.clone(lazySeq)

    // Protected section -------------------------------------------------------- //
    protected lazy val lazySeq: LazyTreapSegmentSeq[E, D, V] =
      if (shuffled) LazyTreapSeqUtil.shuffleLazySeq(initializeSequence, extendedBounds)
      else initializeSequence

    /**
     * Creates initial lazy sequence.
     */
    protected def initializeSequence: LazyTreapSegmentSeq[E, D, V]
  }
}