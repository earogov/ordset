package test.ordset.core.samples.segmentSeq

import ordset.{Hash, util}
import ordset.core.AbstractLazyTreapSegmentSeq.{ControlValue, ControlValueHash, ControlValueOps, EagerValue, LazySegmentBase, LazyValue, ZSegmentSeq, ZValue, ZValueOps}
import ordset.core.{AbstractLazyTreapSegmentSeq, ExtendedBound, IntervalRelation, LazySegmentSeq, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{OrderedMapCommons, TreapOrderedMap, UniformOrderedMap, ZippedOrderedMap}
import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.random.RngManager
import ordset.util.HashUtil.product1Hash
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample.TestZValueOps

abstract class LazyTreapSeqSample[E, D <: Domain[E], V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[E, D, V, LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V]] {

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
  lazy val someLazyZvalue: ZValue[E, D, V] = (valueOps.unit, LazyValue(someLazyValue))

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
    override val rngManager: RngManager
  ) extends LazyTreapSeqSample[E, D, V] {

    /**
     * Returns lazy sequence.
     *
     * Note, during tests state of sequence may be changed (lazy values are computed and cached).
     * Use [[restoreSequence]] to rollback to the initial state.
     */
    override def sequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] = lazySeq

    /**
     * Restores initial state of lazy sequence.
     *
     * Creates sequence with [[initializeSequence]] and saves it into [[lazySeq]] field.
     * Update is synchronized with [[lock]].
     */
    def restoreSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] = lock.synchronized {
      lazySeq = initializeSequence
      lazySeq
    }

    // Protected section -------------------------------------------------------- //
    @volatile
    protected var lazySeq: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] = initializeSequence

    protected final val lock = new Object

    /**
     * Creates initial lazy sequence.
     */
    protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V]
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
    override val rngManager: RngManager
  ) extends LazyTreapSeqSample[E, D, V] {

    /**
     * Returns lazy sequence.
     *
     * Note, each method call returns copy of sequence. So during tests state of original sequence never changes
     * (lazy and eager parts stay the same).
     */
    override def sequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] = LazyTreapSegmentSeq.clone(lazySeq)

    // Protected section -------------------------------------------------------- //
    protected lazy val lazySeq: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] =
      if (shuffled) LazyTreapSeqUtil.shuffleLazySeq(initializeSequence, extendedBounds)
      else initializeSequence

    /**
     * Creates initial lazy sequence.
     */
    protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V]
  }

  /**
   * Implementation of [[ordset.Hash]] for [[ControlValue]] for tests:
   * <tr>- all lazy control values are considered equals and have same hash code;</tr>
   * <tr>- all other control values are compared according to input `valueHash`.</tr>
   */
  final class TestControlValueHash[E, D <: Domain[E], V](
    val valueHash: Hash[ControlValue[E, D, V]]
  ) extends Hash[ControlValue[E, D, V]] {

    import util.HashUtil._
    import TestControlValueHash._

    override def hash(x: ControlValue[E, D, V]): Int = x match {
      case _: LazyValue[_, _, _] => lazyHash
      case _ => valueHash.hash(x)
    }

    override def eqv(x: ControlValue[E, D, V], y: ControlValue[E, D, V]): Boolean = (x, y) match {
      case (_: LazyValue[_, _, _], _: LazyValue[_, _, _]) => true
      case _ => valueHash.eqv(x, y)
    }
  }

  object TestControlValueHash {

    protected lazy val lazyHash: Int = TestControlValueHash.##
  }

  object TestControlValueOps {

    /**
     * Implementation of [[ValueOps]] for [[ControlValue]] for tests:
     * <tr>- all lazy control values are considered equals and have same hash code;</tr>
     * <tr>- all other control values are compared according to input `valueOps`.</tr>
     */
    def get[E, D <: Domain[E], V](valueOps: ValueOps[ControlValue[E, D, V]]): ValueOps[ControlValue[E, D, V]] =
      new ControlValueOps(
        valueOps.unit,
        TestControlValueHash(valueOps.valueHash),
        valueOps.valueIncl
      )
  }

  object TestZValueOps {

    /**
     * Implementation of [[ValueOps]] for [[ZValue]] for tests:
     * <tr>- all lazy control values are considered equals and have same hash code;</tr>
     * <tr>- all other control values are compared according to input `valueOps`.</tr>
     */
    def get[E, D <: Domain[E], V](valueOps: ValueOps[ZValue[E, D, V]]): ValueOps[ZValue[E, D, V]] = {
      val firstOps: ValueOps[V] =
        new ValueOps.MapImpl[ZValue[E, D, V], V](
          valueOps,
          _._1,
          (_, valueOps.unit._2)
        )
      val secondOps: ValueOps[ControlValue[E, D, V]] =
        new ValueOps.MapImpl[ZValue[E, D, V], ControlValue[E, D, V]](
          valueOps,
          _._2,
          (valueOps.unit._1, _)
        )
      new ValueOps.Tuple2Impl[V, ControlValue[E, D, V]](
        valueOps.valueIncl,
        firstOps,
        TestControlValueOps.get(secondOps)
      )
    }
  }

  /**
   * Implementation of [[AbstractLazyTreapSegmentSeq]] for tests. It provides:
   * <tr>- access to zipped sequence;</tr>
   * <tr>- simplified creation and clone operation.</tr>
   */
  class LazyTreapSegmentSeq[E, D <: Domain[E], V] protected (
    initZippedSeq: ZSegmentSeq[E, D, V]
  )(
    implicit
    final override val domainOps: DomainOps[E, D],
    final override val valueOps: ValueOps[V],
    final override val rngManager: RngManager
  ) extends AbstractLazyTreapSegmentSeq[E, D, V]
    with OrderedMapCommons[E, D, V, LazySegmentBase[E, D, V]] {

    // Initialization ----------------------------------------------------------- //
    zippedSeq = initZippedSeq

    // Inspection --------------------------------------------------------------- //
    def getZippedSeq: ZSegmentSeq[E, D, V] = zippedSeq

    // Protected section -------------------------------------------------------- //
    protected final override def consUniform(value: V): LazySegmentSeq[E, D, V] =
      new LazyTreapSegmentSeq(
        makeZippedSeq(
          makeUniformBaseSeq(value),
          makeUniformControlSeq(EagerValue.stable)
        )
      )

    protected final override def consLazy(zippedSeq: ZSegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] =
      new LazyTreapSegmentSeq(zippedSeq)
  }

  object LazyTreapSegmentSeq {

    /**
     * Creates sequence that consists only from lazy segments.
     */
    def totallyLazy[E, D <: Domain[E], V](
      initControlSeq: Iterable[(ExtendedBound.Upper[E], () => SegmentSeq[E, D, V])]
    )(
      implicit
      domainOps: DomainOps[E, D],
      valueOps: ValueOps[V],
      rngManager: RngManager
    ): LazyTreapSegmentSeq[E, D, V] = {
      val initZippedSeq: ZSegmentSeq[E, D, V] = ZippedOrderedMap.apply(
        TreapOrderedMap.getFactory.unsafeBuildAsc(
          List((ExtendedBound.AboveAll, valueOps.unit)),
          domainOps,
          valueOps
        )(),
        TreapOrderedMap.getFactory.unsafeBuildAsc(
          initControlSeq.map(p => (p._1, LazyValue(p._2))),
          domainOps,
          ControlValueOps.get
        )(),
        Tuple2.apply,
        _ => false,
        _ => false
      )(
        domainOps,
        ZValueOps.get(valueOps),
        rngManager
      )
      new LazyTreapSegmentSeq(initZippedSeq)
    }

    /**
     * Creates new lazy sequence with the same internal state as original. The point is that access to the clone
     * will keep internal state of original unmodified.
     */
    def clone[E, D <: Domain[E], V](
      original: LazyTreapSegmentSeq[E, D, V]
    ): LazyTreapSegmentSeq[E, D, V] =
      new LazyTreapSegmentSeq(original.getZippedSeq)(original.domainOps, original.valueOps, original.rngManager)
  }
}