package test.ordset.core.samples.segmentSeq

import ordset.{Hash, util}
import ordset.core.AbstractLazyTreapSegmentSeq.{ControlValue, ControlValueHash, ControlValueOps, EagerValue, LazySegmentBase, LazyValue, ZSegmentSeq, ZValue, ZValueOps}
import ordset.core.{AbstractLazyTreapSegmentSeq, ExtendedBound, LazySegmentSeq, SegmentSeq}
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
  lazy val testZvalueOps: ValueOps[ZValue[E, D, V]] = TestZValueOps.get(lazySeq.getZippedSeq.valueOps)

  /**
   * Implementation of lazy value for test.
   *
   * Should be used for equality checks only ([[testZvalueOps]] considers all lazy values equal).
   * When called throws [[UnsupportedOperationException]].
   */
  lazy val someLazyValue: () => SegmentSeq[E, D, V] =
    () => throw new UnsupportedOperationException("Implementa")

  /**
   * Implementation of lazy [[ZValue]] for test.
   *
   * Should be used for equality checks only ([[testZvalueOps]] considers all lazy values equal).
   * When called throws [[UnsupportedOperationException]].
   */
  lazy val someLazyZvalue: ZValue[E, D, V] = (valueOps.unit, LazyValue(someLazyValue))

  /**
   * Returns lazy sequence.
   *
   * Note, during tests state of sequence may be changed: lazy values are computed and cached.
   * One can use [[restoreSequence]] to rollback to initial state.
   */
  override def sequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] = lazySeq

  /**
   * Restores initial state of lazy sequence.
   *
   * Creates sequence with [[initializeSequence]] and saves into [[lazySeq]] field.
   * Update is synchronized with [[lock]].
   */
  def restoreSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] = lock.synchronized {
    lazySeq = initializeSequence
    lazySeq
  }

  // Protected section -------------------------------------------------------- //
  /**
   * Stores current lazy sequence.
   */
  @volatile
  protected var lazySeq: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] = initializeSequence

  protected final val lock = new Object

  /**
   * Creates initial lazy sequence.
   */
  protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V]
}

object LazyTreapSeqSample {

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

  class LazyTreapSegmentSeq[E, D <: Domain[E], V](
    initControlSeq: Iterable[(ExtendedBound.Upper[E], () => SegmentSeq[E, D, V])]
  )(
    implicit
    final override val domainOps: DomainOps[E, D],
    final override val valueOps: ValueOps[V],
    final override val rngManager: RngManager
  ) extends AbstractLazyTreapSegmentSeq[E, D, V]
    with OrderedMapCommons[E, D, V, LazySegmentBase[E, D, V]] {

    // Initialization ----------------------------------------------------------- //
    zippedSeq = ZippedOrderedMap.apply(
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

    // Inspection --------------------------------------------------------------- //
    def getZippedSeq: ZSegmentSeq[E, D, V] = zippedSeq

    // Protected section -------------------------------------------------------- //
    protected final override def consUniform(value: V): UniformOrderedMap[E, D, V] = UniformOrderedMap.default(value)
  }
}