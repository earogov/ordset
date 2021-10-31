package ordset.test.core.implementations.segmentSeq.lazyTreap

import ordset.{Hash, util}
import ordset.core.{ExtendedBound, SegmentSeq, SegmentSeqException, SeqValidationPredicate, TreapSegmentSeq}
import ordset.core.{LazySegmentSeq, LazySegmentBase, AbstractLazyTreapSegmentSeq}
import ordset.core.internal.lazySeq.*
import ordset.core.internal.lazySeq.ControlValue.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{OrderedMap, OrderedMapCommons, OrderedMapFactory, TreapOrderedMap, ZippedOrderedMap}
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.util.BooleanUtil
import ordset.test.core.implementations.domain.BoundSelector

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
   * Creates sequence that consists of lazy segments only.
   */
  def totallyLazy[E, D <: Domain[E], V](
    initSeq: Iterable[(ExtendedBound.Upper[E], () => SegmentSeq[E, D, V])]
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
        initSeq.map(p => (p._1, LazyValue(p._2))),
        domainOps,
        ControlValueOps.get
      )(),
      Tuple2.apply,
      BooleanUtil.falsePredicate1,
      BooleanUtil.falsePredicate1
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

  /**
   * Returns factory that creates lazy treap ordered map with random internal state (random lazy and eager parts).
   */
  def getRandomMapFactory[E, D <: Domain[E], V](
    implicit boundSelector: BoundSelector[E]
  ): OrderedMapFactory[E, D, V, LazyTreapSegmentSeq[E, D, V]] =
    new RandomMapFactory

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

  // Private section ---------------------------------------------------------- //
  private class RandomMapFactory[E, D <: Domain[E], V](
    implicit boundSelector: BoundSelector[E]
  ) extends OrderedMapFactory[E, D, V, LazyTreapSegmentSeq[E, D, V]] {

    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuildAsc(
      seq: IterableOnce[(ExtendedBound.Upper[E], V)],
      domainOps: DomainOps[E, D],
      valueOps: ValueOps[V]
    )(
      boundsValidation: SeqValidationPredicate[ExtendedBound.Upper[E]] = domainOps.extendedOrd.strictValidation,
      valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
    )(
      implicit rngManager: RngManager
    ): LazyTreapSegmentSeq[E, D, V] = {
      val baseSeq = TreapOrderedMap.getFactory.unsafeBuildAsc(
        seq, domainOps, valueOps
      )(
        boundsValidation, valuesValidation
      )(
        rngManager
      )
      val lazySeq = LazyTreapSeqUtil.makeRandomLazySeq(baseSeq)(boundSelector, rngManager)
      LazyTreapSeqUtil.shuffleLazySeq(lazySeq, baseSeq.extendedUpperBounds)(rngManager)
    }

    override def convertMap(map: OrderedMap[E, D, V]): LazyTreapSegmentSeq[E, D, V] =
      map match {
        case lazyMap: LazyTreapSegmentSeq[E, D, V] => lazyMap
        case _ => convertMapInternal(map)
      }
  }
}
