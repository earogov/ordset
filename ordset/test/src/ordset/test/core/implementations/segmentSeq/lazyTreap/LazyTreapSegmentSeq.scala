package ordset.test.core.implementations.segmentSeq.lazyTreap

import ordset.{Hash, Show, util}
import ordset.core.ExtendedBound
import ordset.core.segmentSeq.*
import ordset.core.segmentSeq.internal.lazySeq.*
import ordset.core.segmentSeq.internal.lazySeq.ControlValue.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.map.{OrderedMap, OrderedMapCommons, OrderedMapFactory, TreapOrderedMap, ZippedOrderedMap}
import ordset.core.segmentSeq.map.{BoundValue, OrderedMapFactoryIterable}
import ordset.core.value.{ValueOps, InclusionPredicate}
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.segmentSeq.validation.ValidatingIterable
import ordset.random.RngManager
import ordset.util.BooleanUtil
import ordset.test.core.implementations.domain.BoundSelector
import java.util.concurrent.atomic.AtomicReference

/**
 * Implementation of [[AbstractLazyTreapSegmentSeq]] for tests. It provides:
 * <div>- access to zipped sequence;</div>
 * <div>- simplified creation and clone operation.</div>
 */
class LazyTreapSegmentSeq[E, D[X] <: Domain[X], V] protected (
  initZippedSeq: ZSegmentSeq[E, D, V]
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractLazyTreapSegmentSeq[E, D, V]
  with OrderedMapCommons[E, D, V, LazySegmentBase[E, D, V]] {

  // Protected section -------------------------------------------------------- //
  protected final override val zippedSeqRef: AtomicReference[ZSegmentSeq[E, D, V]] = 
    new AtomicReference(initZippedSeq)

  protected final override def consUniform(value: V): LazySegmentSeq[E, D, V] =
    new LazyTreapSegmentSeq(
      makeZippedSeq(
        makeUniformBaseSeq(value),
        makeUniformControlSeq(StrictValue.stable)
      )
    )

  protected final override def consLazy(zippedSeq: ZSegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] =
    new LazyTreapSegmentSeq(zippedSeq)
}

object LazyTreapSegmentSeq {

  /**
   * Creates sequence that consists of lazy segments only.
   */
  def totallyLazy[E, D[X] <: Domain[X], V](
    initSeq: Iterable[(ExtendedBound.Upper[E], () => SegmentSeq[E, D, V])]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): LazyTreapSegmentSeq[E, D, V] = {
    implicit val controlValueOps: ValueOps[ControlValue[E, D, V]] = ControlValueOps.get
    implicit val zvalueOps: ValueOps[ZValue[E, D, V]] = ZValueOps.get(valueOps)

    val initBoundValues: Iterable[BoundValue[E, ControlValue[E, D, V]]] = initSeq.map(p => 
      (p._1, LazyValue.Default(p._2)(domainOps.domain))
    )
    val initZippedSeq: ZSegmentSeq[E, D, V] = ZippedOrderedMap.apply(
      TreapOrderedMap.getFactory.buildUniform(valueOps.unit),
      TreapOrderedMap.getFactory.unsafeBuildAsc(initBoundValues),
      Tuple2.apply,
      BooleanUtil.falsePredicate1,
      BooleanUtil.falsePredicate1
    )
    new LazyTreapSegmentSeq(initZippedSeq)
  }

  /**
   * Creates new lazy sequence with the same internal state as original. The point is that access to the clone
   * will keep internal state of original unmodified.
   */
  def clone[E, D[X] <: Domain[X], V](
    original: LazyTreapSegmentSeq[E, D, V]
  ): LazyTreapSegmentSeq[E, D, V] =
    new LazyTreapSegmentSeq(original.getZippedSeq)(original.domainOps, original.valueOps, original.rngManager)

  /**
   * Returns factory that creates lazy treap ordered map with random internal state (random lazy and strict parts).
   */
  def getRandomMapFactory[E, D[X] <: Domain[X], V](
    implicit boundSelector: BoundSelector[E]
  ): OrderedMapFactory[E, D, V, LazyTreapSegmentSeq[E, D, V]] =
    new RandomMapFactory

  /**
   * Implementation of [[ordset.Hash]] for [[ControlValue]] for tests:
   * <div>- all lazy control values are considered equals and have same hash code;</div>
   * <div>- all other control values are compared according to input `valueHash`.</div>
   */
  final class TestControlValueHash[E, D[X] <: Domain[X], V](
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

  /**
   * Implementation of [[ValueOps]] for [[ControlValue]] for tests:
   * <div>- all lazy control values are considered equals and have same hash code;</div>
   * <div>- all other control values are compared according to input `valueOps`.</div>
   */
  final class TestControlValueOps[E, D[X] <: Domain[X], V](
    val valueOps: ValueOps[ControlValue[E, D, V]]
  ) extends ValueOps[ControlValue[E, D, V]] {

    override val unit: ControlValue[E, D, V] = valueOps.unit

    override val valueHash: Hash[ControlValue[E, D, V]] = TestControlValueHash(valueOps.valueHash)

    override val valueIncl: InclusionPredicate[ControlValue[E, D, V]] = valueOps.valueIncl

    override val valueShow: Show[ControlValue[E, D, V]] = ControlValueShow.get
  }

  object TestControlValueOps {

    def get[E, D[X] <: Domain[X], V](valueOps: ValueOps[ControlValue[E, D, V]]): ValueOps[ControlValue[E, D, V]] =
      new TestControlValueOps(valueOps)
  }

  object TestZValueOps {

    /**
     * Implementation of [[ValueOps]] for [[ZValue]] for tests:
     * <div>- all lazy control values are considered equals and have same hash code;</div>
     * <div>- all other control values are compared according to input `valueOps`.</div>
     */
    def get[E, D[X] <: Domain[X], V](valueOps: ValueOps[ZValue[E, D, V]]): ValueOps[ZValue[E, D, V]] = {
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
  private class RandomMapFactory[E, D[X] <: Domain[X], V](
    implicit boundSelector: BoundSelector[E]
  ) extends OrderedMapFactory[E, D, V, LazyTreapSegmentSeq[E, D, V]] {

    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuildAsc(
      seq: ValidatingIterable[BoundValue[E, V]]
    )(
      implicit 
      domainOps: DomainOps[E, D],
      valueOps: ValueOps[V],
      rngManager: RngManager
    ): LazyTreapSegmentSeq[E, D, V] = {
      val baseSeq = TreapOrderedMap.getFactory.unsafeBuildAsc(seq)
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
