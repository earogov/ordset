package ordset.core.map

import ordset.core.{AbstractLazyTreapSegmentSeq, LazySegmentBase, Bound, SegmentSeq}
import ordset.core.internal.lazySeq.*
import ordset.core.internal.lazySeq.ControlValue.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.core.internal.lazySeq.ZSegmentSeqBuilder
import java.util.concurrent.atomic.AtomicReference

class LazyTreapOrderedMap[E, D <: Domain[E], V] protected (
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

  @inline
  protected final override def consUniform(value: V): LazyOrderedMap[E, D, V] =
    new LazyTreapOrderedMap(
      makeZippedSeq(
        makeUniformBaseSeq(value),
        makeUniformControlSeq(EagerValue.stable)
      )
    )

  protected final override def consLazy(zippedSeq: ZSegmentSeq[E, D, V]): LazyOrderedMap[E, D, V] =
    new LazyTreapOrderedMap(zippedSeq)
}

object LazyTreapOrderedMap {

  /**
   * Builds lazy ordered map using two input maps:
   * <tr>1. `baseMap` - ordered map with base values of type `V`;</tr>
   * <tr>2. `supplierMap` - ordered map with optional lazy values (functions that returns another ordered maps).</tr>
   * <tr></tr>
   * <tr>
   *   If segment of `supplierMap` has [[None]] value then corresponding segments of output map have the same
   *   values as `baseMap`.
   * </tr>
   * <tr>
   *   If segment of `supplierMap` has [[Some]] value with a function F: `() => orderedMapF`, then corresponding
   *   segments of output map are lazy. Function F will be computed only if lazy segment is requested.
   *   Values of lazy segments are completely defined by `orderedMapF` and corresponding values of `baseMap`
   *   are ignored.
   * </tr>
   * {{{
   *
   *         A             B              C
   * X------------](-------------)[-------------X  `baseMap`
   *
   *   None     Some(() => orderedMapF)    None
   * X------](-------------------------)[-------X  `supplierMap`
   *
   *   D        E         F           G       H
   * X---](---------](---------)[----------](---X  `orderedMapF`
   *
   *     A       E         F        G        C
   * X------](------](---------)[------)[-------X  output map after lazy
   *                                               value computation
   * }}}
   *
   * @param baseMap     ordered map with base values.
   * @param supplierMap ordered map with lazy values.
   * @param domainOps   domain specific typeclasses: elements ordering, etc.
   * @param valueOps    value specific typeclasses: equality, set inclusion function, etc. 
   * @param rngManager  generator of random sequences.
   */
  def apply[E, D <: Domain[E], V](
    baseMap: OrderedMap[E, D, V],
    supplierMap: SupplierOrderedMap[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): LazyOrderedMap[E, D, V] = {
    val zippedSeq = ZSegmentSeqBuilder.build(baseMap, supplierMap)
    new LazyTreapOrderedMap(zippedSeq)
  }

  /**
   * Builds lazy ordered map (see [[apply]]) using uniform map with value `ValueOps.unit` as a base sequence.
   * 
   * Method is intended to build completely lazy sequences when each segment of `supplierMap` has [[Some]] value.
   */ 
  def completelyLazy[E, D <: Domain[E], V](
    supplierMap: SupplierOrderedMap[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): LazyOrderedMap[E, D, V] =
    apply(UniformOrderedMap.defaultUnit, supplierMap)
}
