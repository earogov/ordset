package ordset.core.segmentSeq.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.segmentSeq.AbstractLazyTreapSegmentSeq
import ordset.core.segmentSeq.internal.lazySeq.*
import ordset.core.segmentSeq.internal.lazySeq.ControlValue.*
import ordset.core.segmentSeq.map.OrderedMap
import ordset.random.RngManager
import java.util.concurrent.atomic.AtomicReference

class LazyTreapOrderedSet[E, D[X] <: Domain[X]] protected (
  initZippedSeq: ZSegmentSeq[E, D, Boolean]
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractLazyTreapSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D, LazySetSegmentBase[E, D]] {

  // Set transformation ------------------------------------------------------- //
  override def inverse(implicit ev: Boolean =:= Boolean): OrderedSet[E, D] = defaultInverse

  // Protected section -------------------------------------------------------- //
  protected final override val zippedSeqRef: AtomicReference[ZSegmentSeq[E, D, Boolean]] = 
    new AtomicReference(initZippedSeq)

  @inline
  protected final override def consUniform(value: Boolean): LazyOrderedSet[E, D] =
    new LazyTreapOrderedSet(
      makeZippedSeq(
        makeUniformBaseSeq(value),
        makeUniformControlSeq(StrictValue.stable)
      )
    )

  protected final override def consLazy(zippedSeq: ZSegmentSeq[E, D, Boolean]): LazyOrderedSet[E, D] =
    new LazyTreapOrderedSet(zippedSeq)
}

object LazyTreapOrderedSet {

  /**
   * Builds lazy ordered set using two input maps:
   * <div>1. `baseSet` - base ordered set;</div>
   * <div>2. `supplierMap` - ordered map with optional lazy values (functions that returns another ordered sets).</div>
   * <div></div>
   * <div>
   *   If segment of `supplierMap` has [[None]] value then corresponding segments of output set have the same
   *   values as `baseSet`.
   * </div>
   * <div>
   *   If segment of `supplierMap` has [[Some]] value with a function F: `() => orderedSеtF`, then corresponding
   *   segments of output set are lazy. Function F will be computed only if lazy segment is requested.
   *   Values of lazy segments are completely defined by `orderedSetF` and corresponding values of `baseSet`
   *   are ignored.
   * </div>
   * {{{
   *
   *      true           false           true
   * X------------](-------------)[-------------X  `baseSet`
   *
   *   None     Some(() => orderedSetF)    None
   * X------](-------------------------)[-------X  `supplierMap`
   *
   *  true    false      true       false    true
   * X---](---------](---------)[----------](---X  `orderedSetF`
   *
   *   true    false     true     false    true
   * X------](------](---------)[------)[-------X  output set after lazy
   *                                               value computation
   * }}}
   *
   * @param baseSet     base ordered set.
   * @param supplierMap ordered map with lazy values.
   * @param domainOps   domain specific typeclasses: elements ordering, etc.
   * @param rngManager  generator of random sequences.
   */
  def apply[E, D[X] <: Domain[X], V](
    baseSet: OrderedSet[E, D],
    supplierMap: SetSupplierOrderedMap[E, D]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): LazyOrderedSet[E, D] = {
    val zippedSeq = ZSegmentSeqBuilder.build(baseSet, supplierMap)
    new LazyTreapOrderedSet(zippedSeq)
  }

  /**
   * Builds lazy ordered set (see [[apply]]) using uniform set with value `ValueOps.unit` (for `Boolean` type) as a 
   * base sequence.
   * 
   * Method is intended to build completely lazy sequences when each segment of `supplierMap` has [[Some]] value.
   */ 
  def completelyLazy[E, D[X] <: Domain[X]](
    supplierMap: SetSupplierOrderedMap[E, D]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): LazyOrderedSet[E, D] =
    apply(UniformOrderedSet.defaultUnit, supplierMap)
}

