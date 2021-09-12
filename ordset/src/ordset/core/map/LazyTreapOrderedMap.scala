package ordset.core.map

import ordset.core.AbstractLazyTreapSegmentSeq.{BaseSegmentSeq, ControlValue, EagerValue, LazySegmentBase, ZSegmentSeq}
import ordset.core.{AbstractLazyTreapSegmentSeq, Bound, LazySegmentSeq, OptionalSeqSupplier, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap

class LazyTreapOrderedMap[E, D <: Domain[E], V] protected (
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
  @inline
  protected final override def consUniform(value: V): LazyOrderedMap[E, D, V] =
    new LazyTreapOrderedMap(
      makeZippedSeq(
        makeUniformBaseSeq(value),
        makeUniformControlSeq(EagerValue.stable)
      )
    )

  protected final override def consLazy(zippedSeq: ZSegmentSeq[E, D, V]): LazyOrderedMap[E, D, V] = ???
}

object LazyTreapOrderedMap {

  /**
   * Builds lazy ordered map using two input maps:
   * <tr>1. `baseMap` - ordered map with base values of type `V`;</tr>
   * <tr>2. `lazyMap` - ordered map with optional lazy values (functions that returns another ordered maps).</tr>
   * <tr></tr>
   * <tr>
   *   If segment of `lazyMap` has [[None]] value then corresponding segments of output map have the same
   *   values as `baseMap`.
   * </tr>
   * <tr>
   *   If segment of `lazyMap` has [[Some]] value with a function F: `() => orderedMapF`, then corresponding
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
   * X------](-------------------------)[-------X  `lazyMap`
   *
   *   D        E         F           G       H
   * X---](---------](---------)[----------](---X  `orderedMapF`
   *
   *     A       E         F        G        C
   * X------](------](---------)[------)[-------X  output map after lazy
   *                                               value computation
   * }}}
   *
   * @param baseMap    ordered map with base values.
   * @param lazyMap    ordered map with lazy values.
   * @param domainOps  domain specific typeclasses: elements ordering, etc.
   * @param valueOps   value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
   */
  def apply[E, D <: Domain[E], V](
    baseMap: OrderedMap[E, D, V],
    lazyMap: OrderedMap[E, D, OptionalSeqSupplier.Type[E, D, V]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): LazySegmentSeq[E, D, V] = {
    val zippedSeq = AbstractLazyTreapSegmentSeq.ZippedSeqBuilder.build(baseMap, lazyMap)
    new LazyTreapOrderedMap(zippedSeq)
  }
}
