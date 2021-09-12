package ordset.core.set

import ordset.core.AbstractLazyTreapSegmentSeq.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.*
import ordset.core.map.OrderedMap
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap

class LazyTreapOrderedSet[E, D <: Domain[E]] protected (
  initZippedSeq: ZSegmentSeq[E, D, Boolean]
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractLazyTreapSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D, LazySetSegmentBase[E, D]] {

  // Initialization ----------------------------------------------------------- //
  zippedSeq = initZippedSeq

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): LazyOrderedSet[E, D] =
    new LazyTreapOrderedSet(
      makeZippedSeq(
        makeUniformBaseSeq(value),
        makeUniformControlSeq(EagerValue.stable)
      )
    )

  protected final override def consLazy(zippedSeq: ZSegmentSeq[E, D, Boolean]): LazyOrderedSet[E, D] = ???
}

object LazyTreapOrderedSet {

  /**
   * Builds lazy ordered set using two input maps:
   * <tr>1. `baseSet` - base ordered set;</tr>
   * <tr>2. `lazyMap` - ordered map with optional lazy values (functions that returns another ordered sets).</tr>
   * <tr></tr>
   * <tr>
   *   If segment of `lazyMap` has [[None]] value then corresponding segments of output set have the same
   *   values as `baseSet`.
   * </tr>
   * <tr>
   *   If segment of `lazyMap` has [[Some]] value with a function F: `() => orderedSеtF`, then corresponding
   *   segments of output set are lazy. Function F will be computed only if lazy segment is requested.
   *   Values of lazy segments are completely defined by `orderedSetF` and corresponding values of `baseSet`
   *   are ignored.
   * </tr>
   * {{{
   *
   *      true           false           true
   * X------------](-------------)[-------------X  `baseSet`
   *
   *   None     Some(() => orderedSetF)    None
   * X------](-------------------------)[-------X  `lazyMap`
   *
   *  true    false      true       false    true
   * X---](---------](---------)[----------](---X  `orderedSetF`
   *
   *   true    false     true     false    true
   * X------](------](---------)[------)[-------X  output set after lazy
   *                                               value computation
   * }}}
   *
   * @param baseSet    base ordered set.
   * @param lazyMap    ordered map with lazy values.
   * @param domainOps  domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def apply[E, D <: Domain[E], V](
    baseSet: OrderedSet[E, D],
    lazyMap: OrderedMap[E, D, OptionalSeqSupplier.Type[E, D, Boolean]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): LazyOrderedSet[E, D] = {
    val zippedSeq = AbstractLazyTreapSegmentSeq.ZippedSeqBuilder.build(baseSet, lazyMap)
    new LazyTreapOrderedSet(zippedSeq)
  }
}

