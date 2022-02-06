package ordset.core.segmentSeq.internal.lazySeq

import ordset.core.segmentSeq.internal.lazySeq.ControlValue.ControlValueOps
import ordset.core.domain.Domain
import ordset.core.value.{ValueOps, InclusionPredicate}
import ordset.util.BooleanUtil

protected[ordset] object ZValue {

  /**
   * Returns operator that combines base value and control value into tuple (value of zipped sequence).
   *
   * If control value is lazy, operator replaces real base value in output tuple with `valueOps.unit`.
   *
   * Consider the case:
   * {{{
   *
   *        A        B        C         D
   *   X-------)[-------)[--------)[--------X - base sequence
   *
   *       u              ?             u
   *   X-------)[-----------------)[--------X - control sequence
   *                function `f`
   *
   *     (A, u)   (B, ?)    (C, ?)   (D, u)
   *   X-------)[-------)[--------)[--------X - zipped sequence v1
   *
   *     (A, u)       (unit, ?)      (D, u)
   *   X-------)[-----------------)[--------X - zipped sequence v2
   * }}}
   *
   * Zipped sequence v1 was received without additional replacement - operator just combines two values into tuple.
   * In that case we get undesirable behavior of lazy sequence: function `f` will be called twice to compute all
   * lazy segments (one time per each zipped segment).
   *
   * Zipped sequence v2 was received with current `operator` and has no such disadvantage.
   */
  def operator[E, D[X] <: Domain[X], V](
    valueOps: ValueOps[V]
  ): (V, ControlValue[E, D, V]) => ZValue[E, D, V] =
    (v, c) => if (c.isLazy) (valueOps.unit, c) else (v, c)

  /**
   * Returns function that check whether base value is invariant for [[operator]]
   * (see [[SegmentSeqT.zipOptimized]]).
   */
  def baseInvariant: Any => Boolean = BooleanUtil.falsePredicate1

  /**
   * Returns function that check whether control value is invariant for [[operator]]
   * (see [[SegmentSeqT.zipOptimized]]).
   */
  def controlInvariant[E, D[X] <: Domain[X], V]: ControlValue[E, D, V] => Boolean =
    controlInvariantInstance.asInstanceOf

  // Private section ---------------------------------------------------------- //
  private lazy val controlInvariantInstance: ControlValue[Any, Domain, Any] => Boolean = _.isLazy
}

object ZValueOps {

  def get[E, D[X] <: Domain[X], V](valueOps: ValueOps[V]): ValueOps[ZValue[E, D, V]] =
    new ValueOps.Tuple2Impl[V, ControlValue[E, D, V]](
      InclusionPredicate.alwaysIncluded,
      valueOps,
      ControlValueOps.get
    )
}