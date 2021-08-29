package ordset.core.value

import ordset.Hash
import ordset.core.SeqValidationPredicate

/**
 * Typeclasses for segment sequence values.
 * @tparam V value type.
 */
trait ValueOps[V] {

  /**
   * Validation function wrapping [[neqv]] method.
   */
  final lazy val distinctionValidation: SeqValidationPredicate[V] = neqv

  /**
   * @return some constant value of type `V`. Returned value doesn't have to get special meaning.
   */
  def unit: V

  /**
   * @return equality and hash typeclass.
   */
  def valueHash: Hash[V]

  /**
   * Returns set inclusion predicate. Value x is included in set if `valueIncl.apply(x) == true`.
   *
   * Example:
   *
   * Let's type of segment sequence value `V` is `Option[String]` and inclusion predicate returns:
   * {{{
   *   valueIncl.apply(Some("A")) == true
   *   valueIncl.apply(None) == false
   * }}}
   * Then segment sequence
   * {{{
   *
   *     Some("A")    None     Some("B")     - sequence values (type `V`)
   *   X----------](--------)[----------X
   * }}}
   * represents ordered map:
   * {{{
   *        A                     B          - map values
   *   X----------]          [----------X
   *                   ^
   *            excluded segment
   * }}}
   */
  def valueIncl: InclusionPredicate[V]

  /**
   * Equality check. It's always consistent with equality and hash typeclass [[valueHash]].
   */
  final def eqv(x: V, y: V): Boolean = valueHash.eqv(x, y)

  /**
   * Inequality check. It's always consistent with equality and hash typeclass [[valueHash]].
   */
  final def neqv(x: V, y: V): Boolean = valueHash.neqv(x, y)

  /**
   * Hash code calculation. It's always consistent with equality and hash typeclass [[valueHash]].
   */
  final def hash(x: V): Int = valueHash.hash(x)

  /**
   * Inclusion check. It's always consistent with set inclusion predicate [[valueIncl]].
   */
  final def isIncluded(x: V): Boolean = valueIncl.apply(x)
}

object ValueOps {

  implicit lazy val booleanValueOps: ValueOps[Boolean] =
    new DefaultImpl[Boolean](
      false,
      ordset.instances.boolean.booleanHash,
      InclusionPredicate.booleanInclusion
    )

  implicit lazy val intValueOps: ValueOps[Int] =
    new DefaultImpl[Int](
      0,
      ordset.instances.int.intHash,
      InclusionPredicate.alwaysIncluded
    )

  implicit lazy val longValueOps: ValueOps[Long] =
    new DefaultImpl[Long](
      0L,
      ordset.instances.long.longHash,
      InclusionPredicate.alwaysIncluded
    )

  implicit lazy val stringValueOps: ValueOps[String] =
    new DefaultImpl[String](
      "",
      ordset.instances.string.stringHash,
      InclusionPredicate.alwaysIncluded
    )

  final class DefaultImpl[V](
    override val unit: V,
    override val valueHash: Hash[V],
    override val valueIncl: InclusionPredicate[V]
  ) extends ValueOps[V]

  final class Tuple2Impl[V1, V2](
    override val valueIncl: InclusionPredicate[(V1, V2)],
    valueOps1: ValueOps[V1],
    valueOps2: ValueOps[V2],
  ) extends ValueOps[(V1, V2)] {

    override def unit: (V1, V2) = (valueOps1.unit, valueOps2.unit)

    override val valueHash: Hash[(V1, V2)] =
      ordset.instances.tuple2.tuple2Hash(valueOps1.valueHash, valueOps2.valueHash)
  }

  final class MapImpl[V1, V2](
    valueOps: ValueOps[V1],
    mapFunc1: V1 => V2,
    mapFunc2: V2 => V1
  ) extends ValueOps[V2] {

    override def unit: V2 = mapFunc1.apply(valueOps.unit)

    override def valueHash: Hash[V2] = Hash.by(mapFunc2)(valueOps.valueHash)

    override def valueIncl: InclusionPredicate[V2] = 
      new InclusionPredicate.MapImpl(valueOps.valueIncl, mapFunc2)
  }
}
