package ordset.core.value

import ordset.{Hash, Show}

/**
 * Typeclasses for segment sequence values.
 * 
 * @tparam V value type.
 */
trait ValueOps[V] {

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
   * @return show typeclass.
   */
  def valueShow: Show[V]

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

  /**
   * String representation of value. It's always consistent with show typeclass [[valueShow]].
   */
  final def show(x: V): String = valueShow.show(x)
}

object ValueOps {

  implicit def booleanValueOps(
    implicit valueShow: Show[Boolean] = ordset.givens.boolean.booleanShow
  ): ValueOps[Boolean] =
    new DefaultImpl[Boolean](
      false,
      ordset.givens.boolean.booleanNaturalOrder,
      InclusionPredicate.booleanInclusion,
      valueShow
    )

  implicit def intValueOps(
    implicit valueShow: Show[Int] = ordset.givens.int.intShow
  ): ValueOps[Int] =
    new DefaultImpl[Int](
      0,
      ordset.givens.int.intNaturalOrder,
      InclusionPredicate.alwaysIncluded,
      valueShow
    )

  implicit def longValueOps(
    implicit valueShow: Show[Long] = ordset.givens.long.longShow
  ): ValueOps[Long] =
    new DefaultImpl[Long](
      0L,
      ordset.givens.long.longNaturalOrder,
      InclusionPredicate.alwaysIncluded,
      valueShow
    )

  implicit def bigIntValueOps(
    implicit valueShow: Show[BigInt] = ordset.givens.bigInt.bigIntShow
  ): ValueOps[BigInt] =
    new DefaultImpl[BigInt](
      BigInt(0),
      ordset.givens.bigInt.bigIntNaturalOrder,
      InclusionPredicate.alwaysIncluded,
      valueShow
    )

  implicit def floatValueOps(
    implicit valueShow: Show[Float] = ordset.givens.float.floatShow
  ): ValueOps[Float] =
    new DefaultImpl[Float](
      0f,
      ordset.givens.float.floatNaturalOrder,
      InclusionPredicate.alwaysIncluded,
      valueShow
    )

  implicit def doubleValueOps(
    implicit valueShow: Show[Double] = ordset.givens.double.doubleShow
  ): ValueOps[Double] =
    new DefaultImpl[Double](
      0d,
      ordset.givens.double.doubleNaturalOrder,
      InclusionPredicate.alwaysIncluded,
      valueShow
    )

  implicit def bigDecimalValueOps(
    implicit valueShow: Show[BigDecimal] = ordset.givens.bigDecimal.bigDecimalShow
  ): ValueOps[BigDecimal] =
    new DefaultImpl[BigDecimal](
      BigDecimal(0),
      ordset.givens.bigDecimal.bigDecimalNaturalOrder,
      InclusionPredicate.alwaysIncluded,
      valueShow
    )

  implicit def stringValueOps(
    implicit valueShow: Show[String] = ordset.givens.string.stringShow
  ): ValueOps[String] =
    new DefaultImpl[String](
      "",
      ordset.givens.string.stringNaturalOrder,
      InclusionPredicate.alwaysIncluded,
      valueShow
    )

  final class DefaultImpl[V](
    override val unit: V,
    override val valueHash: Hash[V],
    override val valueIncl: InclusionPredicate[V],
    override val valueShow: Show[V]
  ) extends ValueOps[V]

  final class Tuple2Impl[V1, V2](
    override val valueIncl: InclusionPredicate[(V1, V2)],
    valueOps1: ValueOps[V1],
    valueOps2: ValueOps[V2],
  ) extends ValueOps[(V1, V2)] {

    override val unit: (V1, V2) = (valueOps1.unit, valueOps2.unit)

    override val valueHash: Hash[(V1, V2)] =
      ordset.givens.tuple2.tuple2Hash(valueOps1.valueHash, valueOps2.valueHash)

    override val valueShow: Show[(V1, V2)] =
      ordset.givens.tuple2.tuple2Show(valueOps1.valueShow, valueOps2.valueShow)
  }
  
  final class EitherImpl[V1, V2](
    override val valueIncl: InclusionPredicate[Either[V1, V2]],
    valueOps1: ValueOps[V1],
    valueOps2: ValueOps[V2],
    leftUnit: Boolean
  ) extends ValueOps[Either[V1, V2]] {
    
    override val unit: Either[V1, V2] = 
      if (leftUnit) Left(valueOps1.unit) 
      else Right(valueOps2.unit)
      
    override val valueHash: Hash[Either[V1, V2]] =
      ordset.givens.either.eitherHash(valueOps1.valueHash, valueOps2.valueHash)

    override val valueShow: Show[Either[V1, V2]] =
      ordset.givens.either.eitherShow(valueOps1.valueShow, valueOps2.valueShow)
  }

  final class MapImpl[V1, V2](
    valueOps: ValueOps[V1],
    mapFunc1: V1 => V2,
    mapFunc2: V2 => V1
  ) extends ValueOps[V2] {

    override val unit: V2 = mapFunc1(valueOps.unit)

    override val valueHash: Hash[V2] = Hash.by(mapFunc2)(valueOps.valueHash)

    override val valueIncl: InclusionPredicate[V2] = 
      new InclusionPredicate.MapImpl(valueOps.valueIncl, mapFunc2)

    override val valueShow: Show[V2] = Show.show(v => valueOps.show(mapFunc2(v)))
  }
}
