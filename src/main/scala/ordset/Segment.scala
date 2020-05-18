package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait Segment[@sp(spNum) E, @sp(Boolean) +V] {
  import Segment._

  def value: V

  def moveTo(bound: Bound[E]): Segment[E, V]

  def lazyList: LazyList[Segment[E, V]] = this match {
    case n: WithNext[E, V] => LazyList.cons(this, n.moveNext.lazyList)
    case _                 => LazyList.empty
  }

  def intervalMapping: IntervalMapping[E, V] = this match {
    case i: Inner[E, V]    => IntervalMapping.bounded(i.movePrev.upperBound.flipUpper, i.upperBound, value)
    case p: WithPrev[E, V] => IntervalMapping.rightUnbounded(p.movePrev.upperBound.flipUpper, value)
    case n: WithNext[E, V] => IntervalMapping.leftUnbounded(n.upperBound, value)
    case _                 => IntervalMapping.unbounded(value)
  }

  def intervalMappingLazyList: LazyList[IntervalMapping[E, V]] = this match {
    case n: WithNext[E, V] => LazyList.cons(n.intervalMapping, n.moveNext.intervalMappingLazyList)
    case _                 => LazyList(intervalMapping)
  }
}

object Segment {

  def first[E, V](
      value: V,
      upperBound: Bound.Upper[E],
      mvTo: Bound[E] => Segment[E, V],
      mvNext: () => WithPrev[E, V]
  ): WithNext[E, V] = WithNextImpl(value, upperBound, mvTo, mvNext)

  def last[E, V](
      value: V,
      mvTo: Bound[E] => Segment[E, V],
      mvPrev: () => WithNext[E, V]
  ): WithPrev[E, V] = WithPrevImpl(value, mvTo, mvPrev)

  def inner[E, V](
      value: V,
      upperBound: Bound.Upper[E],
      mvTo: Bound[E] => Segment[E, V],
      mvPrev: () => WithNext[E, V],
      mvNext: () => WithPrev[E, V]
  ): Inner[E, V] = InnerImpl(value, upperBound, mvTo, mvPrev, mvNext)

  def single[E, V](value: V): Single[E, V] = SingleImpl(value)

  implicit def defaultHash[E, V](implicit hash: Hash[IntervalMapping[E, V]]): Hash[Segment[E, V]] =
    new DefaultHash()(hash)

  trait WithNext[E, +V] extends Segment[E, V] with HasNext[E, V] {

    override def lazyList: LazyList[Segment[E, V]] = LazyList.cons(this, moveNext.lazyList)

    override def intervalMapping: IntervalMapping[E, V] = this match {
      case p: WithPrev[E, V] => IntervalMapping.bounded(p.movePrev.upperBound.flipUpper, upperBound, value)
      case _                 => IntervalMapping.leftUnbounded(upperBound, value)
    }

    override def intervalMappingLazyList: LazyList[IntervalMapping[E, V]] =
      LazyList.cons(intervalMapping, moveNext.intervalMappingLazyList)
  }

  trait WithPrev[E, +V] extends Segment[E, V] with HasPrev[E, V] {

    override def intervalMapping: IntervalMapping[E, V] = this match {
      case n: WithNext[E, V] => IntervalMapping.bounded(movePrev.upperBound.flipUpper, n.upperBound, value)
      case _                 => IntervalMapping.rightUnbounded(movePrev.upperBound.flipUpper, value)
    }
  }

  trait Inner[E, +V] extends WithNext[E, V] with WithPrev[E, V] {

    override def lazyList: LazyList[Segment[E, V]] = LazyList.cons(this, moveNext.lazyList)

    override def intervalMapping: IntervalMapping[E, V] =
      IntervalMapping.bounded(movePrev.upperBound.flipUpper, upperBound, value)

    override def intervalMappingLazyList: LazyList[IntervalMapping[E, V]] =
      LazyList.cons(intervalMapping, moveNext.intervalMappingLazyList)
  }

  trait Single[E, +V] extends Segment[E, V]

  trait HasNext[E, +V] {

    def upperBound: Bound.Upper[E]

    def moveNext: WithPrev[E, V]
  }

  trait HasPrev[E, +V] {

    def movePrev: WithNext[E, V]
  }

  class DefaultHash[E, V](implicit hashIM: Hash[IntervalMapping[E, V]]) extends Hash[Segment[E, V]] {
    import util.Hash._
    // TODO: `hash` and `eqv` should use `upperBound` and `Value` to optimize performance.
    override def hash(x: Segment[E, V]): Int = product1Hash(hashIM.hash(x.intervalMapping))
    override def eqv(x: Segment[E, V], y: Segment[E, V]): Boolean = hashIM.eqv(x.intervalMapping, y.intervalMapping)
  }

  private case class WithNextImpl[E, +V] (
      override val value: V,
      override val upperBound: Bound.Upper[E],
      mvTo: Bound[E] => Segment[E, V],
      mvNext: () => WithPrev[E, V]
  ) extends WithNext[E, V] {

    override def moveTo(bound: Bound[E]): Segment[E, V] = mvTo(bound)

    override def moveNext: WithPrev[E, V] = mvNext()
  }

  private case class WithPrevImpl[E, +V] (
      override val value: V,
      mvTo: Bound[E] => Segment[E, V],
      mvPrev: () => WithNext[E, V]
  ) extends WithPrev[E, V] {

    override def moveTo(bound: Bound[E]): Segment[E, V] = mvTo(bound)

    override def movePrev: WithNext[E, V] = mvPrev()
  }

  private case class InnerImpl[E, +V] (
      override val value: V,
      override val upperBound: Bound.Upper[E],
      mvTo: Bound[E] => Segment[E, V],
      mvPrev: () => WithNext[E, V],
      mvNext: () => WithPrev[E, V]
  ) extends Inner[E, V] {

    override def moveTo(bound: Bound[E]): Segment[E, V] = mvTo(bound)

    override def movePrev: WithNext[E, V] = mvPrev()

    override def moveNext: WithPrev[E, V] = mvNext()
  }

  private case class SingleImpl[E, +V] (
      override val value: V
  ) extends Single[E, V] {

    override def moveTo(bound: Bound[E]): Segment[E, V] = this
  }
}
