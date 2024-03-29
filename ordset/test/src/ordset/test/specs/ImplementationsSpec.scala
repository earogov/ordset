package ordset.test.specs

import scala.collection.immutable.Queue

import org.scalatest.Assertions._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.{Eq, Discrete}
import ordset.util.IterableUtil

@RunWith(classOf[JUnitRunner])
class ImplementationsSpec extends AnyFunSpec {
  
  import BoundedOrderSpec._
  import DiscreteOrderSpec._
  import ordset.implementations._

  it("should implement order for `Unit`") {

    validateDiscreteFiniteOrder(
      new unit.NaturalOrder(),
      {}, 
      {},
      List((Discrete.None, {}), ({}, Discrete.None)),
      List(),
      List({}),
      List()
    )
  }

  it("should implement order for `Boolean`") {

    validateDiscreteFiniteOrder(
      new boolean.NaturalOrder(),
      false, 
      true,
      List((Discrete.None, false), (false, true), (true, Discrete.None)),
      List(),
      List(false, true),
      List()
    )
  }

  it("should implement order for `Byte`") {

    validateDiscreteFiniteOrder(
      new byte.NaturalOrder(),
      Byte.MinValue, 
      Byte.MaxValue,
      List(
        (Discrete.None, Byte.MinValue), 
        (Byte.MinValue, -127.toByte), 
        (0.toByte, 1.toByte), 
        (126.toByte, Byte.MaxValue), 
        (Byte.MaxValue, Discrete.None)
      ),
      List(),
      List(Byte.MinValue, 0.toByte, Byte.MaxValue),
      List()
    )

    validateDiscreteFiniteOrder(
      byte.tryNaturalOrderWithBounds(0.toByte, 10.toByte).get,
      0.toByte, 
      10.toByte,
      List(
        (Discrete.None, -1.toByte), 
        (Discrete.None, 0.toByte), 
        (0.toByte, 1.toByte), 
        (9.toByte, 10.toByte), 
        (10.toByte, Discrete.None), 
        (11.toByte, Discrete.None)
      ),
      List(-3.toByte, -2.toByte, -1.toByte),
      List(0.toByte, 1.toByte, 9.toByte, 10.toByte),
      List(11.toByte, 12.toByte, 13.toByte)
    )
  }

  it("should implement order for `Short`") {

    validateDiscreteFiniteOrder(
      new short.NaturalOrder(),
      Short.MinValue, 
      Short.MaxValue,
      List(
        (Discrete.None, Short.MinValue), 
        (Short.MinValue, -32767.toShort), 
        (0.toShort, 1.toShort), 
        (32766.toShort, Short.MaxValue), 
        (Short.MaxValue, Discrete.None)
      ),
      List(),
      List(Short.MinValue, 0.toShort, Short.MaxValue),
      List()
    )

    validateDiscreteFiniteOrder(
      short.tryNaturalOrderWithBounds(0.toShort, 10.toShort).get,
      0.toShort, 
      10.toShort,
      List(
        (Discrete.None, -1.toShort), 
        (Discrete.None, 0.toShort), 
        (0.toShort, 1.toShort), 
        (9.toShort, 10.toShort), 
        (10.toShort, Discrete.None), 
        (11.toShort, Discrete.None)
      ),
      List(-3.toShort, -2.toShort, -1.toShort),
      List(0.toShort, 1.toShort, 9.toShort, 10.toShort),
      List(11.toShort, 12.toShort, 13.toShort)
    )
  }

  it("should implement order for `Int`") {

    validateDiscreteFiniteOrder(
      new int.NaturalOrder(),
      Int.MinValue, 
      Int.MaxValue,
      List(
        (Discrete.None, Int.MinValue), 
        (Int.MinValue, Int.MinValue + 1), 
        (0, 1), 
        (Int.MaxValue - 1, Int.MaxValue), 
        (Int.MaxValue, Discrete.None)
      ),
      List(),
      List(Int.MinValue, 0, Int.MaxValue),
      List()
    )

    validateDiscreteFiniteOrder(
      int.tryNaturalOrderWithBounds(0, 10).get,
      0, 
      10,
      List((Discrete.None, -1), (Discrete.None, 0), (0, 1), (9, 10), (10, Discrete.None), (11, Discrete.None)),
      List(-3, -2, -1),
      List(0, 1, 9, 10),
      List(11, 12, 13)
    )
  }

  it("should implement order for `Long`") {

    validateDiscreteFiniteOrder(
      new long.NaturalOrder(),
      Long.MinValue, 
      Long.MaxValue,
      List(
        (Discrete.None, Long.MinValue), 
        (Long.MinValue, Long.MinValue + 1), 
        (0L, 1L), 
        (Long.MaxValue - 1, Long.MaxValue), 
        (Long.MaxValue, Discrete.None)
      ),
      List(),
      List(Long.MinValue, 0L, Long.MaxValue),
      List()
    )

    validateDiscreteFiniteOrder(
      long.tryNaturalOrderWithBounds(0L, 10L).get,
      0L, 
      10L,
      List((Discrete.None, -1L), (Discrete.None, 0L), (0L, 1L), (9L, 10L), (10L, Discrete.None), (11L, Discrete.None)),
      List(-3L, -2L, -1L),
      List(0L, 1L, 9L, 10L),
      List(11L, 12L, 13L)
    )
  }

  it("should implement order for `BigInt`") {

    validateDiscreteOrder(
      new bigInt.NaturalOrder(),
      List(
        (BigInt(Int.MinValue) - 1, BigInt(Int.MinValue)), 
        (BigInt(Int.MinValue), BigInt(Int.MinValue) + 1), 
        (BigInt(0), BigInt(1)), 
        (BigInt(Int.MaxValue) - 1, BigInt(Int.MaxValue)), 
        (BigInt(Int.MaxValue), BigInt(Int.MaxValue) + 1)
      )
    )

    validateDiscreteFiniteOrder(
      bigInt.tryNaturalOrderWithBounds(BigInt(0), BigInt(10)).get,
      BigInt(0), 
      BigInt(10),
      List(
        (Discrete.None, BigInt(-1)), 
        (Discrete.None, BigInt(0)), 
        (BigInt(0), BigInt(1)), 
        (BigInt(9), BigInt(10)), 
        (BigInt(10), Discrete.None), 
        (BigInt(11), Discrete.None)
      ),
      List(BigInt(-3), BigInt(-2), BigInt(-1)),
      List(BigInt(0), BigInt(1), BigInt(9), BigInt(10)),
      List(BigInt(11), BigInt(12), BigInt(13))
    )
  }

  it("should implement order for `Char`") {

    validateDiscreteFiniteOrder(
      new char.NaturalOrder(),
      Char.MinValue, 
      Char.MaxValue,
      List(
        (Discrete.None, Char.MinValue), 
        (Char.MinValue, (Char.MinValue + 1).toChar), 
        (10.toChar, 11.toChar), 
        ((Char.MaxValue - 1).toChar, Char.MaxValue), 
        (Char.MaxValue, Discrete.None)
      ),
      List(),
      List(Char.MinValue, 10.toChar, Char.MaxValue),
      List()
    )

    validateDiscreteFiniteOrder(
      char.tryNaturalOrderWithBounds(10.toChar, 20.toChar).get,
      10.toChar, 
      20.toChar,
      List(
        (Discrete.None, 8.toChar), 
        (Discrete.None, 9.toChar), 
        (10.toChar, 11.toChar), 
        (19.toChar, 20.toChar), 
        (20.toChar, Discrete.None), 
        (21.toChar, Discrete.None)
      ),
      List(7.toChar, 8.toChar, 9.toChar),
      List(10.toChar, 11.toChar, 19.toChar, 20.toChar),
      List(21.toChar, 22.toChar, 23.toChar)
    )
  }

  it("should implement order for `String`") {

    validateBoundedBelowOrder(
      new string.NaturalOrder(),
      "",
      true,
      List(),
      List("", "a", "aa")
    )

    validateBoundedOrder(
      string.tryNaturalOrderWithBounds("aaa", false, "ab", true).get,
      "aaa",
      false, 
      "ab",
      true,
      List("a", "aa", "aaa"),
      List("aaaa", "aab", "aac"),
      List("ac", "aba", "b")
    )
  }

  it("should implement order for `Float`") {

    validateBoundedOrder(
      new float.NaturalOrder(),
      Float.MinValue,
      true,
      Float.MaxValue,
      true,
      List(),
      List(Float.MinValue, 0f, Float.MaxValue),
      List()
    )

    validateBoundedOrder(
      float.tryNaturalOrderWithBounds(0f, false, 10f, true).get,
      0f,
      false,
      10f,
      true,
      List(-2f, -1f, 0f),
      List(1f, 9f, 10f),
      List(11f, 12f, 13f)
    )
  }

  it("should implement order for `Double`") {

    validateBoundedOrder(
      new double.NaturalOrder(),
      Double.MinValue,
      true,
      Double.MaxValue,
      true,
      List(),
      List(Double.MinValue, 0d, Double.MaxValue),
      List()
    )

    validateBoundedOrder(
      double.tryNaturalOrderWithBounds(0d, false, 10d, true).get,
      0d,
      false,
      10d,
      true,
      List(-2d, -1d, 0d),
      List(1d, 9d, 10d),
      List(11d, 12d, 13d)
    )
  }

  it("should implement order for `BigDecimal`") {

    validateBoundedOrder(
      bigDecimal.tryNaturalOrderWithBounds(BigDecimal(0d), false, BigDecimal(10d), true).get,
      BigDecimal(0d),
      false,
      BigDecimal(10d),
      true,
      List(BigDecimal(-2d), BigDecimal(-1d), BigDecimal(0d)),
      List(BigDecimal(1d), BigDecimal(9d), BigDecimal(10d)),
      List(BigDecimal(11d), BigDecimal(12d), BigDecimal(13d))
    )
  }

  it("should implement order for `Iterable`") {

    import ordset.givens.int._

    validateBoundedBelowOrder[Iterable[Int]](
      new iterable.NaturalOrder(),
      Nil,
      true,
      List(),
      List(Nil, List(1), List(2), List(3))
    )(
      new IterableEq()
    )

    validateBoundedOrder(
      iterable.tryNaturalOrderWithBounds(List(1, 1, 1), false, List(1, 2), true).get,
      List(1, 1, 1),
      false, 
      List(1, 2),
      true,
      List(List(1), List(1, 1), List(1, 1, 1)),
      List(List(1, 1, 1, 1), List(1, 1, 2), List(1, 1, 3)),
      List(List(1, 3), List(1, 2, 1), List(2))
    )(
      new IterableEq()
    )
  }

  it("should implement order for `List`") {

    import ordset.givens.int._

    validateBoundedBelowOrder[List[Int]](
      new list.NaturalOrder(),
      Nil,
      true,
      List(),
      List(Nil, List(1), List(2), List(3))
    )(
      new ListEq()
    )

    validateBoundedOrder(
      list.tryNaturalOrderWithBounds(List(1, 1, 1), false, List(1, 2), true).get,
      List(1, 1, 1),
      false, 
      List(1, 2),
      true,
      List(List(1), List(1, 1), List(1, 1, 1)),
      List(List(1, 1, 1, 1), List(1, 1, 2), List(1, 1, 3)),
      List(List(1, 3), List(1, 2, 1), List(2))
    )(
      new ListEq()
    )
  }

  it("should implement order for `LazyList`") {

    import ordset.givens.int._

    validateBoundedBelowOrder[LazyList[Int]](
      new lazyList.NaturalOrder(),
      LazyList(),
      true,
      List(),
      List(LazyList(), LazyList(1), LazyList(2), LazyList(3))
    )(
      new LazyListEq()
    )

    validateBoundedOrder(
      lazyList.tryNaturalOrderWithBounds(LazyList(1, 1, 1), false, LazyList(1, 2), true).get,
      LazyList(1, 1, 1),
      false, 
      LazyList(1, 2),
      true,
      List(LazyList(1), LazyList(1, 1), LazyList(1, 1, 1)),
      List(LazyList(1, 1, 1, 1), LazyList(1, 1, 2), LazyList(1, 1, 3)),
      List(LazyList(1, 3), LazyList(1, 2, 1), LazyList(2))
    )(
      new LazyListEq()
    )
  }

  it("should implement order for `Queue`") {

    import ordset.givens.int._

    validateBoundedBelowOrder[Queue[Int]](
      new queue.NaturalOrder(),
      Queue(),
      true,
      List(),
      List(Queue(), Queue(1), Queue(2), Queue(3))
    )(
      new QueueEq()
    )

    validateBoundedOrder(
      queue.tryNaturalOrderWithBounds(Queue(1, 1, 1), false, Queue(1, 2), true).get,
      Queue(1, 1, 1),
      false, 
      Queue(1, 2),
      true,
      List(Queue(1), Queue(1, 1), Queue(1, 1, 1)),
      List(Queue(1, 1, 1, 1), Queue(1, 1, 2), Queue(1, 1, 3)),
      List(Queue(1, 3), Queue(1, 2, 1), Queue(2))
    )(
      new QueueEq()
    )
  }

  private class IterableEq[T](implicit ev: Eq[T]) extends Eq[Iterable[T]] {

    override def eqv(xs: Iterable[T], ys: Iterable[T]) = IterableUtil.iterableEq(xs, ys)(ev)
  }

  private class ListEq[T](implicit ev: Eq[T]) extends Eq[List[T]] {

    override def eqv(xs: List[T], ys: List[T]) = IterableUtil.iterableEq(xs, ys)(ev)
  }

  private class LazyListEq[T](implicit ev: Eq[T]) extends Eq[LazyList[T]] {

    override def eqv(xs: LazyList[T], ys: LazyList[T]) = IterableUtil.iterableEq(xs, ys)(ev)
  }

  private class QueueEq[T](implicit ev: Eq[T]) extends Eq[Queue[T]] {

    override def eqv(xs: Queue[T], ys: Queue[T]) = IterableUtil.iterableEq(xs, ys)(ev)
  }
}
