package ordset.test.specs

import scala.collection.immutable.Queue

import org.scalatest.Assertions._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

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
      List((null, {}), ({}, null)),
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
      List((null, false), (false, true), (true, null)),
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
        (null, Byte.MinValue), 
        (Byte.MinValue, -127.toByte), 
        (0.toByte, 1.toByte), 
        (126.toByte, Byte.MaxValue), 
        (Byte.MaxValue, null)
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
        (null, -1.toByte), 
        (null, 0.toByte), 
        (0.toByte, 1.toByte), 
        (9.toByte, 10.toByte), 
        (10.toByte, null), 
        (11.toByte, null)
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
        (null, Short.MinValue), 
        (Short.MinValue, -32767.toShort), 
        (0.toShort, 1.toShort), 
        (32766.toShort, Short.MaxValue), 
        (Short.MaxValue, null)
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
        (null, -1.toShort), 
        (null, 0.toShort), 
        (0.toShort, 1.toShort), 
        (9.toShort, 10.toShort), 
        (10.toShort, null), 
        (11.toShort, null)
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
        (null, Int.MinValue), 
        (Int.MinValue, Int.MinValue + 1), 
        (0, 1), 
        (Int.MaxValue - 1, Int.MaxValue), 
        (Int.MaxValue, null)
      ),
      List(),
      List(Int.MinValue, 0, Int.MaxValue),
      List()
    )

    validateDiscreteFiniteOrder(
      int.tryNaturalOrderWithBounds(0, 10).get,
      0, 
      10,
      List((null, -1), (null, 0), (0, 1), (9, 10), (10, null), (11, null)),
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
        (null, Long.MinValue), 
        (Long.MinValue, Long.MinValue + 1), 
        (0L, 1L), 
        (Long.MaxValue - 1, Long.MaxValue), 
        (Long.MaxValue, null)
      ),
      List(),
      List(Long.MinValue, 0L, Long.MaxValue),
      List()
    )

    validateDiscreteFiniteOrder(
      long.tryNaturalOrderWithBounds(0L, 10L).get,
      0L, 
      10L,
      List((null, -1L), (null, 0L), (0L, 1L), (9L, 10L), (10L, null), (11L, null)),
      List(-3L, -2L, -1L),
      List(0L, 1L, 9L, 10L),
      List(11L, 12L, 13L)
    )
  }

  it("should implement order for `Char`") {

    validateDiscreteFiniteOrder(
      new char.NaturalOrder(),
      Char.MinValue, 
      Char.MaxValue,
      List(
        (null, Char.MinValue), 
        (Char.MinValue, (Char.MinValue + 1).toChar), 
        (10.toChar, 11.toChar), 
        ((Char.MaxValue - 1).toChar, Char.MaxValue), 
        (Char.MaxValue, null)
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
        (null, 8.toChar), 
        (null, 9.toChar), 
        (10.toChar, 11.toChar), 
        (19.toChar, 20.toChar), 
        (20.toChar, null), 
        (21.toChar, null)
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

  it("should implement order for `List`") {

    import ordset.givens.int._

    validateBoundedBelowOrder[List[Int]](
      new list.NaturalOrder(),
      Nil,
      true,
      List(),
      List(Nil, List(1), List(2), List(3))
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
    )
  }
}
