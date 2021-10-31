package ordset.test.array

import ordset.core.instances
import org.scalatest.funspec.AnyFunSpec

import scala.collection.immutable.ArraySeq

trait SortedArraySearchBehaviors {
  this: AnyFunSpec =>

  import ordset.Order
  import ordset.array.SortedArraySearch._

  def searchClosestNotGreater(searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int): Unit = {

    val ord: Order[Int] = instances.int.intAscOrder

    it("should throw OutOfBounds exception for illegal indexes") {
      val emp = ArraySeq.empty[Int]
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, emp, ord, 0, 0))

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, 0, 6))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, -1, 2))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, -2, -1))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, 7, 8))
    }

    it("should throw IllegalArgument exception if start index is greater then end") {
      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assertThrows[IllegalArgumentException](searchFun(6, arr, ord, 4, 3))
    }

    it("should return SortedArraySearch.NotFound if array contains no such element") {
      val single = ArraySeq[Int](7)
      assert(searchFun(6, single, ord, 0, 0) == NotFound)

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assert(searchFun(0, arr, ord, 0, 5) == NotFound)
      assert(searchFun(6, arr, ord, 3, 3) == NotFound)
      assert(searchFun(6, arr, ord, 3, 5) == NotFound)
    }

    it("should return index of element otherwise") {
      val single = ArraySeq[Int](5)
      assert(single(searchFun(6, single, ord, 0, 0)) == 5)

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assert(arr(searchFun(2, arr, ord, 0, 5)) == 1)
      assert(arr(searchFun(6, arr, ord, 0, 1)) == 3)
      assert(arr(searchFun(6, arr, ord, 1, 1)) == 3)
      assert(arr(searchFun(6, arr, ord, 1, 2)) == 5)
      assert(arr(searchFun(6, arr, ord, 1, 3)) == 5)
      assert(arr(searchFun(6, arr, ord, 1, 4)) == 5)
      assert(arr(searchFun(6, arr, ord, 1, 5)) == 5)
      assert(arr(searchFun(6, arr, ord, 0, 5)) == 5)
      assert(arr(searchFun(12, arr, ord, 0, 5)) == 11)
      assert(arr(searchFun(12, arr, ord, 4, 5)) == 11)
      assert(arr(searchFun(12, arr, ord, 5, 5)) == 11)

      val arr2 = ArraySeq[Int](0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
      val end2 = arr2.length - 1
      assert(arr2(searchFun(0, arr2, ord, 0, end2)) == 0)
      assert(arr2(searchFun(5, arr2, ord, 0, end2)) == 0)
      assert(arr2(searchFun(10, arr2, ord, 0, end2)) == 10)
      assert(arr2(searchFun(15, arr2, ord, 0, end2)) == 10)
      assert(arr2(searchFun(20, arr2, ord, 0, end2)) == 20)
      assert(arr2(searchFun(25, arr2, ord, 0, end2)) == 20)
      assert(arr2(searchFun(30, arr2, ord, 0, end2)) == 30)
      assert(arr2(searchFun(35, arr2, ord, 0, end2)) == 30)
      assert(arr2(searchFun(40, arr2, ord, 0, end2)) == 40)
      assert(arr2(searchFun(45, arr2, ord, 0, end2)) == 40)
      assert(arr2(searchFun(50, arr2, ord, 0, end2)) == 50)
      assert(arr2(searchFun(55, arr2, ord, 0, end2)) == 50)
      assert(arr2(searchFun(60, arr2, ord, 0, end2)) == 60)
      assert(arr2(searchFun(65, arr2, ord, 0, end2)) == 60)
      assert(arr2(searchFun(70, arr2, ord, 0, end2)) == 70)
      assert(arr2(searchFun(75, arr2, ord, 0, end2)) == 70)
      assert(arr2(searchFun(80, arr2, ord, 0, end2)) == 80)
      assert(arr2(searchFun(85, arr2, ord, 0, end2)) == 80)
      assert(arr2(searchFun(90, arr2, ord, 0, end2)) == 90)
      assert(arr2(searchFun(95, arr2, ord, 0, end2)) == 90)
      assert(arr2(searchFun(100, arr2, ord, 0, end2)) == 100)
      assert(arr2(searchFun(105, arr2, ord, 0, end2)) == 100)
    }
  }

  def searchClosestLess(searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int): Unit = {

    val ord: Order[Int] = instances.int.intAscOrder

    it("should throw OutOfBounds exception for illegal indexes") {
      val emp = ArraySeq.empty[Int]
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, emp, ord, 0, 0))

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, 0, 6))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, -1, 2))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, -2, -1))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, 7, 8))
    }

    it("should throw IllegalArgument exception if start index is greater then end") {
      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assertThrows[IllegalArgumentException](searchFun(6, arr, ord, 4, 3))
    }

    it("should return SortedArraySearch.NotFound if array contains no such element") {
      val single = ArraySeq[Int](7)
      assert(searchFun(6, single, ord, 0, 0) == NotFound)
      assert(searchFun(7, single, ord, 0, 0) == NotFound)

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assert(searchFun(0, arr, ord, 0, 5) == NotFound)
      assert(searchFun(1, arr, ord, 0, 5) == NotFound)
      assert(searchFun(6, arr, ord, 3, 3) == NotFound)
      assert(searchFun(7, arr, ord, 3, 3) == NotFound)
      assert(searchFun(6, arr, ord, 3, 5) == NotFound)
      assert(searchFun(7, arr, ord, 3, 5) == NotFound)
    }

    it("should return index of element otherwise") {
      val single = ArraySeq[Int](5)
      assert(single(searchFun(6, single, ord, 0, 0)) == 5)

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assert(arr(searchFun(2, arr, ord, 0, 5)) == 1)
      assert(arr(searchFun(3, arr, ord, 0, 5)) == 1)
      assert(arr(searchFun(6, arr, ord, 0, 1)) == 3)
      assert(arr(searchFun(6, arr, ord, 1, 1)) == 3)
      assert(arr(searchFun(6, arr, ord, 1, 2)) == 5)
      assert(arr(searchFun(6, arr, ord, 1, 3)) == 5)
      assert(arr(searchFun(6, arr, ord, 1, 4)) == 5)
      assert(arr(searchFun(6, arr, ord, 1, 5)) == 5)
      assert(arr(searchFun(6, arr, ord, 0, 5)) == 5)
      assert(arr(searchFun(7, arr, ord, 1, 3)) == 5)
      assert(arr(searchFun(7, arr, ord, 1, 4)) == 5)
      assert(arr(searchFun(7, arr, ord, 1, 5)) == 5)
      assert(arr(searchFun(7, arr, ord, 0, 5)) == 5)
      assert(arr(searchFun(12, arr, ord, 0, 5)) == 11)
      assert(arr(searchFun(12, arr, ord, 4, 5)) == 11)
      assert(arr(searchFun(12, arr, ord, 5, 5)) == 11)

      val arr2 = ArraySeq[Int](0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
      val end2 = arr2.length - 1
      assert(arr2(searchFun(5, arr2, ord, 0, end2)) == 0)
      assert(arr2(searchFun(10, arr2, ord, 0, end2)) == 0)
      assert(arr2(searchFun(15, arr2, ord, 0, end2)) == 10)
      assert(arr2(searchFun(20, arr2, ord, 0, end2)) == 10)
      assert(arr2(searchFun(25, arr2, ord, 0, end2)) == 20)
      assert(arr2(searchFun(30, arr2, ord, 0, end2)) == 20)
      assert(arr2(searchFun(35, arr2, ord, 0, end2)) == 30)
      assert(arr2(searchFun(40, arr2, ord, 0, end2)) == 30)
      assert(arr2(searchFun(45, arr2, ord, 0, end2)) == 40)
      assert(arr2(searchFun(50, arr2, ord, 0, end2)) == 40)
      assert(arr2(searchFun(55, arr2, ord, 0, end2)) == 50)
      assert(arr2(searchFun(60, arr2, ord, 0, end2)) == 50)
      assert(arr2(searchFun(65, arr2, ord, 0, end2)) == 60)
      assert(arr2(searchFun(70, arr2, ord, 0, end2)) == 60)
      assert(arr2(searchFun(75, arr2, ord, 0, end2)) == 70)
      assert(arr2(searchFun(80, arr2, ord, 0, end2)) == 70)
      assert(arr2(searchFun(85, arr2, ord, 0, end2)) == 80)
      assert(arr2(searchFun(90, arr2, ord, 0, end2)) == 80)
      assert(arr2(searchFun(95, arr2, ord, 0, end2)) == 90)
      assert(arr2(searchFun(100, arr2, ord, 0, end2)) == 90)
      assert(arr2(searchFun(105, arr2, ord, 0, end2)) == 100)
    }
  }

  def searchClosestNotLess(searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int): Unit = {

    val ord: Order[Int] = instances.int.intAscOrder

    it("should throw OutOfBounds exception for illegal indexes") {
      val emp = ArraySeq.empty[Int]
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, emp, ord, 0, 0))

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, 0, 6))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, -1, 2))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, -2, -1))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, 7, 8))
    }

    it("should throw IllegalArgument exception if start index is greater then end") {
      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assertThrows[IllegalArgumentException](searchFun(6, arr, ord, 4, 3))
    }

    it("should return SortedArraySearch.NotFound if array contains no such element") {
      val single = ArraySeq[Int](7)
      assert(searchFun(8, single, ord, 0, 0) == NotFound)

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assert(searchFun(12, arr, ord, 0, 5) == NotFound)
      assert(searchFun(8, arr, ord, 3, 3) == NotFound)
      assert(searchFun(12, arr, ord, 3, 5) == NotFound)
    }

    it("should return index of element otherwise") {
      val single = ArraySeq[Int](5)
      assert(single(searchFun(4, single, ord, 0, 0)) == 5)

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assert(arr(searchFun(0, arr, ord, 0, 5)) == 1)
      assert(arr(searchFun(2, arr, ord, 0, 1)) == 3)
      assert(arr(searchFun(1, arr, ord, 1, 1)) == 3)
      assert(arr(searchFun(4, arr, ord, 1, 2)) == 5)
      assert(arr(searchFun(4, arr, ord, 1, 3)) == 5)
      assert(arr(searchFun(4, arr, ord, 1, 4)) == 5)
      assert(arr(searchFun(4, arr, ord, 1, 5)) == 5)
      assert(arr(searchFun(4, arr, ord, 0, 5)) == 5)
      assert(arr(searchFun(10, arr, ord, 0, 5)) == 11)
      assert(arr(searchFun(10, arr, ord, 4, 5)) == 11)
      assert(arr(searchFun(10, arr, ord, 5, 5)) == 11)

      val arr2 = ArraySeq[Int](0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
      val end2 = arr2.length - 1
      assert(arr2(searchFun(-1, arr2, ord, 0, end2)) == 0)
      assert(arr2(searchFun(0, arr2, ord, 0, end2)) == 0)
      assert(arr2(searchFun(5, arr2, ord, 0, end2)) == 10)
      assert(arr2(searchFun(10, arr2, ord, 0, end2)) == 10)
      assert(arr2(searchFun(15, arr2, ord, 0, end2)) == 20)
      assert(arr2(searchFun(20, arr2, ord, 0, end2)) == 20)
      assert(arr2(searchFun(25, arr2, ord, 0, end2)) == 30)
      assert(arr2(searchFun(30, arr2, ord, 0, end2)) == 30)
      assert(arr2(searchFun(35, arr2, ord, 0, end2)) == 40)
      assert(arr2(searchFun(40, arr2, ord, 0, end2)) == 40)
      assert(arr2(searchFun(45, arr2, ord, 0, end2)) == 50)
      assert(arr2(searchFun(50, arr2, ord, 0, end2)) == 50)
      assert(arr2(searchFun(55, arr2, ord, 0, end2)) == 60)
      assert(arr2(searchFun(60, arr2, ord, 0, end2)) == 60)
      assert(arr2(searchFun(65, arr2, ord, 0, end2)) == 70)
      assert(arr2(searchFun(70, arr2, ord, 0, end2)) == 70)
      assert(arr2(searchFun(75, arr2, ord, 0, end2)) == 80)
      assert(arr2(searchFun(80, arr2, ord, 0, end2)) == 80)
      assert(arr2(searchFun(85, arr2, ord, 0, end2)) == 90)
      assert(arr2(searchFun(90, arr2, ord, 0, end2)) == 90)
      assert(arr2(searchFun(95, arr2, ord, 0, end2)) == 100)
      assert(arr2(searchFun(100, arr2, ord, 0, end2)) == 100)
    }
  }

  def searchClosestGreater(searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int): Unit = {

    val ord: Order[Int] = instances.int.intAscOrder

    it("should throw OutOfBounds exception for illegal indexes") {
      val emp = ArraySeq.empty[Int]
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, emp, ord, 0, 0))

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, 0, 6))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, -1, 2))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, -2, -1))
      assertThrows[ArrayIndexOutOfBoundsException](searchFun(6, arr, ord, 7, 8))
    }

    it("should throw IllegalArgument exception if start index is greater then end") {
      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assertThrows[IllegalArgumentException](searchFun(6, arr, ord, 4, 3))
    }

    it("should return SortedArraySearch.NotFound if array contains no such element") {
      val single = ArraySeq[Int](7)
      assert(searchFun(7, single, ord, 0, 0) == NotFound)
      assert(searchFun(8, single, ord, 0, 0) == NotFound)

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assert(searchFun(11, arr, ord, 0, 5) == NotFound)
      assert(searchFun(12, arr, ord, 0, 5) == NotFound)
      assert(searchFun(7, arr, ord, 3, 3) == NotFound)
      assert(searchFun(8, arr, ord, 3, 3) == NotFound)
      assert(searchFun(11, arr, ord, 3, 5) == NotFound)
      assert(searchFun(12, arr, ord, 3, 5) == NotFound)
    }

    it("should return index of element otherwise") {
      val single = ArraySeq[Int](5)
      assert(single(searchFun(4, single, ord, 0, 0)) == 5)

      val arr = ArraySeq[Int](1, 3, 5, 7, 9, 11)
      assert(arr(searchFun(0, arr, ord, 0, 5)) == 1)
      assert(arr(searchFun(2, arr, ord, 0, 1)) == 3)
      assert(arr(searchFun(1, arr, ord, 1, 1)) == 3)
      assert(arr(searchFun(4, arr, ord, 1, 2)) == 5)
      assert(arr(searchFun(4, arr, ord, 1, 3)) == 5)
      assert(arr(searchFun(4, arr, ord, 1, 4)) == 5)
      assert(arr(searchFun(4, arr, ord, 1, 5)) == 5)
      assert(arr(searchFun(4, arr, ord, 0, 5)) == 5)
      assert(arr(searchFun(5, arr, ord, 1, 3)) == 7)
      assert(arr(searchFun(5, arr, ord, 1, 4)) == 7)
      assert(arr(searchFun(5, arr, ord, 1, 5)) == 7)
      assert(arr(searchFun(5, arr, ord, 0, 5)) == 7)
      assert(arr(searchFun(10, arr, ord, 0, 5)) == 11)
      assert(arr(searchFun(10, arr, ord, 4, 5)) == 11)
      assert(arr(searchFun(10, arr, ord, 5, 5)) == 11)

      val arr2 = ArraySeq[Int](0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
      val end2 = arr2.length - 1
      assert(arr2(searchFun(-1, arr2, ord, 0, end2)) == 0)
      assert(arr2(searchFun(0, arr2, ord, 0, end2)) == 10)
      assert(arr2(searchFun(5, arr2, ord, 0, end2)) == 10)
      assert(arr2(searchFun(10, arr2, ord, 0, end2)) == 20)
      assert(arr2(searchFun(15, arr2, ord, 0, end2)) == 20)
      assert(arr2(searchFun(20, arr2, ord, 0, end2)) == 30)
      assert(arr2(searchFun(25, arr2, ord, 0, end2)) == 30)
      assert(arr2(searchFun(30, arr2, ord, 0, end2)) == 40)
      assert(arr2(searchFun(35, arr2, ord, 0, end2)) == 40)
      assert(arr2(searchFun(40, arr2, ord, 0, end2)) == 50)
      assert(arr2(searchFun(45, arr2, ord, 0, end2)) == 50)
      assert(arr2(searchFun(50, arr2, ord, 0, end2)) == 60)
      assert(arr2(searchFun(55, arr2, ord, 0, end2)) == 60)
      assert(arr2(searchFun(60, arr2, ord, 0, end2)) == 70)
      assert(arr2(searchFun(65, arr2, ord, 0, end2)) == 70)
      assert(arr2(searchFun(70, arr2, ord, 0, end2)) == 80)
      assert(arr2(searchFun(75, arr2, ord, 0, end2)) == 80)
      assert(arr2(searchFun(80, arr2, ord, 0, end2)) == 90)
      assert(arr2(searchFun(85, arr2, ord, 0, end2)) == 90)
      assert(arr2(searchFun(90, arr2, ord, 0, end2)) == 100)
      assert(arr2(searchFun(95, arr2, ord, 0, end2)) == 100)
    }
  }
}

class SortedArraySearchSpec extends AnyFunSpec with SortedArraySearchBehaviors {

  import ordset.Order
  import ordset.array.SortedArraySearch._

  describe("Binary search in sorted array") {
    
    describe("of closest element not greater then specified") {
      
      val searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int =
        (elem, arr, ord, s, e) => binSearchClosestNotGreater[Int](elem, arr)(s, e)(ord)

      it should behave like searchClosestNotGreater(searchFun)
    }

    describe("of closest element less then specified") {

      val searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int =
        (elem, arr, ord, s, e) => binSearchClosestLess[Int](elem, arr)(s, e)(ord)

      it should behave like searchClosestLess(searchFun)
    }

    describe("of closest element not less then specified") {

      val searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int =
        (elem, arr, ord, s, e) => binSearchClosestNotLess[Int](elem, arr)(s, e)(ord)

      it should behave like searchClosestNotLess(searchFun)
    }

    describe("of closest element greater then specified") {

      val searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int =
        (elem, arr, ord, s, e) => binSearchClosestGreater[Int](elem, arr)(s, e)(ord)

      it should behave like searchClosestGreater(searchFun)
    }
  }

  describe("Linear search in sorted array") {

    describe("of closest element not greater then specified") {

      val searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int =
        (elem, arr, ord, s, e) => linSearchClosestNotGreater[Int](elem, arr)(s, e)(ord)

      it should behave like searchClosestNotGreater(searchFun)
    }

    describe("of closest element not less then specified") {

      val searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int =
        (elem, arr, ord, s, e) => linSearchClosestNotLess[Int](elem, arr)(s, e)(ord)

      it should behave like searchClosestNotLess(searchFun)
    }
  }

  describe("Optimistic binary search in sorted array") {

    describe("of closest element not greater then specified") {

      val searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int =
        (elem, arr, ord, s, e) => optimisticBinSearchClosestNotGreater[Int](elem, arr)(s, e)(ord)

      it should behave like searchClosestNotGreater(searchFun)
    }

    describe("of closest element not less then specified") {

      val searchFun: (Int, ArraySeq[Int], Order[Int], Int, Int) => Int =
        (elem, arr, ord, s, e) => optimisticBinSearchClosestNotLess[Int](elem, arr)(s, e)(ord)

      it should behave like searchClosestNotLess(searchFun)
    }
  }
}
