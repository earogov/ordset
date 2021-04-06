package test.ordset.util.label

import ordset.{Hash, Order}
import ordset.util.label.Label
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

import scala.collection.immutable.Queue

@RunWith(classOf[JUnitRunner])
class LabelSpec extends AnyFunSpec {

  it("should have consistent equality checks") {
    val order = Label.defaultOrder
    val hash = Label.defaultHash
    val a = Label("A")
    val a2 = Label("A")
    val b = Label("B")

    testObjectEquality(a, a2, b)
    testHashTypeclass(a, a2, b, hash)
    testObjectComparison(a, a2, b)
    testOrderTypeclass(a, a2, b, order)
  }

  it("should have consistent equality checks for set") {
    val hash = Label.defaultSetHash

    val abc = Set(Label("A"), Label("B"), Label("C"))
    val abc2 = Set(Label("C"), Label("B"), Label("A"))
    val ab = Set(Label("A"), Label("B"))

    testObjectEquality(abc, abc2, ab)
    testHashTypeclass(abc, abc2, ab, hash)
  }

  it("should have consistent equality checks for queue") {
    val order = Label.defaultQueueOrder
    val hash = Label.defaultQueueHash

    val abc = Queue(Label("A"), Label("B"), Label("C"))
    val abc2 = Queue(Label("A"), Label("B"), Label("C"))
    val ba = Queue(Label("B"), Label("A"))

    testObjectEquality(abc, abc2, ba)
    testHashTypeclass(abc, abc2, ba, hash)
    testOrderTypeclass(abc, abc2, ba, order)
  }

  private def testObjectEquality[T](x: T, same: T, notSame: T): Unit = {
    assert(x == x)
    assert(x == same)
    assert(same == x)
    assert(x != notSame)
    assert(notSame != x)

    assert(x.hashCode() == x.hashCode())
    assert(x.hashCode() == same.hashCode())
  }

  private def testHashTypeclass[T](x: T, same: T, notSame: T, hash: Hash[T]): Unit = {
    assert(hash.eqv(x, x))
    assert(hash.eqv(x, same))
    assert(hash.eqv(same, x))
    assert(!hash.eqv(x, notSame))
    assert(!hash.eqv(notSame, x))

    assert(hash.hash(x) == hash.hash(x))
    assert(hash.hash(x) == hash.hash(same))
  }

  private def testObjectComparison[T <: Comparable[T]](x: T, same: T, greater: T): Unit = {
    assert(x.compareTo(x) == 0)
    assert(x.compareTo(same) == 0)
    assert(same.compareTo(x) == 0)
    assert(x.compareTo(greater) < 0)
    assert(greater.compareTo(x) > 0)
  }

  private def testOrderTypeclass[T](x: T, same: T, greater: T, order: Order[T]): Unit = {
    assert(order.eqv(x, x))
    assert(order.eqv(x, same))
    assert(order.eqv(same, x))
    assert(!order.eqv(x, greater))
    assert(!order.eqv(greater, x))

    assert(order.compare(x, x) == 0)
    assert(order.compare(x, same) == 0)
    assert(order.compare(same, x) == 0)
    assert(order.compare(x, greater) < 0)
    assert(order.compare(greater, x) > 0)
  }
}
