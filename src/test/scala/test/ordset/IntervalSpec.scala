//package test.ordset
//
//import ordset.{Bound, Order, Eq, Interval}
//import org.scalatest.funspec.AnyFunSpec
//
//class IntervalSpec extends AnyFunSpec {
//
//  import ordset.syntax.SetBuilderNotation._
//  import cats.instances.int._
//
//  private val order: Order[Bound[Int]] = implicitly[Order[Bound[Int]]]
//  private val eq: Eq[Interval[Int]] = implicitly[Eq[Interval[Int]]]
//
//  private implicit def toCrossOps[E](interval: Interval[E]): CrossOps[E] = new CrossOps[E](interval)
//
//  it("should have valid bound indicators") {
//    validateLeftUnbounded(x <= 0)
//    validateLeftUnbounded(x < 0)
//    validateRightUnbounded(x >= 5)
//    validateRightUnbounded(x > 5)
//    validateBounded(x >= 0 & x < 10)
//    validateBounded(x >= 5 & x <= 5)
//    validateEmpty(Interval.empty())
//    validateUnbounded(Interval.universal())
//  }
//
//  it("should cross another interval") {
//    import Interval._
//
//    // Empty cases
//    assert(Empty.isCrossOf(Empty, Empty))
//    assert(Empty.isCrossOf(Empty, Universal))
//    assert(Empty.isCrossOf(Empty, x <= 0))
//    assert(Empty.isCrossOf(Empty, x > 0 & x < 5))
//    assert(Empty.isCrossOf(Empty, x >= 5))
//
//    // Unbounded cases
//    assert(Empty.isCrossOf(Universal, Empty))
//    assert(Universal.isCrossOf(Universal, Universal))
//    assert(Interval(x <= 5).isCrossOf(Universal, x <= 5))
//    assert((x >= 0 & x < 5).isCrossOf(Universal, x >= 0 & x < 5))
//    assert(Interval(x >= 5).isCrossOf(Universal, x >= 5))
//
//    // Left unbounded cases
//    // ----|
//    //        |-----
//    assert(Empty.isCrossOf(x <= 0, x > 0))
//    assert(Empty.isCrossOf(x <= 0, x > 5))
//    // --------|
//    //     |--------
//    assert((x >= 0 & x < 5).isCrossOf(x < 5, x >= 0))
//    // ----|
//    //     |--------
//    assert((x >= 5 & x <= 5).isCrossOf(x <= 5, x >= 5))
//    // --------|
//    // --------|
//    assert(Interval(x < 0).isCrossOf(x < 0, x < 0))
//    // --------|
//    // ------------|
//    assert(Interval(x < 0).isCrossOf(x < 0, x <= 5))
//    // --------|
//    //     |-------|
//    assert((x >= 0 & x < 5).isCrossOf(x < 5, x >= 0 & x < 10))
//    // ----|
//    //     |-------|
//    assert((x >= 0 & x <= 0).isCrossOf(x <= 0, x >= 0 & x < 10))
//    // ------------|
//    //     |---|
//    assert((x >= 0 & x < 5).isCrossOf(x < 10, x >= 0 & x < 5))
//    // ------------|
//    //         |---|
//    assert((x >= 0 & x < 5).isCrossOf(x < 5, x >= 0 & x < 5))
//    // ------------|
//    //             |
//    assert((x >= 0 & x <= 0).isCrossOf(x <= 0, x >= 0 & x <= 0))
//
//    // Right unbounded cases
//    // ----|
//    //        |-----
//    assert(Empty.isCrossOf(x > 5, x <= 0))
//    assert(Empty.isCrossOf(x > 5, x <= 5))
//    // --------|
//    //     |--------
//    assert((x >= 0 & x < 5).isCrossOf(x >= 0, x < 5))
//    // ----|
//    //     |--------
//    assert((x >= 0 & x <= 0).isCrossOf(x >= 0, x <= 0))
//    //     |--------
//    // |------------
//    assert(Interval(x >= 5).isCrossOf(x > 0, x >= 5))
//    //     |--------
//    //     |--------
//    assert(Interval(x >= 5).isCrossOf(x >= 5, x >= 5))
//    //     |--------
//    // |-------|
//    assert((x >= 0 & x < 5).isCrossOf(x >= 0, x >= -5 & x < 5))
//    // |------------
//    //     |---|
//    assert((x >= 0 & x < 5).isCrossOf(x > -10, x >= 0 & x < 5))
//    // |------------
//    // |---|
//    assert((x >= 0 & x < 5).isCrossOf(x >= 0, x >= 0 & x < 5))
//    // |------------
//    // |
//    assert((x >= 5 & x <= 5).isCrossOf(x >= 5, x >= 5 & x <= 5))
//
//    // Bounded cases
//    // |----|
//    //        |----|
//    assert(Empty.isCrossOf(x >= -10 & x < 0, x >= 0 & x < 10))
//    // |-----------|
//    //     |---|
//    assert((x >= 0 & x <= 5).isCrossOf(x >= -10 & x < 10, x >= 0 & x <= 5))
//    // |-----------|
//    // |---|
//    assert((x >= 0 & x <= 5).isCrossOf(x >= 0 & x < 10, x >= 0 & x <= 5))
//    // |-----------|
//    // |
//    assert((x >= 0 & x <= 0).isCrossOf(x >= 0 & x < 10, x >= 0 & x <= 0))
//    // |-----------|
//    //         |---|
//    assert((x >= 0 & x <= 5).isCrossOf(x >= -10 & x <= 5, x >= 0 & x <= 5))
//    // |-----------|
//    //             |
//    assert((x >= 5 & x <= 5).isCrossOf(x >= -10 & x <= 5, x >= 5 & x <= 5))
//    // |-------|
//    //     |-------|
//    assert((x >= 0 & x <= 5).isCrossOf(x > -10 & x <= 5, x >= 0 & x < 10))
//    // |-----|
//    //       |-----|
//    assert((x >= 5 & x <= 5).isCrossOf(x > -10 & x <= 5, x >= 5 & x < 10))
//  }
//
//  private def validateLeftUnbounded[E](x: Interval.Less[E]): Unit = {
//    assert(!x.isEmpty)
//    assert(x.isLeftUnbounded)
//    assert(!x.hasLowerBound)
//    assert(!x.isRightUnbounded)
//    assert(x.hasUpperBound)
//    assert(!x.isBounded)
//    assert(!x.isUniversal)
//  }
//
//  private def validateRightUnbounded[E](x: Interval.Greater[E]): Unit = {
//    assert(!x.isEmpty)
//    assert(!x.isLeftUnbounded)
//    assert(x.hasLowerBound)
//    assert(x.isRightUnbounded)
//    assert(!x.hasUpperBound)
//    assert(!x.isBounded)
//    assert(!x.isUniversal)
//  }
//
//  private def validateBounded[E](x: Interval.Between[E]): Unit = {
//    assert(!x.isEmpty)
//    assert(!x.isLeftUnbounded)
//    assert(x.hasLowerBound)
//    assert(!x.isRightUnbounded)
//    assert(x.hasUpperBound)
//    assert(x.isBounded)
//    assert(!x.isUniversal)
//  }
//
//  private def validateEmpty(x: Interval.Empty.type): Unit = {
//    assert(x.isEmpty)
//    assert(!x.isLeftUnbounded)
//    assert(!x.hasLeftBound)
//    assert(!x.isRightUnbounded)
//    assert(!x.hasRightBound)
//    assert(!x.isBounded)
//    assert(!x.isUnbounded)
//  }
//
//  private def validateUnbounded(x: Interval.Universal.type): Unit = {
//    assert(!x.isEmpty)
//    assert(!x.isLeftUnbounded)
//    assert(!x.hasLeftBound)
//    assert(!x.isRightUnbounded)
//    assert(!x.hasRightBound)
//    assert(!x.isBounded)
//    assert(x.isUnbounded)
//  }
//
//  private class CrossOps[E](interval: Interval[E]) {
//
//    def isCrossOf(x: Interval[E], y: Interval[E])(implicit eq: Eq[Interval[E]], order: Order[Bound[E]]): Boolean =
//      eq.eqv(interval, x.cross(y)) && eq.eqv(interval, y.cross(x))
//  }
//}
