package ordset.test.core.behaviors.segmentSeq

import ordset.Discrete
import ordset.core.value.ValueOps
import ordset.core.domain.{DomainOps, Domain}
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.core.segmentSeq.set.OrderedSetBuilderIterable
import ordset.test.core.behaviors.segmentSeq.ValidationTest.{TestCase, FailureCase, SuccessCase}
import org.scalatest.funspec.AnyFunSpec
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import scala.language.postfixOps
import cats.implicits
import scala.collection.mutable.ListBuffer
import ordset.core.segmentSeq.map.OrderedMapBuilderIterable

trait BuilderValidationBehaviors {
  this: AnyFunSpec =>

  import ordset.givens.int._
  import ordset.givens.bigInt._
  import ordset.givens.double._
  import ordset.givens.iterable._
  import ordset.givens.tuple2._
  import ordset.implementations.int
  import ordset.implementations.bigInt
  import ordset.implementations.double

  def sequenceOfIntervalsForOrderedSetIsValidated: Unit = {

    it("should validate sequence of intervals on unbounded continuous domain") {

      ValidationTest(continuousUnbounded.setTestCases).run
    }

    it("should validate sequence of intervals on bounded continuous domain") {

      ValidationTest(continuousBounded.setTestCases).run
    }

    it("should validate sequence of intervals on unbounded discrete domain") {

      ValidationTest(discreteUnbounded.setTestCases).run
    }

    it("should validate sequence of intervals on bounded discrete domain") {

      ValidationTest(discreteBounded.setTestCases).run
    }
  }

  def sequenceOfIntervalsForOrderedMapIsValidated: Unit = {

    it("should validate sequence of intervals on unbounded continuous domain") {

      ValidationTest(continuousUnbounded.mapTestCases).run
    }

    it("should validate sequence of intervals on bounded continuous domain") {

      ValidationTest(continuousBounded.mapTestCases).run
    }

    it("should validate sequence of intervals on unbounded discrete domain") {

      ValidationTest(discreteUnbounded.mapTestCases).run
    }

    it("should validate sequence of intervals on bounded discrete domain") {

      ValidationTest(discreteBounded.mapTestCases).run
    }
  }

  private object continuousUnbounded {

    type Dom[X] = Domain.ContinuousUnbounded[X]

    implicit val domainOps: DomainOps[Double, Dom] = DomainOps.UnboundedOps.default

    val x: BoundBuilder[Double, Dom] = BoundBuilder(domainOps)

    val setTestCases = List[TestCase[Interval[Double, Dom]]](
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x,
            x >= 0d
          )
        ),
        "Invalid sequence of intervals {{x}, {x >= 0.0}}: intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x <= 0d,
            x
          )
        ),
        "Invalid sequence of intervals {{x <= 0.0}, {x}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x < -10d,
            x >= 10d & x < 20d,
            x > 0d
          )
        ),
        "Invalid sequence of intervals {{10.0 <= x < 20.0}, {x > 0.0}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 2"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x >= 10d & x < 20d,
            x >= 10d
          )
        ),
        "Invalid sequence of intervals {{10.0 <= x < 20.0}, {x >= 10.0}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x >= 5d,
            x >= 10d
          )
        ),
        "Invalid sequence of intervals {{x >= 5.0}, {x >= 10.0}}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x > -100d & x < -10d,
            x >= 10d & x < 20d,
            x > 10d
          )
        ),
        "Invalid sequence of intervals {{10.0 <= x < 20.0}, {x > 10.0}}: " +
        "intervals must not overlap. Index = 2"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x > -100d & x < -10d,
            x >= 10d & x < 20d,
            x >= 20d
          )
        ),
        "Invalid sequence of intervals {{10.0 <= x < 20.0}, {x >= 20.0}}: " +
        "intervals must follow each other with a gap (adjacent intervals should be merged). Index = 2"
      ),
      SuccessCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x
          )
        )
      ),
      SuccessCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x > -100d & x < -10d,
            x >= 10d & x < 20d,
            x >= 200d
          )
        )
      )
    )

    val mapTestCases = List[TestCase[IntervalRelation[Double, Dom, String]]](
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x,
            "b" forAll x >= 0d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{x} -> a, {x >= 0.0} -> b}: intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x <= 0d,
            "b" forAll x
          ),
          ""
        ),
        "Invalid sequence of interval relations {{x <= 0.0} -> a, {x} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x < -10d,
            "b" forAll x >= 10d & x < 20d,
            "c" forAll x > 0d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10.0 <= x < 20.0} -> b, {x > 0.0} -> c}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x >= 10d & x < 20d,
            "b" forAll x >= 10d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10.0 <= x < 20.0} -> a, {x >= 10.0} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x >= 5d,
            "b" forAll x >= 10d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{x >= 5.0} -> a, {x >= 10.0} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -100d & x < -10d,
            "b" forAll x >= 10d & x < 20d,
            "c" forAll x > 10d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10.0 <= x < 20.0} -> b, {x > 10.0} -> c}: " +
        "intervals must not overlap. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -100d & x < -10d,
            "b" forAll x >= 10d & x < 20d,
            "b" forAll x >= 20d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10.0 <= x < 20.0} -> b, {x >= 20.0} -> b}: " +
        "adjacent intervals must have different values. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x
          ),
          "a"
        ),
        "Invalid interval relation {x} -> a: " +
        "value equals to the default value 'a' (such intervals should be dropped). Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -100d & x < -10d,
            "b" forAll x >= 10d & x < 20d,
            "c" forAll x > 10d
          ),
          "b"
        ),
        "Invalid interval relation {10.0 <= x < 20.0} -> b: " +
        "value equals to the default value 'b' (such intervals should be dropped). Index = 1"
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x
          ),
          ""
        )
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -100d & x < -10d,
            "b" forAll x >= 10d & x < 20d,
            "c" forAll x >= 200d
          ),
          ""
        )
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -100d & x < 10d,
            "b" forAll x >= 10d & x < 20d,
            "b" forAll x >= 200d
          ),
          ""
        )
      )
    )
  }

  private object continuousBounded {

    type Dom[X] = Domain.ContinuousBounded[X]

    implicit val domainOps: DomainOps[Double, Dom] = 
      DomainOps.BoundedOps.default(
        Domain.ContinuousBounded.default(double.tryNaturalOrderWithBounds(0d, false, 100d, true).get),
        doubleShow
      )

    val otherDomainOps: DomainOps[Double, Dom] = 
          DomainOps.BoundedOps.default(
            Domain.ContinuousBounded.default(double.tryNaturalOrderWithBounds(-100d, true, 200d, true).get),
            doubleShow
          )

    val x: BoundBuilder[Double, Dom] = BoundBuilder(domainOps)

    val setTestCases = List[TestCase[Interval[Double, Dom]]](
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            otherDomainOps.intervals.factory.belowBound(x < -20),
            x > 10
          )
        ),
        "Invalid interval {-100.0 <= x < -20.0}: " +
        "lower bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x < 10,
            otherDomainOps.intervals.factory.betweenBounds(x > 50, x < 150)
          )
        ),
        "Invalid interval {50.0 < x < 150.0}: " +
        "upper bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            none(x),
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            none(x),
            x > 50d
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x < 50d,
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x < 0d,
            x >= 10d
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x <= 10d,
            x >= 200d
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x,
            x >= 50d
          )
        ),
        "Invalid sequence of intervals {{0.0 < x <= 100.0}, {50.0 <= x <= 100.0}}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x <= 50d,
            x
          )
        ),
        "Invalid sequence of intervals {{0.0 < x <= 50.0}, {0.0 < x <= 100.0}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x > -10d,
            x >= 50d
          )
        ),
        "Invalid sequence of intervals {{0.0 < x <= 100.0}, {50.0 <= x <= 100.0}}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x <= 50d,
            x < 200d
          )
        ),
        "Invalid sequence of intervals {{0.0 < x <= 50.0}, {0.0 < x <= 100.0}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x < 10d,
            x > 10d & x < 20d,
            x > 5d
          )
        ),
        "Invalid sequence of intervals {{10.0 < x < 20.0}, {5.0 < x <= 100.0}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 2"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x >= 10d & x < 20d,
            x >= 10d
          )
        ),
        "Invalid sequence of intervals {{10.0 <= x < 20.0}, {10.0 <= x <= 100.0}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x > -10d & x <= 10d,
            x >= 10d & x < 20d,
            x > 30d
          )
        ),
        "Invalid sequence of intervals {{0.0 < x <= 10.0}, {10.0 <= x < 20.0}}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x > -10d & x < 5d,
            x >= 10d & x < 20d,
            x >= 20d
          )
        ),
        "Invalid sequence of intervals {{10.0 <= x < 20.0}, {20.0 <= x <= 100.0}}: " +
        "intervals must follow each other with a gap (adjacent intervals should be merged). Index = 2"
      ),
      SuccessCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x
          )
        )
      ),
      SuccessCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x > -100d & x < 10d,
            x >= 15d & x < 20d,
            x >= 30d
          )
        )
      )
    )

    val mapTestCases = List[TestCase[IntervalRelation[Double, Dom, String]]](
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll otherDomainOps.intervals.factory.belowBound(x < -20),
            "b" forAll x > 10
          ),
          ""
        ),
        "Invalid interval relation {-100.0 <= x < -20.0} -> a: " +
        "lower bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x < 10,
            "b" forAll otherDomainOps.intervals.factory.betweenBounds(x > 50, x < 150)
          ),
          ""
        ),
        "Invalid interval relation {50.0 < x < 150.0} -> b: " +
        "upper bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll none(x),
            "b" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll none(x),
            "b" forAll x > 50d
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x < 50d,
            "b" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> b: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x < 0d,
            "b" forAll x >= 10d
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x <= 10d,
            "b" forAll x >= 200d
          ),
          ""
        ),
        "Invalid interval relation {} -> b: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x,
            "b" forAll x >= 50d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0.0 < x <= 100.0} -> a, {50.0 <= x <= 100.0} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x <= 50d,
            "b" forAll x
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0.0 < x <= 50.0} -> a, {0.0 < x <= 100.0} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -10d,
            "b" forAll x >= 50d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0.0 < x <= 100.0} -> a, {50.0 <= x <= 100.0} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x <= 50d,
            "b" forAll x < 200d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0.0 < x <= 50.0} -> a, {0.0 < x <= 100.0} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x < 10d,
            "b" forAll x > 10d & x < 20d,
            "c" forAll x > 5d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10.0 < x < 20.0} -> b, {5.0 < x <= 100.0} -> c}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x >= 10d & x < 20d,
            "b" forAll x >= 10d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10.0 <= x < 20.0} -> a, {10.0 <= x <= 100.0} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -10d & x <= 10d,
            "b" forAll x >= 10d & x < 20d,
            "c" forAll x > 30d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0.0 < x <= 10.0} -> a, {10.0 <= x < 20.0} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -10d & x < 5d,
            "b" forAll x >= 10d & x < 20d,
            "b" forAll x >= 20d
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10.0 <= x < 20.0} -> b, {20.0 <= x <= 100.0} -> b}: " +
        "adjacent intervals must have different values. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -10d & x < 5d,
            "" forAll x >= 10d & x < 20d,
            "b" forAll x >= 20d
          ),
          ""
        ),
        "Invalid interval relation {10.0 <= x < 20.0} -> : " +
        "value equals to the default value '' (such intervals should be dropped). Index = 1"
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x
          ),
          ""
        )
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -100d & x < 10d,
            "b" forAll x >= 10d & x < 20d,
            "c" forAll x >= 30d
          ),
          ""
        )
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Double, Dom, String]](
            "a" forAll x > -100d & x < 10d,
            "b" forAll x >= 10d & x < 20d,
            "b" forAll x >= 50d
          ),
          ""
        )
      )
    )
  }

  private object discreteUnbounded {

    type Dom[X] = Domain.DiscreteUnbounded[X]

    implicit val domainOps: DomainOps[BigInt, Dom] = 
      DomainOps.UnboundedOps.default(
        Domain.DiscreteUnbounded.default(new bigInt.NaturalOrder), 
        bigIntShow
      )

    val x: BoundBuilder[BigInt, Dom] = BoundBuilder(domainOps)

    val setTestCases = List[TestCase[Interval[BigInt, Dom]]](
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            none(x),
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            none(x),
            x > 0
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x < 0,
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x,
            x >= 0
          )
        ),
        "Invalid sequence of intervals {{x}, {x >= 0}}: intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x <= 0,
            x
          )
        ),
        "Invalid sequence of intervals {{x <= 0}, {x}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x < -10,
            x >= 10 & x < 20,
            x > 0
          )
        ),
        "Invalid sequence of intervals {{10 <= x < 20}, {x > 0}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 2"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x > 9 & x < 20,
            x >= 10
          )
        ),
        "Invalid sequence of intervals {{9 < x < 20}, {x >= 10}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x > -100 & x < -10,
            x >= 10 & x < 20,
            x > 10
          )
        ),
        "Invalid sequence of intervals {{10 <= x < 20}, {x > 10}}: " +
        "intervals must not overlap. Index = 2"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x > -100 & x < -10,
            x >= 10 & x < 20,
            x >= 20
          )
        ),
        "Invalid sequence of intervals {{10 <= x < 20}, {x >= 20}}: " +
        "intervals must follow each other with a gap (adjacent intervals should be merged). Index = 2"
      ),
      SuccessCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x
          )
        )
      ),
      SuccessCase(
        OrderedSetBuilderIterable.default(
          List[Interval[BigInt, Dom]](
            x > -100 & x < -10,
            x >= 10 & x < 20,
            x > 20
          )
        )
      )
    )

    val mapTestCases = List[TestCase[IntervalRelation[BigInt, Dom, String]]](
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll none(x),
            "b" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll none(x),
            "b" forAll x > 0
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x < 0,
            "b" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> b: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x,
            "b" forAll x >= 0
          ),
          ""
        ),
        "Invalid sequence of interval relations {{x} -> a, {x >= 0} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x <= 0,
            "b" forAll x
          ),
          ""
        ),
        "Invalid sequence of interval relations {{x <= 0} -> a, {x} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x < -10,
            "b" forAll x >= 10 & x < 20,
            "c" forAll x > 0
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10 <= x < 20} -> b, {x > 0} -> c}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x > 9 & x < 20,
            "b" forAll x >= 10
          ),
          ""
        ),
        "Invalid sequence of interval relations {{9 < x < 20} -> a, {x >= 10} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x > -100 & x < -10,
            "b" forAll x >= 10 & x < 20,
            "c" forAll x > 10
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10 <= x < 20} -> b, {x > 10} -> c}: " +
        "intervals must not overlap. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x > -100 & x < -10,
            "b" forAll x >= 10 & x < 20,
            "b" forAll x >= 20
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10 <= x < 20} -> b, {x >= 20} -> b}: " +
        "adjacent intervals must have different values. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x > -100 & x < -10,
            "" forAll x >= 10 & x < 20,
            "c" forAll x >= 20
          ),
          ""
        ),
        "Invalid interval relation {10 <= x < 20} -> : " +
        "value equals to the default value '' (such intervals should be dropped). Index = 1"
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x
          ),
          ""
        )
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[BigInt, Dom, String]](
            "a" forAll x > -100 & x < -10,
            "b" forAll x >= 10 & x < 20,
            "c" forAll x >= 20
          ),
          ""
        )
      )
    )
  }

  private object discreteBounded {

    type Dom[X] = Domain.DiscreteBounded[X]

    implicit val domainOps: DomainOps[Int, Dom] = 
      DomainOps.BoundedOps.default(
        Domain.DiscreteBounded.default(int.tryNaturalOrderWithBounds(0, 100).get),
        intShow
      )

    val otherDomainOps: DomainOps[Int, Dom] = 
      DomainOps.BoundedOps.default(
        Domain.DiscreteBounded.default(int.tryNaturalOrderWithBounds(-100, 200).get),
        intShow
      )

    val x: BoundBuilder[Int, Dom] = BoundBuilder(domainOps)

    val setTestCases = List[TestCase[Interval[Int, Dom]]](
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            otherDomainOps.intervals.factory.belowBound(x < -20),
            x > 10
          )
        ),
        "Invalid interval {-100 <= x < -20}: " +
        "lower bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x < 10,
            otherDomainOps.intervals.factory.betweenBounds(x > 50, x < 150)
          )
        ),
        "Invalid interval {50 < x < 150}: " +
        "upper bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            none(x),
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            none(x),
            x > 50
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x < 50,
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x < 0,
            x >= 10
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x <= 10,
            x >= 200
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x,
            x >= 50
          )
        ),
        "Invalid sequence of intervals {{0 <= x <= 100}, {50 <= x <= 100}}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x <= 50,
            x
          )
        ),
        "Invalid sequence of intervals {{0 <= x <= 50}, {0 <= x <= 100}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x > -10,
            x >= 50
          )
        ),
        "Invalid sequence of intervals {{0 <= x <= 100}, {50 <= x <= 100}}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x <= 50,
            x < 200
          )
        ),
        "Invalid sequence of intervals {{0 <= x <= 50}, {0 <= x <= 100}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x < 10,
            x > 10 & x < 20,
            x > 5
          )
        ),
        "Invalid sequence of intervals {{10 < x < 20}, {5 < x <= 100}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 2"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x >= 10 & x < 20,
            x > 9
          )
        ),
        "Invalid sequence of intervals {{10 <= x < 20}, {9 < x <= 100}}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x > -10 & x <= 10,
            x >= 10 & x < 20,
            x > 30
          )
        ),
        "Invalid sequence of intervals {{0 <= x <= 10}, {10 <= x < 20}}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x > -10 & x < 5,
            x >= 10 & x < 20,
            x >= 20
          )
        ),
        "Invalid sequence of intervals {{10 <= x < 20}, {20 <= x <= 100}}: " +
        "intervals must follow each other with a gap (adjacent intervals should be merged). Index = 2"
      ),
      SuccessCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x
          )
        )
      ),
      SuccessCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Int, Dom]](
            x > -100 & x < 10,
            x >= 15 & x < 20,
            x >= 30
          )
        )
      )
    )

    val mapTestCases = List[TestCase[IntervalRelation[Int, Dom, String]]](
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll otherDomainOps.intervals.factory.belowBound(x < -20),
            "b" forAll x > 10
          ),
          ""
        ),
        "Invalid interval relation {-100 <= x < -20} -> a: " +
        "lower bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x < 10,
            "b" forAll otherDomainOps.intervals.factory.betweenBounds(x > 50, x < 150)
          ),
          ""
        ),
        "Invalid interval relation {50 < x < 150} -> b: " +
        "upper bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll none(x),
            "b" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll none(x),
            "b" forAll x > 50
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x < 50,
            "b" forAll none(x)
          ),
          ""
        ),
        "Invalid interval relation {} -> b: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x < 0,
            "b" forAll x >= 10
          ),
          ""
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x <= 10,
            "b" forAll x >= 200
          ),
          ""
        ),
        "Invalid interval relation {} -> b: interval must be non-empty. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x,
            "b" forAll x >= 50
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0 <= x <= 100} -> a, {50 <= x <= 100} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x <= 50,
            "b" forAll x
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0 <= x <= 50} -> a, {0 <= x <= 100} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x > -10,
            "b" forAll x >= 50
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0 <= x <= 100} -> a, {50 <= x <= 100} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x <= 50,
            "b" forAll x < 200
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0 <= x <= 50} -> a, {0 <= x <= 100} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x < 10,
            "b" forAll x >= 10 & x < 20,
            "c" forAll x > 5
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10 <= x < 20} -> b, {5 < x <= 100} -> c}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x >= 10 & x < 20,
            "b" forAll x > 9
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10 <= x < 20} -> a, {9 < x <= 100} -> b}: " +
        "intervals must be sorted by lower bound in ascending order. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x > -10 & x <= 10,
            "b" forAll x >= 10 & x < 20,
            "c" forAll x > 30
          ),
          ""
        ),
        "Invalid sequence of interval relations {{0 <= x <= 10} -> a, {10 <= x < 20} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x > -10 & x < 5,
            "b" forAll x >= 10 & x < 20,
            "b" forAll x >= 20
          ),
          ""
        ),
        "Invalid sequence of interval relations {{10 <= x < 20} -> b, {20 <= x <= 100} -> b}: " +
        "adjacent intervals must have different values. Index = 2"
      ),
      FailureCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x > -10 & x <= 10,
            "b" forAll x >= 10 & x < 20,
            "c" forAll x > 30
          ),
          "b"
        ),
        "Invalid interval relation {10 <= x < 20} -> b: " +
        "value equals to the default value 'b' (such intervals should be dropped). Index = 1"
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x
          ),
          ""
        )
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x > -100 & x < 10,
            "a" forAll x >= 15 & x < 20,
            "c" forAll x >= 30
          ),
          ""
        )
      ),
      SuccessCase(
        OrderedMapBuilderIterable.default(
          List[IntervalRelation[Int, Dom, String]](
            "a" forAll x > -100 & x < 15,
            "b" forAll x >= 15 & x < 20,
            "b" forAll x >= 30
          ),
          "c"
        )
      )
    )
  }
}
