package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.{DomainOps, Domain}
import ordset.core.interval.Interval
import ordset.core.segmentSeq.set.OrderedSetBuilderIterable
import ordset.test.core.behaviors.segmentSeq.ValidationTest.{TestCase, FailureCase, SuccessCase}
import org.scalatest.funspec.AnyFunSpec
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import scala.language.postfixOps

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
  }
}
