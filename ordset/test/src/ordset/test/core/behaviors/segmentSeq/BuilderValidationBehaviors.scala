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
  }

  private object continuousUnbounded {

    type Dom[X] = Domain.ContinuousUnbounded[X]

    implicit val domainOps: DomainOps[Double, Dom] = DomainOps.UnboundedOps.default

    val x: BoundBuilder[Double, Dom] = BoundBuilder(domainOps)

    val setTestCases = List[TestCase[Interval[Double, Dom]]](
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
            x > 0d
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        OrderedSetBuilderIterable.default(
          List[Interval[Double, Dom]](
            x < 0d,
            none(x)
          )
        ),
        "Invalid interval {}: interval must be non-empty. Index = 1"
      ),
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

    implicit val domainOps: DomainOps[Double, Domain.ContinuousBounded] = 
      DomainOps.BoundedOps.default(
        Domain.ContinuousBounded.default(double.tryNaturalOrderWithBounds(0d, false, 100d, true).get),
        doubleShow
      )

    val x: BoundBuilder[Double, Dom] = BoundBuilder(domainOps)

    val setTestCases = List[TestCase[Interval[Double, Dom]]](
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
            x > -10,
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
            x < 200
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
}
