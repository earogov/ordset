package ordset.test.core.behaviors.segmentSeq

import ordset.{Discrete, ContravariantShow}
import ordset.core.{Bound, ExtendedBound}
import ordset.core.value.ValueOps
import ordset.core.domain.{DomainOps, Domain}
import ordset.core.segmentSeq.set.OrderedSetFactoryIterable
import ordset.core.segmentSeq.map.{OrderedMapFactoryIterable, BoundValue}
import ordset.test.core.behaviors.segmentSeq.FactoryValidationTest.{TestCase, FailureCase, SuccessCase}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import scala.language.postfixOps
import org.scalatest.funspec.AnyFunSpec
import scala.collection.mutable.ListBuffer
import ordset.core.ExtendedBound
import junit.framework.Test

trait FactoryValidationBehaviors {
  this: AnyFunSpec =>
  
  import ordset.givens.int._
  import ordset.givens.bigInt._
  import ordset.givens.double._
  import ordset.givens.iterable._
  import ordset.givens.tuple2._
  import ordset.implementations.int
  import ordset.implementations.bigInt
  import ordset.implementations.double

  def sequenceOfBoundsForOrderedSetIsValidated: Unit = {

    it("should validate sequence of bounds on unbounded continuous domain") {

      FactoryValidationTest(continuousUnbounded.setTestCases).run
    }

    it("should validate sequence of bounds on bounded continuous domain") {

      FactoryValidationTest(continuousBounded.setTestCases).run
    }

    it("should validate sequence of bounds on unbounded discrete domain") {

      FactoryValidationTest(discreteUnbounded.setTestCases).run
    }

    it("should validate sequence of bounds on bounded discrete domain") {

      FactoryValidationTest(discreteBounded.setTestCases).run
    }
  }

  def sequenceOfBoundsForOrderedMapIsValidated: Unit = {

    it("should validate sequence of bounds on unbounded continuous domain") {

      FactoryValidationTest(continuousUnbounded.mapTestCases).run
    }

    it("should validate sequence of bounds on bounded continuous domain") {

      FactoryValidationTest(continuousBounded.mapTestCases).run
    }

    it("should validate sequence of bounds on unbounded discrete domain") {

      FactoryValidationTest(discreteUnbounded.mapTestCases).run
    }

    it("should validate sequence of bounds on bounded discrete domain") {

      FactoryValidationTest(discreteBounded.mapTestCases).run
    }
  }

  private def generateBoundValues[E, V](
    bounds: Iterable[Bound.Upper[E]]
  )(
    implicit 
    values: Discrete.Succeeding.Infinite[V],
    valueOps: ValueOps[V]
  ): Iterable[BoundValue[E, V]] = {
    var value = valueOps.unit
    val buffer = bounds.foldLeft(new ListBuffer[BoundValue[E, V]]()) { (buf, bound) =>
      buf.addOne((bound, value))
      value = values.successor(value)
      buf
    }
    buffer.addOne((ExtendedBound.AboveAll, value))
  }

  private def generateMapTestCases[E, D[X] <: Domain[X], V](
    setCases: Iterable[TestCase[Bound.Upper[E]]]
  )(
    implicit 
    values: Discrete.Succeeding.Infinite[V],
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Iterable[TestCase[BoundValue[E, V]]] = 
    setCases.map { c => 
      val boundValues = OrderedMapFactoryIterable.default(generateBoundValues(c.iterable))
      val boundValueShow = iterableShow(tuple2Show(domainOps.showOps.extendedShow, valueOps.valueShow))
      (c: TestCase[Bound.Upper[E]]) match {
        case c: FailureCase[Bound.Upper[E]] => FailureCase(boundValues, c.error)(boundValueShow)
        case c: SuccessCase[Bound.Upper[E]] => SuccessCase(boundValues)(boundValueShow)
      } 
    }

  private object continuousUnbounded {

    implicit val domainOps: DomainOps[Double, Domain.ContinuousUnbounded] = DomainOps.UnboundedOps.default

    val setTestCases = List[TestCase[Bound.Upper[Double]]](
      FailureCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(0d`]`, -1d`)`, 10d`]`)),
        "Invalid sequence of bounds {x <= 0.0, x < -1.0}: sequence must be monotonically increasing. Index = 1"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(0d`]`, 1d`)`, -1d`]`)),
        "Invalid sequence of bounds {x < 1.0, x <= -1.0}: sequence must be monotonically increasing. Index = 2"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(0d`]`, 1d`]`, 1d`)`)),
        "Invalid sequence of bounds {x <= 1.0, x < 1.0}: sequence must be monotonically increasing. Index = 2"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(1d`]`, 1d`]`, 1d`]`)),
        "Invalid sequence of bounds {x <= 1.0, x <= 1.0}: sequence must be monotonically increasing. Index = 1"
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List()),
      ),
      SuccessCase(
        OrderedSetFactoryIterable.single[Double, Domain.ContinuousBounded](0d`]`),
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(0d`]`, 1d`)`, 1d`]`))
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(0d`]`, 1d`)`, 2d`]`, 3d`)`, 4d`]`))
      )
    )

    val mapTestCases: List[TestCase[BoundValue[Double, BigInt]]] = 
      List(
        generateMapTestCases(setTestCases),
        List(
          FailureCase(
            OrderedMapFactoryIterable.default[Double, Domain.ContinuousBounded, BigInt](
              List((0d`]`, 0), (10d`)`, 0), (100d`]`, 1), (ExtendedBound.AboveAll, 2))
            ),
            "Invalid sequence of values {0, 0}: adjacent values must be non-equal. Index = 1"
          ),
          FailureCase(
            OrderedMapFactoryIterable.default[Double, Domain.ContinuousBounded, BigInt](
              List((0d`]`, 100), (10d`)`, 0), (100d`]`, 1), (ExtendedBound.AboveAll, 1))
            ),
            "Invalid sequence of values {1, 1}: adjacent values must be non-equal. Index = 3"
          )
        )
      ).flatten
  }

  object continuousBounded {

    implicit val domainOps: DomainOps[Double, Domain.ContinuousBounded] = 
      DomainOps.BoundedOps.default(
        Domain.ContinuousBounded.default(double.tryNaturalOrderWithBounds(0d, false, 100d, true).get),
        doubleShow
      )

    val commonTestCases = List[TestCase[Bound.Upper[Double]]](
      FailureCase(
        OrderedSetFactoryIterable.single[Double, Domain.ContinuousBounded](0d`]`),
        "Invalid bound x <= 0.0: out of domain bounds [x > 0.0, x <= 100.0]. Index = 0"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(0d`]`)),
        "Invalid bound x <= 0.0: out of domain bounds [x > 0.0, x <= 100.0]. Index = 0"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(1d`)`, 10d`)`, 100d`)`, 101d`)`)),
        "Invalid bound x < 101.0: out of domain bounds [x > 0.0, x <= 100.0]. Index = 3"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(1d`]`, 100d`)`, 10d`]`)),
        "Invalid sequence of bounds {x < 100.0, x <= 10.0}: sequence must be monotonically increasing. Index = 2"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(1d`]`, 10d`]`, 10d`]`)),
        "Invalid sequence of bounds {x <= 10.0, x <= 10.0}: sequence must be monotonically increasing. Index = 2"
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List())
      ),
      SuccessCase(
        OrderedSetFactoryIterable.single[Double, Domain.ContinuousBounded](5d`]`)
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(1d`]`, 5d`]`, 10d`]`))
      )
    )

    val setTestCases: List[TestCase[Bound.Upper[Double]]] = 
      List(
        commonTestCases,
        List(
          FailureCase(
            OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](List(1d`)`, 10d`)`, 100d`]`, 101d`)`)),
            "Invalid bound x <= 100.0: bound must be less than upper bound of domain. Index = 2"
          )
        )
      ).flatten

    val mapTestCases: List[TestCase[BoundValue[Double, BigInt]]] = 
      List(
        generateMapTestCases(
          List(
            commonTestCases,
            List(
              FailureCase(
                OrderedSetFactoryIterable.default[Double, Domain.ContinuousBounded](
                  List(1d`)`, 10d`)`, 100d`]`, 101d`)`)
                ),
                "Invalid bound x <= 100.0: bound must be less than upper bound of domain, " +
                "or use `ExtendedBound.AboveAll` to specify last value of segment sequence. Index = 2"
              )
            )
          ).flatten
        ),
        List[TestCase[BoundValue[Double, BigInt]]](
          FailureCase(
            OrderedMapFactoryIterable.default[Double, Domain.ContinuousBounded, BigInt](
              List((1d`]`, 0), (10d`)`, 0), (100d`]`, 1), (ExtendedBound.AboveAll, 2))
            ),
            "Invalid sequence of values {0, 0}: adjacent values must be non-equal. Index = 1"
          ),
          FailureCase(
            OrderedMapFactoryIterable.default[Double, Domain.ContinuousBounded, BigInt](
              List((1d`]`, 100), (10d`)`, 0), (100d`)`, 1), (ExtendedBound.AboveAll, 1))
            ),
            "Invalid sequence of values {1, 1}: adjacent values must be non-equal. Index = 3"
          )
        )
      ).flatten
  }

  object discreteUnbounded {

    implicit val domainOps: DomainOps[BigInt, Domain.DiscreteUnbounded] = 
      DomainOps.UnboundedOps.default(
        Domain.DiscreteUnbounded.default(new bigInt.NaturalOrder), 
        bigIntShow
      )

    val setTestCases = List[TestCase[Bound.Upper[BigInt]]](
      FailureCase(
        OrderedSetFactoryIterable.default[BigInt, Domain.DiscreteUnbounded](
          List(BigInt(0) `]`, BigInt(-11) `)`, BigInt(10) `]`)
        ),
        "Invalid sequence of bounds {x <= 0, x < -11}: sequence must be monotonically increasing. Index = 1"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[BigInt, Domain.DiscreteUnbounded](
          List(BigInt(0) `]`, BigInt(5) `)`, BigInt(-11) `]`)
        ),
        "Invalid sequence of bounds {x < 5, x <= -11}: sequence must be monotonically increasing. Index = 2"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[BigInt, Domain.DiscreteUnbounded](
          List(BigInt(0) `]`, BigInt(5) `)`, BigInt(5) `)`)
        ),
        "Invalid sequence of bounds {x < 5, x < 5}: sequence must be monotonically increasing. Index = 2"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[BigInt, Domain.DiscreteUnbounded](
          List(BigInt(0) `]`, BigInt(5) `]`, BigInt(5) `)`)
        ),
        "Invalid sequence of bounds {x <= 5, x < 5}: sequence must be monotonically increasing. Index = 2"
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[BigInt, Domain.DiscreteUnbounded](List())
      ),
      SuccessCase(
        OrderedSetFactoryIterable.single[BigInt, Domain.DiscreteUnbounded](BigInt(5) `)`)
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[BigInt, Domain.DiscreteUnbounded](
          List(BigInt(0) `]`, BigInt(5) `)`, BigInt(5) `]`)
        )
      )
    )

    val mapTestCases: List[TestCase[BoundValue[BigInt, BigInt]]] = 
      List(
        generateMapTestCases(setTestCases),
        List(
          FailureCase(
            OrderedMapFactoryIterable.default[BigInt, Domain.DiscreteUnbounded, BigInt](
              List((BigInt(0) `]`, 0), (BigInt(10) `)`, 0), (BigInt(100) `]`, 1), (ExtendedBound.AboveAll, 2))
            ),
            "Invalid sequence of values {0, 0}: adjacent values must be non-equal. Index = 1"
          ),
          FailureCase(
            OrderedMapFactoryIterable.default[BigInt, Domain.DiscreteUnbounded, BigInt](
              List((BigInt(0) `]`, 100), (BigInt(10) `)`, 0), (BigInt(100) `]`, 1), (ExtendedBound.AboveAll, 1))
            ),
            "Invalid sequence of values {1, 1}: adjacent values must be non-equal. Index = 3"
          )
        )
      ).flatten
  }

  object discreteBounded {

    implicit val domainOps: DomainOps[Int, Domain.DiscreteBounded] = 
      DomainOps.BoundedOps.default(
        Domain.DiscreteBounded.default(int.tryNaturalOrderWithBounds(0, 100).get),
        intShow
      )

    val commonTestCases: List[TestCase[Bound.Upper[Int]]] = List(
      FailureCase(
        OrderedSetFactoryIterable.single[Int, Domain.DiscreteBounded](-1`]`),
        "Invalid bound x <= -1: out of domain bounds [x >= 0, x <= 100]. Index = 0"
      ),
      // We have got `out of domain bounds` message, because bounds `x <= 100` and `x < 101` are not equal on given
      // bounded discrete domain. 100 is the last element of bounded domain, and domain has no information about its
      // successor. So it can't figure out, that bounds are the same. In other words bounds `x <= 100` and `x < 101`
      // are equal on discrete unbounded domain, and are different on given bounded discrete domain.
      FailureCase(
        OrderedSetFactoryIterable.single[Int, Domain.DiscreteBounded](101`)`),
        "Invalid bound x < 101: out of domain bounds [x >= 0, x <= 100]. Index = 0"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Int, Domain.DiscreteBounded](List(-1`]`)),
        "Invalid bound x <= -1: out of domain bounds [x >= 0, x <= 100]. Index = 0"
      ),
      // See comment above for `OrderedSetFactoryIterable.single[Int, Domain.DiscreteBounded](101`)`)`.
      FailureCase(
        OrderedSetFactoryIterable.default[Int, Domain.DiscreteBounded](List(1`)`, 10`)`, 100`)`, 101`)`)),
        "Invalid bound x < 101: out of domain bounds [x >= 0, x <= 100]. Index = 3"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Int, Domain.DiscreteBounded](List(0`]`, 5`)`, 5`)`)),
        "Invalid sequence of bounds {x < 5, x < 5}: sequence must be monotonically increasing. Index = 2"
      ),
      FailureCase(
        OrderedSetFactoryIterable.default[Int, Domain.DiscreteBounded](List(0`]`, 5`]`, 5`)`)),
        "Invalid sequence of bounds {x <= 5, x < 5}: sequence must be monotonically increasing. Index = 2"
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[Int, Domain.DiscreteBounded](List())
      ),
      SuccessCase(
        OrderedSetFactoryIterable.default[Int, Domain.DiscreteBounded](List(1`)`, 10`)`, 100`)`))
      )
    )

    val setTestCases: List[TestCase[Bound.Upper[Int]]] = 
      List(
        commonTestCases,
        List(
          FailureCase(
            OrderedSetFactoryIterable.default[Int, Domain.DiscreteBounded](List(1`)`, 10`)`, 100`]`)),
            "Invalid bound x <= 100: bound must be less than upper bound of domain. Index = 2"
          )
        )
      ).flatten

    val mapTestCases: List[TestCase[BoundValue[Int, BigInt]]] =
      List(
        generateMapTestCases(
          List(
            commonTestCases,
            List(
              FailureCase(
                OrderedSetFactoryIterable.default[Int, Domain.DiscreteBounded](List(1`)`, 10`)`, 100`]`)),
                "Invalid bound x <= 100: bound must be less than upper bound of domain, " +
                "or use `ExtendedBound.AboveAll` to specify last value of segment sequence. Index = 2"
              )
            )
          ).flatten
        ),
        List(
          FailureCase(
            OrderedMapFactoryIterable.default[Int, Domain.DiscreteBounded, BigInt](
              List((0`]`, 0), (10`)`, 0), (100`]`, 1), (ExtendedBound.AboveAll, 2))
            ),
            "Invalid sequence of values {0, 0}: adjacent values must be non-equal. Index = 1"
          ),
          FailureCase(
            OrderedMapFactoryIterable.default[Int, Domain.DiscreteBounded, BigInt](
              List((0`]`, 100), (10`)`, 0), (100`)`, 1), (ExtendedBound.AboveAll, 1))
            ),
            "Invalid sequence of values {1, 1}: adjacent values must be non-equal. Index = 3"
          )
        )
      ).flatten
  }
}
