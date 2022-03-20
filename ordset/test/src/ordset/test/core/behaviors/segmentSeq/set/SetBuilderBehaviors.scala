package ordset.test.core.behaviors.segmentSeq.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.test.core.behaviors.segmentSeq.set.SetBuilderTest.*
import org.scalatest.funspec.AnyFunSpec
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import scala.language.postfixOps
import ordset.core.segmentSeq.set.{TreapOrderedSet, OrderedSetFactory, OrderedSet}

trait SetBuilderBehaviors {
  this: AnyFunSpec =>

  import ordset.givens.int._
  import ordset.givens.bigInt._
  import ordset.givens.double._
  import ordset.givens.iterable._
  import ordset.givens.tuple2._
  import ordset.implementations.int
  import ordset.implementations.bigInt
  import ordset.implementations.double
  import ordset.test.core.TestRngUtil.Givens._

  def orderedSetIsCreatedByBuilder: Unit = {

    it("should build ordered set on unbounded continuous domain") {

      SetBuilderTest(continuousUnbounded.testCases).run
    }

    it("should build ordered set on bounded continuous domain") {

      SetBuilderTest(continuousBounded.testCases).run
    }

    it("should build ordered set on unbounded discrete domain") {

      SetBuilderTest(discreteUnbounded.testCases).run
    }

    it("should build ordered set on bounded discrete domain") {

      SetBuilderTest(discreteBounded.testCases).run
    }
  }

  object continuousUnbounded {

    type E = Double
    type Dom[X] = Domain.ContinuousUnbounded[X]

    implicit val domainOps: DomainOps[E, Dom] = DomainOps.UnboundedOps.default
    
    val factory: OrderedSetFactory[E, Dom, OrderedSet[E, Dom]]#Provided = TreapOrderedSet.getFactory[E, Dom].provided
    
    val x: BoundBuilder[E, Dom] = BoundBuilder(domainOps)

    val testCases = List[TestCase[E, Dom]](
      SuccessCase(
        List(),
        factory.unsafeBuild(
          List(),
          false
        )
      ),
      SuccessCase(
        List(
          x
        ),
        factory.unsafeBuild(
          List(),
          true
        )
      ),
      SuccessCase(
        List(
          x > 10d
        ),
        factory.unsafeBuild(
          List(10d`](`),
          false
        )
      ),
      SuccessCase(
        List(
          x < 10d
        ),
        factory.unsafeBuild(
          List(10d`)[`),
          true
        )
      ),
      SuccessCase(
        List(
          x >= 20d & x < 30d,
        ),
        factory.unsafeBuild(
          List(20d`)[`, 30d`)[`),
          false
        )
      ),
      SuccessCase(
        List(
          x < 20d,
          x > 20d
        ),
        factory.unsafeBuild(
          List(20d`)[`, 20d`](`),
          true
        )
      ),
      SuccessCase(
        List(
          x < 10d,
          x >= 20d & x < 30d,
          x >= 50d
        ),
        factory.unsafeBuild(
          List(10d`)[`, 20d`)[`, 30d`)[`, 50d`)[`),
          true
        )
      ),
      FailureCase(
        List(
          x < 10d,
          x >= 10d
        ),
        "Invalid sequence of intervals {{x < 10.0}, {x >= 10.0}}: " +
        "intervals must follow each other with a gap (adjacent intervals should be merged). Index = 1"
      ),
      FailureCase(
        List(
          x < 10d,
          x >= -10d
        ),
        "Invalid sequence of intervals {{x < 10.0}, {x >= -10.0}}: intervals must not overlap. Index = 1"
      )
    )
  }

  object continuousBounded {

    type E = Double
    type Dom[X] = Domain.ContinuousBounded[X]

    implicit val domainOps: DomainOps[E, Dom] =
      DomainOps.BoundedOps.default(
        Domain.ContinuousBounded.default(double.tryNaturalOrderWithBounds(0d, false, 100d, true).get),
        doubleShow
      )

    val otherDomainOps: DomainOps[E, Dom] = 
      DomainOps.BoundedOps.default(
        Domain.ContinuousBounded.default(double.tryNaturalOrderWithBounds(-100d, true, 200d, true).get),
        doubleShow
      )

    val factory: OrderedSetFactory[E, Dom, OrderedSet[E, Dom]]#Provided = TreapOrderedSet.getFactory[E, Dom].provided

    val x: BoundBuilder[E, Dom] = BoundBuilder(domainOps)

    val y: BoundBuilder[E, Dom] = BoundBuilder(otherDomainOps)

    val testCases = List[TestCase[E, Dom]](
      SuccessCase(
        List(),
        factory.unsafeBuild(
          List(),
          false
        )
      ),
      SuccessCase(
        List(
          x
        ),
        factory.unsafeBuild(
          List(),
          true
        )
      ),
      SuccessCase(
        List(
          x > 10d
        ),
        factory.unsafeBuild(
          List(10d`](`),
          false
        )
      ),
      SuccessCase(
        List(
          x < 10d
        ),
        factory.unsafeBuild(
          List(10d`)[`),
          true
        )
      ),
      SuccessCase(
        List(
          x >= 20d & x < 30d,
        ),
        factory.unsafeBuild(
          List(20d`)[`, 30d`)[`),
          false
        )
      ),
      SuccessCase(
        List(
          x < 20d,
          x > 20d
        ),
        factory.unsafeBuild(
          List(20d`)[`, 20d`](`),
          true
        )
      ),
      SuccessCase(
        List(
          x < 10d,
          x >= 20d & x < 30d,
          x >= 50d
        ),
        factory.unsafeBuild(
          List(10d`)[`, 20d`)[`, 30d`)[`, 50d`)[`),
          true
        )
      ),
      FailureCase(
        List(
          x < 10d,
          x >= 20d & x < 90d,
          x >= 120d & x < 130d
        ),
        "Invalid interval {}: interval must be non-empty. Index = 2"
      ),
      FailureCase(
        List(
          y > -10d & y < 10d,
          y >= 20d & x < 90d
        ),
        "Invalid interval {-10.0 < x < 10.0}: " +
        "lower bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 0"
      ),
      FailureCase(
        List(
          x,
          y > 50 & y < 150
        ),
        "Invalid interval {50.0 < x < 150.0}: " +
        "upper bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 1"
      ),
      FailureCase(
        List(
          x,
          y > 120 & y < 150
        ),
        "Invalid interval {120.0 < x < 150.0}: " +
        "lower bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 1"
      )
    )
  }

  object discreteUnbounded {

    type E = BigInt
    type Dom[X] = Domain.DiscreteUnbounded[X]

    implicit val domainOps: DomainOps[E, Dom] =
      DomainOps.UnboundedOps.default(
        Domain.DiscreteUnbounded.default(new bigInt.NaturalOrder), 
        bigIntShow
      )

    val factory: OrderedSetFactory[E, Dom, OrderedSet[E, Dom]]#Provided = TreapOrderedSet.getFactory[E, Dom].provided

    val x: BoundBuilder[E, Dom] = BoundBuilder(domainOps)

    val testCases = List[TestCase[E, Dom]](
      SuccessCase(
        List(),
        factory.unsafeBuild(
          List(),
          false
        )
      ),
      SuccessCase(
        List(
          x
        ),
        factory.unsafeBuild(
          List(),
          true
        )
      ),
      SuccessCase(
        List(
          x > 10
        ),
        factory.unsafeBuild(
          List(BigInt(10)`](`),
          false
        )
      ),
      SuccessCase(
        List(
          x < 10
        ),
        factory.unsafeBuild(
          List(BigInt(10)`)[`),
          true
        )
      ),
      SuccessCase(
        List(
          x >= 20 & x < 30,
        ),
        factory.unsafeBuild(
          List(BigInt(20)`)[`, BigInt(30)`)[`),
          false
        )
      ),
      SuccessCase(
        List(
          x < 20,
          x > 20
        ),
        factory.unsafeBuild(
          List(BigInt(20)`)[`, BigInt(20)`](`),
          true
        )
      ),
      SuccessCase(
        List(
          x < 10,
          x >= 20 & x < 30,
          x >= 50
        ),
        factory.unsafeBuild(
          List(BigInt(10)`)[`, BigInt(20)`)[`, BigInt(30)`)[`, BigInt(50)`)[`),
          true
        )
      ),
      FailureCase(
        List(
          x <= 9,
          x >= 10
        ),
        "Invalid sequence of intervals {{x <= 9}, {x >= 10}}: " +
        "intervals must follow each other with a gap (adjacent intervals should be merged). Index = 1"
      ),
      FailureCase(
        List(
          x < 10,
          x >= -10
        ),
        "Invalid sequence of intervals {{x < 10}, {x >= -10}}: intervals must not overlap. Index = 1"
      )
    )
  }

  object discreteBounded {

    type E = Int
    type Dom[X] = Domain.DiscreteBounded[X]

    implicit val domainOps: DomainOps[E, Dom] =
      DomainOps.BoundedOps.default(
        Domain.DiscreteBounded.default(int.tryNaturalOrderWithBounds(0, 100).get),
        intShow
      )

    val otherDomainOps: DomainOps[E, Dom] = 
      DomainOps.BoundedOps.default(
        Domain.DiscreteBounded.default(int.tryNaturalOrderWithBounds(-100, 200).get),
        intShow
      )

    val factory: OrderedSetFactory[E, Dom, OrderedSet[E, Dom]]#Provided = TreapOrderedSet.getFactory[E, Dom].provided

    val x: BoundBuilder[E, Dom] = BoundBuilder(domainOps)

    val y: BoundBuilder[E, Dom] = BoundBuilder(otherDomainOps)

    val testCases = List[TestCase[E, Dom]](
      SuccessCase(
        List(),
        factory.unsafeBuild(
          List(),
          false
        )
      ),
      SuccessCase(
        List(
          x
        ),
        factory.unsafeBuild(
          List(),
          true
        )
      ),
      SuccessCase(
        List(
          x > 10
        ),
        factory.unsafeBuild(
          List(10`](`),
          false
        )
      ),
      SuccessCase(
        List(
          x < 10
        ),
        factory.unsafeBuild(
          List(10`)[`),
          true
        )
      ),
      SuccessCase(
        List(
          x >= 20 & x < 30,
        ),
        factory.unsafeBuild(
          List(20`)[`, 30`)[`),
          false
        )
      ),
      SuccessCase(
        List(
          x < 20,
          x > 20
        ),
        factory.unsafeBuild(
          List(20`)[`, 20`](`),
          true
        )
      ),
      SuccessCase(
        List(
          x < 10,
          x >= 20 & x < 30,
          x >= 50
        ),
        factory.unsafeBuild(
          List(10`)[`, 20`)[`, 30`)[`, 50`)[`),
          true
        )
      ),
      FailureCase(
        List(
          x <= 9,
          x >= 10
        ),
        "Invalid sequence of intervals {{0 <= x <= 9}, {10 <= x <= 100}}: " +
        "intervals must follow each other with a gap (adjacent intervals should be merged). Index = 1"
      ),
      FailureCase(
        List(
          x < 10,
          x >= 20 & x < 90,
          x >= 120 & x < 130
        ),
        "Invalid interval {}: interval must be non-empty. Index = 2"
      ),
      FailureCase(
        List(
          y > -10 & y < 10,
          y >= 20 & x < 90
        ),
        "Invalid interval {-10 < x < 10}: " +
        "lower bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 0"
      ),
      FailureCase(
        List(
          x,
          y > 50 & y < 150
        ),
        "Invalid interval {50 < x < 150}: " +
        "upper bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 1"
      ),
      FailureCase(
        List(
          x,
          y > 120 & y < 150
        ),
        "Invalid interval {120 < x < 150}: " +
        "lower bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 1"
      )
    )
  }
}
