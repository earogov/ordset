package ordset.test.core.behaviors.segmentSeq.map

import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.behaviors.segmentSeq.map.MapBuilderTest.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.map.{TreapOrderedMap, OrderedMapFactory, OrderedMap, BoundValue}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import scala.language.postfixOps

trait MapBuilderBehaviors {
  this: AnyFunSpec =>

  import ordset.givens.string._
  import ordset.givens.int._
  import ordset.givens.bigInt._
  import ordset.givens.double._
  import ordset.givens.iterable._
  import ordset.givens.tuple2._
  import ordset.implementations.int
  import ordset.implementations.bigInt
  import ordset.implementations.double
  import ordset.test.core.TestRngUtil.Givens._

  def orderedMapIsCreatedByBuilder: Unit = {

    it("should build ordered map on unbounded continuous domain") {

      MapBuilderTest(continuousUnbounded.testCases).run
    }

    it("should build ordered map on bounded continuous domain") {

      MapBuilderTest(continuousBounded.testCases).run
    }

    it("should build ordered map on unbounded discrete domain") {

      MapBuilderTest(discreteUnbounded.testCases).run
    }

    it("should build ordered map on bounded discrete domain") {

      MapBuilderTest(discreteBounded.testCases).run
    }
  }

  object continuousUnbounded {

    type E = Double
    type V = String
    type Dom[X] = Domain.ContinuousUnbounded[X]

    implicit val domainOps: DomainOps[E, Dom] = DomainOps.UnboundedOps.default

    val factory: OrderedMapFactory[E, Dom, V, OrderedMap[E, Dom, V]]#Provided = 
      TreapOrderedMap.getFactory[E, Dom, V].provided

    val x: BoundBuilder[E, Dom] = BoundBuilder(domainOps)

    val testCases = List[TestCase[E, Dom, V]](
      SuccessCase(
        "",
        List(),
        factory.buildUniform("")
      ),
      SuccessCase(
        "",
        List(
          "a" forAll x
        ),
        factory.buildUniform("a")
      ),
      SuccessCase(
        "",
        List(
          "a" forAll x <= 0
        ),
        factory.unsafeBuild(
          List(
            (0d`]`, "a"),
            (AboveAll, "")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10d & x <= 20d),
          "b" forAll (x > 20d & x < 30d)
        ),
        factory.unsafeBuild(
          List(
            (10d`]`, ""),
            (20d`]`, "a"),
            (30d`)`, "b"),
            (AboveAll, "")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10d & x < 20d),
          "a" forAll (x > 20d)
        ),
        factory.unsafeBuild(
          List(
            (10d`]`, ""),
            (20d`)`, "a"),
            (20d`]`, ""),
            (AboveAll, "a")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10d & x <= 20d),
          "b" forAll (x > 20d & x < 30d),
          "c" forAll (x >= 40d)
        ),
        factory.unsafeBuild(
          List(
            (10d`]`, ""),
            (20d`]`, "a"),
            (30d`)`, "b"),
            (40d`)`, ""),
            (AboveAll, "c")
          )
        )
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 0d),
          "" forAll (x > 10d & x <= 5d)
        ),
        "Invalid interval relation {} -> : interval must be non-empty. Index = 1"
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 0d),
          "" forAll (x > 10d & x <= 20d)
        ),
        "Invalid interval relation {10.0 < x <= 20.0} -> : " +
        "value equals to the default value '' (such intervals should be dropped). Index = 1"
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 0d),
          "a" forAll (x > 0d)
        ),
        "Invalid sequence of interval relations {{x <= 0.0} -> a, {x > 0.0} -> a}: " +
        "adjacent intervals must have different values. Index = 1"
      )
    )
  }

  object continuousBounded {

    type E = Double
    type V = String
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

    val factory: OrderedMapFactory[E, Dom, V, OrderedMap[E, Dom, V]]#Provided = 
      TreapOrderedMap.getFactory[E, Dom, V].provided

    val x: BoundBuilder[E, Dom] = BoundBuilder(domainOps)

    val y: BoundBuilder[E, Dom] = BoundBuilder(otherDomainOps)

    val testCases = List[TestCase[E, Dom, V]](
      SuccessCase(
        "",
        List(),
        factory.buildUniform("")
      ),
      SuccessCase(
        "",
        List(
          "a" forAll x
        ),
        factory.buildUniform("a")
      ),
      SuccessCase(
        "",
        List(
          "a" forAll x < 1
        ),
        factory.unsafeBuild(
          List(
            (1d`)`, "a"),
            (AboveAll, "")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10d & x <= 20d),
          "b" forAll (x > 20d & x < 30d)
        ),
        factory.unsafeBuild(
          List(
            (10d`]`, ""),
            (20d`]`, "a"),
            (30d`)`, "b"),
            (AboveAll, "")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10d & x < 20d),
          "a" forAll (x > 20d)
        ),
        factory.unsafeBuild(
          List(
            (10d`]`, ""),
            (20d`)`, "a"),
            (20d`]`, ""),
            (AboveAll, "a")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10d & x <= 20d),
          "b" forAll (x > 20d & x < 30d),
          "c" forAll (x >= 40d)
        ),
        factory.unsafeBuild(
          List(
            (10d`]`, ""),
            (20d`]`, "a"),
            (30d`)`, "b"),
            (40d`)`, ""),
            (AboveAll, "c")
          )
        )
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 0d),
          "b" forAll (x > 50d & x <= 130d)
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 5d),
          "b" forAll (x > 2d & x <= 20d),
        ),
        "Invalid sequence of interval relations {{0.0 < x <= 5.0} -> a, {2.0 < x <= 20.0} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 5d),
          "b" forAll (y > 90d & y <= 150d),
        ),
        "Invalid interval relation {90.0 < x <= 150.0} -> b: " +
        "upper bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 1"
      ),
      FailureCase(
        "",
        List(
          "a" forAll x,
          "b" forAll (y > 90d & y <= 150d),
        ),
        "Invalid interval relation {90.0 < x <= 150.0} -> b: " +
        "upper bound of interval is out of domain bounds [x > 0.0, x <= 100.0]. Index = 1"
      )
    )
  }

  object discreteUnbounded {

    type E = BigInt
    type V = String
    type Dom[X] = Domain.DiscreteUnbounded[X]

    implicit val domainOps: DomainOps[E, Dom] =
      DomainOps.UnboundedOps.default(
        Domain.DiscreteUnbounded.default(new bigInt.NaturalOrder), 
        bigIntShow
      )

    val factory: OrderedMapFactory[E, Dom, V, OrderedMap[E, Dom, V]]#Provided = 
      TreapOrderedMap.getFactory[E, Dom, V].provided

    val x: BoundBuilder[E, Dom] = BoundBuilder(domainOps)

    val testCases = List[TestCase[E, Dom, V]](
      SuccessCase(
        "",
        List(),
        factory.buildUniform("")
      ),
      SuccessCase(
        "",
        List(
          "a" forAll x
        ),
        factory.buildUniform("a")
      ),
      SuccessCase(
        "",
        List(
          "a" forAll x < 1
        ),
        factory.unsafeBuild(
          List(
            (BigInt(1)`)`, "a"),
            (AboveAll, "")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10 & x <= 20),
          "b" forAll (x >= 21 & x < 30)
        ),
        factory.unsafeBuild(
          List(
            (BigInt(10)`]`, ""),
            (BigInt(20)`]`, "a"),
            (BigInt(30)`)`, "b"),
            (AboveAll, "")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10 & x < 20),
          "a" forAll (x > 20)
        ),
        factory.unsafeBuild(
          List(
            (BigInt(10)`]`, ""),
            (BigInt(20)`)`, "a"),
            (BigInt(20)`]`, ""),
            (AboveAll, "a")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10 & x <= 20),
          "b" forAll (x >= 21 & x < 30),
          "c" forAll (x >= 40)
        ),
        factory.unsafeBuild(
          List(
            (BigInt(10)`]`, ""),
            (BigInt(20)`]`, "a"),
            (BigInt(30)`)`, "b"),
            (BigInt(40)`)`, ""),
            (AboveAll, "c")
          )
        )
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 0),
          "" forAll (x > 10 & x < 10)
        ),
        "Invalid interval relation {} -> : interval must be non-empty. Index = 1"
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 0),
          "" forAll (x > 10 & x <= 20)
        ),
        "Invalid interval relation {10 < x <= 20} -> : " +
        "value equals to the default value '' (such intervals should be dropped). Index = 1"
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 0),
          "a" forAll (x >= 1)
        ),
        "Invalid sequence of interval relations {{x <= 0} -> a, {x >= 1} -> a}: " +
        "adjacent intervals must have different values. Index = 1"
      )
    )
  }

  object discreteBounded {

    type E = Int
    type V = String
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

    val factory: OrderedMapFactory[E, Dom, V, OrderedMap[E, Dom, V]]#Provided = 
      TreapOrderedMap.getFactory[E, Dom, V].provided

    val x: BoundBuilder[E, Dom] = BoundBuilder(domainOps)

    val y: BoundBuilder[E, Dom] = BoundBuilder(otherDomainOps)

    val testCases = List[TestCase[E, Dom, V]](
      SuccessCase(
        "",
        List(),
        factory.buildUniform("")
      ),
      SuccessCase(
        "",
        List(
          "a" forAll x
        ),
        factory.buildUniform("a")
      ),
      SuccessCase(
        "",
        List(
          "a" forAll x < 1
        ),
        factory.unsafeBuild(
          List(
            (1`)`, "a"),
            (AboveAll, "")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10 & x <= 20),
          "b" forAll (x >= 21 & x < 30)
        ),
        factory.unsafeBuild(
          List(
            (10`]`, ""),
            (20`]`, "a"),
            (30`)`, "b"),
            (AboveAll, "")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10 & x < 20),
          "a" forAll (x > 20)
        ),
        factory.unsafeBuild(
          List(
            (10`]`, ""),
            (20`)`, "a"),
            (20`]`, ""),
            (AboveAll, "a")
          )
        )
      ),
      SuccessCase(
        "",
        List(
          "a" forAll (x > 10 & x <= 20),
          "b" forAll (x >= 21 & x < 30),
          "c" forAll (x >= 40)
        ),
        factory.unsafeBuild(
          List(
            (10`]`, ""),
            (20`]`, "a"),
            (30`)`, "b"),
            (40`)`, ""),
            (AboveAll, "c")
          )
        )
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x < 0),
          "b" forAll (x > 50 & x <= 130)
        ),
        "Invalid interval relation {} -> a: interval must be non-empty. Index = 0"
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 5),
          "b" forAll (x > 4 & x <= 20),
        ),
        "Invalid sequence of interval relations {{0 <= x <= 5} -> a, {4 < x <= 20} -> b}: " +
        "intervals must not overlap. Index = 1"
      ),
      FailureCase(
        "",
        List(
          "a" forAll (x <= 5),
          "b" forAll (y > 90 & y <= 150),
        ),
        "Invalid interval relation {90 < x <= 150} -> b: " +
        "upper bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 1"
      ),
      FailureCase(
        "",
        List(
          "a" forAll x,
          "b" forAll (y > 90 & y <= 150),
        ),
        "Invalid interval relation {90 < x <= 150} -> b: " +
        "upper bound of interval is out of domain bounds [x >= 0, x <= 100]. Index = 1"
      )
    )
  }
}
