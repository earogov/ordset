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
      )
    )
  }
}
