package ordset.test.core.behaviors.segmentSeq.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.test.core.behaviors.segmentSeq.SetBuilderTest.*
import org.scalatest.funspec.AnyFunSpec
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import scala.language.postfixOps
import ordset.test.core.behaviors.segmentSeq.SetBuilderTest
import ordset.core.segmentSeq.set.{TreapOrderedSet, OrderedSetFactory, OrderedSet}
import org.scalactic.Fail

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
  }

  object continuousUnbounded {

    type E = Double
    type Dom[X] = Domain.ContinuousUnbounded[X]

    implicit val domainOps: DomainOps[E, Dom] = DomainOps.UnboundedOps.default
    
    val factory: OrderedSetFactory[E, Dom, OrderedSet[E, Dom]]#Partial = TreapOrderedSet.getFactory[E, Dom].provided
    
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
      )
    )
  }
}
