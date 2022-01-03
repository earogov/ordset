package ordset.test.core.behaviors.lazyTreapSeq

import ordset.core.interval.IntervalRelation
import ordset.core.domain.Domain
import ordset.util.label.Label
import ordset.test.core.behaviors.{TestPackageBase, TestShowUtil}
import ordset.core.ExtendedBound

trait LazyTreapSeqMultipleTakeTest[E, D <: Domain[E], V] {

  def multipleTakeCases: Iterable[LazyTreapSeqMultipleTakeTest.TestPackage[E, D, V]]
}

object LazyTreapSeqMultipleTakeTest {

  sealed trait TestCommand[E, D <: Domain[E], V]

  case class TakeAboveCommand[E, D <: Domain[E], V](
    val bound: ExtendedBound[E]
  ) extends TestCommand[E, D, V] {

    override def toString: String = TestShowUtil.namedCaseWithBoundToString("takeAboveCommand", bound)
  }

  case class TakeBelowCommand[E, D <: Domain[E], V](
    val bound: ExtendedBound[E]
  ) extends TestCommand[E, D, V] {

    override def toString: String = TestShowUtil.namedCaseWithBoundToString("takeBelowCommand", bound)
  }

  case class Validation[E, D <: Domain[E], V](
    val expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCommand[E, D, V] {

    override def toString: String = "validation"
  }

  case class TestPackage[E, D <: Domain[E], V](
    override val labels: Set[Label],
    commands: Iterable[TestCommand[E, D, V]]
  ) extends TestPackageBase(labels)
}