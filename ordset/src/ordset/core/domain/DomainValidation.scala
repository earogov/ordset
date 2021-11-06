package ordset.core.domain

import ordset.core.interval.{Interval, IntervalBuilder}
import scala.util.{Try, Failure, Success}

object DomainValidation {

  def validateBounds[E](d: DomainLike[E]): Try[Unit] = 
    if (d.extendedOrd.gt(d.lowerExtendedBound, d.upperExtendedBound)) 
      Failure(
        new IllegalArgumentException(
          s"Invalid domain $d: lower bound ${d.lowerExtendedBound} is greater than upper bound ${d.upperExtendedBound}."
        )
      )
    else
      Success({})

  def tryBuildBounds[E, D <: Domain[E]](d: DomainLike[E], b: IntervalBuilder[E, D]): Try[Interval.NonEmpty[E, D]] =
    b.betweenExtended(d.lowerExtendedBound, d.upperExtendedBound) match {
      case i: Interval.NonEmpty[E, D] => Success(i)
      case _ => 
        Failure(
          new IllegalArgumentException(
            s"Domain $d is not compatible with interval builder: " +
            s"domain has lower bound ${d.lowerExtendedBound} and upper ${d.upperExtendedBound}, " +
            "interval builder returns empty interval for such bounds."
          )
        )
    }
}