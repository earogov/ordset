package ordset

import ordset.domain.{Domain, DomainOps}

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

trait SegmentLike[@sp(spNum) E, D <: Domain[E], @sp(Boolean) +V] {

  def domainOps: DomainOps[E, D]

  def value: V

  def hasNext: Boolean = false

  def hasPrev: Boolean = false

  def hasUpperBound: Boolean = false

  def hasLowerBound: Boolean = false

  def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

  def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

  def isFirst: Boolean = false

  def isLast: Boolean = false

  def isInner: Boolean = false

  def isSingle: Boolean = false

  def isInitial: Boolean = false

  def isTerminal: Boolean = false

  def moveToFirst: Segment.First[E, D, V]

  def moveToLast: Segment.Last[E, D, V]

  def moveTo(bound: Bound[E]): Segment[E, D, V]
}