package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

trait SegmentLike[@sp(spNum) E, D <: Domain[E], @sp(Boolean) V] {

  def sequence: SegmentSeq[E, D, V]
  
  def domainOps: DomainOps[E, D]

  def valueOps: ValueOps[V]
  
  def value: V

  def isIncluded: Boolean

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