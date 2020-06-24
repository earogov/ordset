package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

trait SegmentLike[@sp(spNum) E, +D <: Domain[E], @sp(Boolean) +V] {

  def domain: D

  def value: V

  def hasNext: Boolean = false

  def hasPrev: Boolean = false

  def isFirst: Boolean = false

  def isLast: Boolean = false

  def isInner: Boolean = false

  def isSingle: Boolean = false

  def isInitial: Boolean = false

  def isTerminal: Boolean = false

  def moveToFirst: Segment[E, D, V]

  def moveToLast: Segment[E, D, V]

  def moveTo(bound: Bound[E]): Segment[E, D, V]
}