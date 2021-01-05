package ordset

import ordset.domain.{Domain, DomainOps}

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

/**
  * Representation of ordered sequence of elements with identifier of type `E`.
  * It's encoded by a sequence of segments which covers universal set without gaps and overlapping.
  * `Boolean` value is assigned to each segment indicating whether it belongs to set:
  * {{{
 *
  *   Segment 0       Segment 1       Segment 2   - segment index
  * -------------|-----------------|------------
  *    false            true           false      - belongs to set
  * }}}
  * Type `W` represents some value which is associated with each segment.
  * To define ordered set of elements we should consider this value as 'belongs to set' indicator (`W` = `Boolean`).
  * To define ordered map to some type `V` (`E` -> `V`) we assume `W` = `Option[V]`. Where `None` corresponds to
  * segments that don't belong to set.
  */
trait SegmentSeq[@sp(spNum) E, D <: Domain[E], @sp(Boolean) W] {

  implicit def domainOps: DomainOps[E, D]

  /** @return true if sequence is empty i.e. contains no elements. */
  def isEmpty: Boolean

  /** @return true if sequence is universal i.e. contains all elements of domain. */
  def isUniversal: Boolean

  /** @return true if sequence contains `bound`. */
  def contains(bound: Bound[E]): Boolean

  /** @return true if sequence contains `element`. */
  def contains(element: E): Boolean = contains(Bound.Upper.inclusive(element))

  /** @return first segment of sequence. */
  def firstSegment: Segment.First[E, D, W]

  /** @return last segment of sequence. */
  def lastSegment: Segment.Last[E, D, W]

  /** @return segment which contains specified `bound`. */
  def getSegment(bound: Bound[E]): Segment[E, D, W]

  /** @return segment which contains specified `element`. */
  def getSegment(element: E): Segment[E, D, W] = getSegment(Bound.Upper.inclusive(element))
}
