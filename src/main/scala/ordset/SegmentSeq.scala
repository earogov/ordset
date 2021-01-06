package ordset

import ordset.domain.{Domain, DomainOps}

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

/**
 * Representation of ordered set of elements with identifier of type `E`, for example:
 * <tr>{(..., -1), [1], (5, 10]}                      </tr>
 * <tr>{(2020-01-01, 2020-12-31], [2021-01-11, ...)}  </tr>
 * <tr>                                               </tr>
 *
 * It's encoded by a sequence of segments which covers universal set <u>without gaps and overlapping</u>.
 * `Boolean` value is assigned to each segment indicating whether it belongs to set:
 * {{{
 *
 *    Segment 0       Segment 1       Segment 2   - segment index
 * X-------------|-----------------|------------X
 *     false            true           false      - belongs to set
 *
 * * where X---| and |---X are unbounded segments.
 * }}}
 * Example with integers above will look like:
 *
 * {{{
 *                        Segment 2
 *   Segment 0    Segment 1  |    Segment 3    Segment 4  Segment 5
 *      v            v       v       v            v          v
 *            -1             1              5         10
 * X----------)[-----------)[](------------](--------](----------X
 *     true       false    true    false       true      false
 * }}}
 * Because of segments are follow without gaps and overlapping it's enough to keep either upper or lower bound.
 * Generally upper bounds are stored and lower bounds are computed by upper bound of previous segment.
 * Therefore transformation operations are defined in terms of upper bounds.
 * <tr></tr>
 *
 * In all ordering relations of bounds (like bound1 > bound2 etc.) we assume that:
 * <tr>- upper bound of last segment has maximal value (equivalent to plus infinity);   </tr>
 * <tr>- lower bound of first segment has minimal value (equivalent to minus infinity). </tr>
 * <tr>
 * These properties MUST be provided by implementations of [[DomainOps.segmentUpperOrd]] and
 * [[DomainOps.segmentLowerOrd]].
 * </tr>
 * <tr></tr>
 *
 * Type `W` represents some value which is associated with each segment.
 * To define ordered set of elements we consider this value as 'belongs to set' indicator (`W` = `Boolean`).
 * To define ordered map to some type `V` (`E` -> `V`) we can assume `W` = `Option[V]`. Where `None` corresponds to
 * segments that don't belong to set.
 */
trait SegmentSeq[@sp(spNum) E, D <: Domain[E], @sp(Boolean) W] {

  implicit def domainOps: DomainOps[E, D]

  // Inspection --------------------------------------------------------------- //
  /** @return true if sequence is empty i.e. contains no elements. */
  def isEmpty: Boolean

  /** @return true if sequence is universal i.e. contains all elements of domain. */
  def isUniversal: Boolean

  /** @return true if sequence contains `bound`. */
  def contains(bound: Bound[E]): Boolean

  /** @return true if sequence contains `element`. */
  def contains(element: E): Boolean = contains(Bound.Upper.inclusive(element))

  // Navigation --------------------------------------------------------------- //
  /** @return first segment of sequence. */
  def firstSegment: Segment.First[E, D, W]

  /** @return last segment of sequence. */
  def lastSegment: Segment.Last[E, D, W]

  /** @return segment which contains specified `bound`. */
  def getSegment(bound: Bound[E]): Segment[E, D, W]

  /** @return segment which contains specified `element`. */
  def getSegment(element: E): Segment[E, D, W] = getSegment(Bound.Upper.inclusive(element))

  // Transformation ----------------------------------------------------------- //
  /**
   * Return sequence containing only those segments of original sequence that satisfy condition:
   * {{{
   * upper bound of segment > specified bound
   * }}}
   * First value of output sequence equals to the value of original segment closest to `bound` and satisfying condition:
   * {{{
   * upper bound of segment > specified bound
   * }}}
   * {{{
   * original:
   *                    bound
   *                      v
   *   X--------|---------|--------|---------X
   *        A       B         C         D          - values
   *
   * original.droppedBelow(bound):
   *
   *   X---------------------------|---------X
   *                          C         D          - values
   * }}}
   */
  def droppedBelow(bound: Bound[E]): SegmentSeq[E, D, W]

  /**
   * Returns sequence containing only those segments of original sequence that satisfy condition:
   * {{{
   * upper bound of segment <= specified bound
   * }}}
   * Last value of output sequence equals to the value of original segment closest to `bound` and satisfying condition:
   * {{{
   * upper bound of segment > specified bound
   * }}}
   * {{{
   * original:
   *                    bound
   *                      v
   *   X--------|---------|--------|---------X
   *        A       B         C         D          - values
   *
   * original.droppedAbove(bound):
   *
   *   X--------|---------|------------------X
   *        A       B         C                    - values
   * }}}
   */
   def droppedAbove(bound: Bound[E]): SegmentSeq[E, D, W]

  /**
   * @return tuple of sequences: ([[droppedAbove]], [[droppedBelow]]).
   */
  def slice(bound: Bound[E]): (SegmentSeq[E, D, W], SegmentSeq[E, D, W])

  /**
   * Returns sequence containing:
   * <tr>- all segments of original sequence except last; </tr>
   * <tr>- segments of other sequence that satisfy condition:
   * {{{
   * upper bound of other segment > lower bound of original last segment
   * }}}
   * {{{
   * original:
   *
   *
   *   X--------|---------|------------------X
   *        A        B             C               - values
   *
   * other:
   *
   *   X--------------|-------------|--------X
   *           D             E          F          - values
   *
   * original.appended(other):
   *
   *   X--------|---------|---------|--------X
   *        A        B         E        F          - values
   *
   * }}}
   */
  def appended(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W]
}
