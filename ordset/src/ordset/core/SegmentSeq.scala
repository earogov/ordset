package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

/**
 * [[SegmentSeq]] is intended to encode ordered sets and maps of elements, such that^*1^:
 * <tr>                                                                                     </tr>
 * <tr>1. {(-2, -1), [1], (5, 10]} - simple set of integers                                 </tr>
 * <tr>                                                                                     </tr>
 * <tr>2. {(2020-01-01, 2020-12-31], [2021-01-11, +inf)} - set of dates without upper bound </tr>
 * <tr>                                                                                     </tr>
 * <tr>3. {(-inf, 0) -> B, [10, 20] -> A} - map integer -> string                           </tr>
 * <tr>                                                                                     </tr>
 * <tr>4. {(-inf, +inf) -> Z} - universal or unbounded map                                  </tr>
 * <tr>                                                                                     </tr>
 * <tr>
 *   *1 - Default interval notation is used for inclusive/exclusive bounds.
 * </tr>
 * <tr></tr>
 *
 * We don't have here traditional empty map without any keys and values and even {{} -> Value} which maps
 * empty set to same value. Such objects can not be represented by [[SegmentSeq]].
 * <tr></tr>
 *
 * We can not directly enumerate all elements as for standard (unordered) sets and maps. Instead we describe them
 * as a sequence of segments - intervals of elements of type `E` with some value `W`. Order for type `E` is defined
 * by [[DomainOps]].
 * <tr></tr>
 *
 * All implementations of [[SegmentSeq]] MUST provide basic property:
 * <tr>1. <u>segments cover universal set without gaps and overlapping</u>.                 </tr>
 * <tr>                                                                                     </tr>
 * Also for performance reasons implementations SHOULD provide property:
 * <tr>2. <u>adjacent segment have different values</u>.                                    </tr>
 * <tr>                                                                                     </tr>
 *
 * To define ordered set we assume `W` = `Boolean` and consider it as a 'inclusion in set' flag.
 * Then example 1 will look like:
 * {{{
 *
 *                                         Segment 3
 *   Segment 0     Segment 1     Segment 2    |     Segment 4   Segment 5   Segment 6
 *      v             v             v         v        v           v           v
 *             -2            -1               1              5         10
 * X-----------](------------)[-------------)[](------------](--------](----------X
 * ^    false        true          false    true    false       true       false  ^
 * |                                  ^                                           |
 * Unbounded               Value `W` (inclusion in set)                   Unbounded
 * }}}
 *
 * To define map to some type `V` one may accept `W` = `Option[V]`. Where `None` corresponds to segments that don't
 * belong to set. Consider example 3:
 * {{{
 *
 *      Segment 0           Segment 1          Segment 2             Segment 3
 *         v         0         v       10         v        20           v
 * X----------------)[-----------------)[------------------](---------------------X
 *      Some(B)             None              Some(A)                 None
 *                                              ^                       ^
 *         Value `W` (segment is included in set with value `A`)        |
 *                                                                  not included
 * }}}
 *
 * Because of segments are follow without gaps and overlapping it's enough to keep either upper or lower bound.
 * Generally upper bounds are stored and lower bounds are computed (by upper bounds of previous segments).
 * Therefore transformation operations are defined in terms of upper bounds.
 * <tr></tr>
 *
 * Note that in all ordering relations of bounds (like bound1 `>` bound2 etc.) we assume:
 * <tr>- upper bound of last segment has maximal value (equivalent to plus infinity);   </tr>
 * <tr>- lower bound of first segment has minimal value (equivalent to minus infinity). </tr>
 * <tr>
 * These properties MUST be provided by implementations of [[DomainOps.segmentUpperOrd]] and
 * [[DomainOps.segmentLowerOrd]].
 * </tr>
 */
trait SegmentSeq[@sp(spNum) E, D <: Domain[E], @sp(Boolean) W] {

  implicit def domainOps: DomainOps[E, D]

  implicit def rngManager: RngManager

  // Inspection --------------------------------------------------------------- //
  /** @return `true` if sequence is empty i.e. contains no elements. */
  def isEmpty: Boolean

  /** @return `true` if sequence is universal i.e. contains all elements of domain. */
  def isUniversal: Boolean

  /**
   * @return `true` sequence is empty or universal, i.e has single segment.
   */
  def isUniform: Boolean

  /** @return `true` if sequence contains `bound`. */
  def contains(bound: Bound[E]): Boolean

  /** @return `true` if sequence contains `element`. */
  def contains(element: E): Boolean = contains(Bound.Upper.inclusive(element))

  // Navigation --------------------------------------------------------------- //
  /**
   * @return collection of all upper bounds.
   */
  def upperBounds: Iterable[Bound.Upper[E]]

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
   * Returns sequence containing upper bounds of segments that satisfy condition:
   * {{{
   * upper bound >= specified bound
   * }}}
   * Each upper bound brings to the output sequence value that was associated with it in original sequence.
   * {{{
   *
   * original:
   *                     bound
   *                       v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takenAbove(bound):
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   */
  def takenAbove(bound: Bound[E]): SegmentSeq[E, D, W]

  /**
   * Returns sequence containing upper bounds of segments that satisfy condition:
   * {{{
   * upper bound < specified bound
   * }}}
   * Each upper bound brings to the output sequence value that was associated with it in original sequence.
   * {{{
   *
   * original:
   *                     bound
   *                       v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takenBelow(bound):
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   * }}}
   */
   def takenBelow(bound: Bound[E]): SegmentSeq[E, D, W]

  /**
   * Returns tuple of sequences: ([[takenBelow]], [[takenAbove]]).
   *
   * {{{
   * original:
   *                     bound
   *                       v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.sliced(bound)._1:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   *
   * original.sliced(bound)._2:
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   */
  def sliced(bound: Bound[E]): (SegmentSeq[E, D, W], SegmentSeq[E, D, W])

  /**
   * Returns sequence containing:
   * <tr>- upper bounds of all original segments except last;</tr>
   * <tr>- upper bounds of `other` sequence that satisfy condition:
   * {{{
   * upper bound > upper bound of original's penultimate segment
   * }}}
   * Each upper bound brings to the output sequence value that was associated with it
   * in initial sequence (original or `other`).
   * <tr>
   * If original sequence does not have penultimate segment (sequence with single segment)
   * then output sequence is equals to `other` sequence, i.e. all `other`'s upper bounds
   * with its values should be moved to the output sequence.
   * </tr>
   * {{{
   *
   * original:
   *              penultimate        last
   *                   v              v
   *   X--------](---------)[------------------X
   *        A         B             C              - values
   *
   * other:
   *
   *   X--------------)[-------------](--------X
   *           D             E            F        - values
   *
   * original.appended(other):
   *
   *   X--------](---------)[--------](--------X
   *        A         B          E        F        - values
   *
   *
   * Methods definitions provide invariant:
   * {{{
   *   original == original.takenBelow(bound) appended original.takenAbove(bound)
   * }}}
   */
  def appended(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W]

  override def toString: String =
    SetBuilderFormat.segmentSeq(this, (e: E) => e.toString, (v: W) => v.toString)
}
