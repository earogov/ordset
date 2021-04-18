package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

/**
 * [[SegmentSeq]] encodes ordered sets and maps of elements, such that^*1^:
 * <tr>                                                                                     </tr>
 * <tr>1. {(-2, -1), [1], (5, 10]} - set of integers                                        </tr>
 * <tr>                                                                                     </tr>
 * <tr>2. {(-2.0, 0.0), (0.0, 10.0]} - set of real numbers                                  </tr>
 * <tr>                                                                                     </tr>
 * <tr>3. {(2020-01-01, 2020-12-31], [2021-01-11, +inf)} - set of dates without upper bound </tr>
 * <tr>                                                                                     </tr>
 * <tr>4. {(-inf, 0) -> B, [10, 20] -> A} - map integer -> string                           </tr>
 * <tr>                                                                                     </tr>
 * <tr>5. {(-inf, +inf) -> Z} - universal or unbounded map                                  </tr>
 * <tr>                                                                                     </tr>
 * <tr>
 *   *1 - Default interval notation is used for inclusive/exclusive bounds.
 * </tr>
 * <tr></tr>
 *
 * We can not explicitly enumerate all elements as for standard (unordered) sets and maps. Instead we describe ordered
 * map as a sequence of segments - intervals of elements of type `E` with some value `W`. Order for type `E` is defined
 * by [[DomainOps]].
 * <tr></tr>
 * 
 * Such objects as traditional empty maps (without any keys and values) and {{} -> Value} which maps empty set to some
 * value can not be represented by [[SegmentSeq]]. Generally segment sequence is defined as
 * <tr></tr>
 * 
 * {(l,,i,,, u,,i,,) -> v,,i,,} for i ∈ [1, N]
 * <tr>where</tr>
 * <tr>l,,i,, - lower bound of segment i and l,,1,, is the minimal bound of domain;</tr>
 * <tr>u,,i,, - upper bound of segment i and u,,N,, is the maximal bound of domain;</tr>
 * <tr>v,,i,, - value of segment i;</tr>
 * <tr>N - number of segment in sequence.</tr>
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
 * belong to set. Consider example 4:
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
 * <tr></tr>
 *
 * <h1>Notes</h1>
 *
 * 1. In all ordering relations of bounds (like bound1 `>` bound2 etc.) we assume:
 * <tr>
 *   - upper bound of last segment has maximal value in domain (for unbounded domain it's equivalent to plus infinity);   
 * </tr>
 * <tr>
 *   - lower bound of first segment has minimal value in domain (for unbounded domain it's equivalent to minus infinity). 
 * </tr>
 * <tr>
 * These properties MUST be provided by implementations of [[DomainOps.segmentUpperOrd]] and [[DomainOps.segmentLowerOrd]].
 * </tr>
 * <tr></tr>
 * 
 * @tparam E type of element in ordered set
 * @tparam D type of domain
 * @tparam V type of value assigned to interval
 */
trait SegmentSeq[@sp(spNum) E, D <: Domain[E], @sp(Boolean) W] {

  /** Domain operations. */
  implicit def domainOps: DomainOps[E, D]

  /** Value operations (equality type class, etc). */
  implicit def valueOps: ValueOps[W]

  /** Random numbers generator. */
  implicit def rngManager: RngManager

  // Inspection --------------------------------------------------------------- //
  /** @return `true` if sequence is empty i.e. contains no elements. */
  def isEmpty: Boolean

  /** @return `true` if sequence is universal i.e. contains all elements of domain. */
  def isUniversal: Boolean

  /**
   * @return `true` if sequence is empty or universal, i.e has single segment.
   */
  def isUniform: Boolean

  /**
   * @return `true` if sequence represents ordered set, i.e. `W` is `Boolean`. 
   */
  def isSet: Boolean
  
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
   * Returns sequence containing
   * <tr>- segment (minBound, u,,1,,) -> v,,1,,</tr>
   * <tr>- segments {i > 1: (l,,i,, u,,i,,,) -> v,,i,,} of original sequence for which l,,i,, `>` u,,1,,</tr> 
   * <tr>where</tr>
   * <tr>minBound - minimal bound of domain;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>
   *   S,,1,, - segment of original sequence for which l,,1,, `≤` `bound` and u,,1,, `≥` `bound`;
   * </tr>
   * {{{
   * Example 1
   * 
   * original:
   *                 bound
   *                   )
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takenAbove(bound):
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * {{{
   * Example 2
   *
   * original:
   *                     bound
   *                       )
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takenAbove(bound):
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.takenAbove(bound) == sequence.getSegment(bound).takenAbove 
   *   for any bound
   *   
   *   2. sequence == sequence.takenBelow(bound).appended(bound, sequence.takenAbove(bound)) 
   *   for any bound
   * }}}
   */
  def takenAbove(bound: Bound[E]): SegmentSeq[E, D, W]

  /**
   * Returns sequence containing
   * <tr>- segments {i ∈ [1, N-1]: (l,,i,, u,,i,,,) -> v,,i,,} of original sequence for which u,,i,, `<` l,,N,,</tr>
   * <tr>- segment (l,,N,,, maxBound) -> v,,N,,</tr>
   * <tr>where</tr>
   * <tr>maxBound - maximal bound of domain;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>
   *   S,,N,, - segment of original sequence for which l,,N,, `≤` `bound` and u,,N,, `≥` `bound`;
   * </tr>
   * {{{
   * Example 1
   *
   * original:
   *                 bound
   *                   )
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takenBelow(bound):
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   * }}}
   * {{{
   * Example 2
   *
   * original:
   *            bound
   *             (
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takenBelow(bound):
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.takenBelow(bound) == sequence.getSegment(bound).takenBelow 
   *   for any bound
   *   
   *   2. sequence == sequence.takenBelow(bound).appended(bound, sequence.takenAbove(bound)) 
   *   for any bound
   * }}}
   */
   def takenBelow(bound: Bound[E]): SegmentSeq[E, D, W]

  /**
   * Returns tuple of sequences: ([[takenBelow]], [[takenAbove]]).
   *
   * {{{
   * original:
   *                 bound
   *                   )
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
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.sliced(bound) == sequence.getSegment(bound).sliced 
   *   for any bound
   *   
   *   2. sequence == sequence.sliced(bound)._1.appended(bound, sequence.sliced(bound)._2) 
   *   for any bound
   * }}}
   */
  def sliced(bound: Bound[E]): (SegmentSeq[E, D, W], SegmentSeq[E, D, W])
  
  /**
   * Returns sequence containing:
   * <tr>- upper bounds of all original segments except last;</tr>
   * <tr>- upper bounds of `other` sequence that satisfy condition:</tr>
   * {{{
   * upper bound > lower bound of original's last segment
   * }}}
   * Each upper bound brings to the output sequence value that was associated with it
   * in initial sequence (original or `other`).
   * {{{
   *
   * original:
   *                                 last
   *                                  v
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
   * }}}
   * Methods definitions provide invariant:
   * {{{
   *   original == original.takenBelow(bound).appended(original.takenAbove(bound))
   * }}}
   */
  def appended(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W]

  /**
   * Returns sequence containing:
   * <tr>- segments {(l,,i,, min(u,,i,,, U(`bound`))) -> v,,i,,} of original sequence for which l,,i,, `<` `bound`; </tr>
   * <tr>- segments {(max(l,,i,,, L(`bound`)), u,,i,,) -> v,,i,,} of `other` sequence for which u,,i,, `>` `bound`; </tr>
   * <tr>where</tr>
   * <tr>l,,i,, - lower bound of segment i in sequence;</tr>
   * <tr>u,,i,, - upper bound of segment i in sequence;</tr>
   * <tr>v,,i,, - value of segment i in sequence;      </tr>
   * <tr>
   *   U - upper bound operator, it acts as identity if bound is upper and flips bound otherwise 
   *   (see [[Bound.provideUpper]]);
   * </tr>
   * <tr>
   *   L - lower bound operator, it acts as identity if bound is lower and flips bound otherwise 
   *   (see [[Bound.provideLower]]).
   * </tr>
   *
   * {{{
   * Example 1
   * 
   * original:
   *                        bound
   *                         [
   *   X--------](-----------------)[-----------X
   *        A              B               C       - values
   *
   * other:
   *
   *   X--------------)[-------------](--------X
   *           C               D           E       - values
   *           
   * original.appended(bound, other):
   * 
   *                       bound
   *                         v
   *   X--------](----------)[-------)[---------X
   *        A          B         D         E      - values
   * }}}
   * {{{
   * Example 2
   *
   * original:
   *                             bound
   *                               )
   *   X--------](-----------------)[-----------X
   *        A              B               C       - values
   *
   * other:
   *
   *   X--------------)[----------------](-----X
   *           C               D            E      - values
   *
   * original.appended(bound, other):
   *
   *                             bound
   *                               v
   *   X--------](-----------------)[---](-----X
   *        A              B          D     E     - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence == sequence.takenBelow(bound).appended(bound, sequence.takenAbove(bound)) 
   *   for any bound
   * }}}
   */
  def appended(bound: Bound[E], other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W]
  
  override def toString: String =
    SetBuilderFormat.segmentSeq(this, (e: E) => e.toString, (v: W) => v.toString)
}
