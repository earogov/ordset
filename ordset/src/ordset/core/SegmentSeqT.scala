package ordset.core

import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{LazyTreapOrderedMap, MappedOrderedMap, MappedValueOrderedMap, TreapOrderedMap, UniformOrderedMap, ZippedOrderedMap}
import ordset.core.util.SegmentSeqUtil
import ordset.random.RngManager
import ordset.util.BooleanUtil

import scala.Specializable.AllNumeric as spNum
import scala.specialized as sp

/**
 * Segment sequence encodes ordered sets and maps of elements, such that^*1^:
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
 * map as a sequence of segments - intervals of elements of type `E` with some value `V`. Order for type `E` is defined
 * by [[DomainOps]].
 * <tr></tr>
 * 
 * Such objects as traditional empty maps (without any keys and values) and {{} -> Value} which maps empty set to some
 * value can not be represented by segment sequence. Generally it's defined as
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
 * All implementations of segment sequence MUST provide basic properties:
 * <tr>1. <u>segments cover universal set without gaps and overlapping</u>.                 </tr>
 * <tr>2. <u>adjacent segments have different values</u>.                                   </tr>
 * <tr>                                                                                     </tr>
 *
 * To define ordered set we assume `V` = `Boolean` and consider it as a 'inclusion in set' flag.
 * Then example 1 will the following:
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
 * To define map to some type `W` one may accept `V` = `Option[W]`. Where `None` corresponds to segments that don't
 * belong to set. Consider example 4:
 * {{{
 *
 *      Segment 0           Segment 1          Segment 2             Segment 3
 *         v         0         v       10         v        20           v
 * X----------------)[-----------------)[------------------](---------------------X
 *      Some(B)             None              Some(A)                 None
 *                                              ^                       ^
 *         Value `V` (segment is included in set with value `A`)        |
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
 * @tparam D type of elements domain
 * @tparam V type of value assigned to interval of elements
 * @tparam S type of additional segment state
 */
trait SegmentSeqT[@sp(spNum) E, D <: Domain[E], @sp(Boolean) V, +S] {

  // Inspection --------------------------------------------------------------- //
  /** Domain operations. */
  implicit def domainOps: DomainOps[E, D]

  /** Value operations (equality type class, etc). */
  implicit def valueOps: ValueOps[V]

  /** Random numbers generator. */
  implicit def rngManager: RngManager
  
  /** @return `true` if sequence is empty i.e. contains no elements. */
  def isEmpty: Boolean

  /** @return `true` if sequence is universal i.e. contains all elements of domain. */
  def isUniversal: Boolean

  /**
   * @return `true` if sequence is empty or universal, i.e has single segment.
   */
  def isUniform: Boolean
  
  /** @return `true` if `bound` is included in ordered set or map. */
  def includesBound(bound: Bound[E]): Boolean = getSegmentForBound(bound).isIncluded

  /** @return `true` if `bound` is included in ordered set or map. */
  def includesExtended(bound: ExtendedBound[E]): Boolean =
    bound match {
      case b: Bound[E] => includesBound(b)
      case ExtendedBound.BelowAll => firstSegment.isIncluded
      case ExtendedBound.AboveAll => lastSegment.isIncluded
    }

  /** @return `true` if `element` is included in ordered set or map. */
  def includesElement(element: E): Boolean = includesBound(Bound.Upper.inclusive(element))

  override def toString: String =
    SetBuilderFormat.segmentSeq(this, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[V])
  
  // Navigation --------------------------------------------------------------- //
  /**
   * @return collection of all upper bounds.
   */
  def upperBounds: Iterable[Bound.Upper[E]] = 
    SegmentSeqUtil.getUpperBoundsIterableFromSegment(firstSegment, inclusive = true)

  /**
   * @return collection of extended upper bounds. It's always non-empty with last element equals to 
   *         [[ExtendedBound.AboveAll]].
   */
  def extendedUpperBounds: Iterable[ExtendedBound.Upper[E]] = 
    firstSegment.forwardIterable.map(_.upperExtended)
  
  /** @return first segment of sequence. */
  def firstSegment: SegmentT.First[E, D, V, S] with S

  /** @return last segment of sequence. */
  def lastSegment: SegmentT.Last[E, D, V, S] with S

  /** @return segment that contains specified `bound`. */
  def getSegmentForBound(bound: Bound[E]): SegmentT[E, D, V, S] with S

  /** @return segment that contains specified `bound`. */
  def getSegmentForExtended(bound: ExtendedBound[E]): SegmentT[E, D, V, S] with S =
    bound match {
      case b: Bound[E] => getSegmentForBound(b)
      case ExtendedBound.BelowAll => firstSegment
      case ExtendedBound.AboveAll => lastSegment
    }

  /** @return segment that contains specified `element`. */
  def getSegmentForElement(element: E): SegmentT[E, D, V, S] with S = 
    getSegmentForBound(Bound.Upper.inclusive(element))

  /**
   * Returns value of segment that contains specified `bound`:
   * {{{
   *   getValueForBound(bound) == getSegmentForBound(bound).value
   *   for any bound
   * }}}
   */
  def getValueForBound(bound: Bound[E]): V

  /**
   * Returns value of segment that contains specified `bound`:
   * {{{
   *   getValueForExtended(bound) == getSegmentForExtended(bound).value
   *   for any bound
   * }}}
   */
  def getValueForExtended(bound: ExtendedBound[E]): V

  /**
   * Returns value of segment that contains specified `element`:
   * {{{
   *   getValueForElement(element) == getSegmentForElement(element).value
   *   for any element
   * }}}
   */
  def getValueForElement(element: E): V =
    getValueForBound(Bound.Upper.inclusive(element))

  // Transformation ----------------------------------------------------------- //
  /**
   * Returns sequence containing
   * <tr>- segment (minBound, u,,0,,) -> v,,0,,</tr>
   * <tr>- segments {i > 0: (l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which l,,i,, `>` u,,0,,</tr> 
   * <tr>where</tr>
   * <tr>minBound - minimal bound of domain;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>
   *   S,,0,, - segment of original sequence for which l,,0,, `≤` `bound` and u,,0,, `≥` `bound`;
   * </tr>
   *
   * <h3>Example 1</h3>
   * {{{
   * original:
   *                 bound
   *                   )
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takeAboveBound(bound):
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * <h3>Example 2</h3>
   * {{{
   * original:
   *                     bound
   *                       )
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takeAboveBound(bound):
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.takeAboveBound(bound) == sequence.getSegmentForBound(bound).takeAbove
   *   for any bound
   *   
   *   2. sequence == sequence.takeBelowBound(bound).appendAboveBound(bound, sequence.takeAboveBound(bound))
   *   for any bound
   *
   *   3. sequence == sequence.takeAboveBound(bound).prependBelowBound(bound, sequence.takeBelowBound(bound))
   *   for any bound
   * }}}
   */
  def takeAboveBound(bound: Bound[E]): SegmentSeq[E, D, V]

  /**
   * Adds support of unlimited bounds to [[takeAboveBound]]:
   * <tr>
   *   if `bound` is [[ExtendedBound.BelowAll]] returns current sequence;
   * </tr>
   * <tr>
   *   if `bound` is [[ExtendedBound.AboveAll]] returns uniform sequence with the value of last segment
   *   of current sequence;
   * </tr>
   * <tr>
   *   otherwise result is the same as for method [[takeAboveBound]].
   * </tr>
   */
  def takeAboveExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V]

  /**
   * Returns sequence containing
   * <tr>- segments {i ∈ [0, N-1]: (l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which u,,i,, `<` l,,N,,</tr>
   * <tr>- segment (l,,N,,, maxBound) -> v,,N,,</tr>
   * <tr>where</tr>
   * <tr>maxBound - maximal bound of domain;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>
   *   S,,N,, - segment of original sequence for which l,,N,, `≤` `bound` and u,,N,, `≥` `bound`;
   * </tr>
   *
   * <h3>Example 1</h3>
   * {{{
   * original:
   *                 bound
   *                   )
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takeBelowBound(bound):
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   * }}}
   * <h3>Example 2</h3>
   * {{{
   * original:
   *            bound
   *             (
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.takeBelowBound(bound):
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.takeBelowBound(bound) == sequence.getSegmentForBound(bound).takeBelow
   *   for any bound
   *
   *   2. sequence == sequence.takeBelowBound(bound).appendAboveBound(bound, sequence.takeAboveBound(bound))
   *   for any bound
   *
   *   3. sequence == sequence.takeAboveBound(bound).prependBelowBound(bound, sequence.takeBelowBound(bound))
   *   for any bound
   * }}}
   */
   def takeBelowBound(bound: Bound[E]): SegmentSeq[E, D, V]

  /**
   * Adds support of unlimited bounds to [[takeBelowBound]]:
   * <tr>
   *   if `bound` is [[ExtendedBound.BelowAll]] returns uniform sequence with the value of first segment
   *   of current sequence;
   * </tr>
   * <tr>
   *   if `bound` is [[ExtendedBound.AboveAll]] returns current sequence;
   * </tr>
   * <tr>
   *   otherwise result is the same as for method [[takeBelowBound]].
   * </tr>
   */
  def takeBelowExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V]

  /**
   * Returns tuple of sequences: ([[takeBelowBound]], [[takeAboveBound]]).
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *                 bound
   *                   )
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * original.sliceAtBound(bound)._1:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   *
   * original.sliceAtBound(bound)._2:
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.sliceAtBound(bound) == sequence.getSegmentForBound(bound).slice
   *   for any bound
   *   
   *   2. sequence == sequence.sliceAtBound(bound)._1.appendAboveBound(bound, sequence.sliceAtBound(bound)._2)
   *   for any bound
   *
   *   3. sequence == sequence.sliceAtBound(bound)._2.prependBelowBound(bound, sequence.sliceAtBound(bound)._1)
   *   for any bound
   * }}}
   */
  def sliceAtBound(bound: Bound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V])

  /**
   * Adds support of unlimited bounds to [[sliceAtBound]].
   *
   * Returns tuple of sequences: ([[takeBelowExtended]], [[takeAboveExtended]]).
   */
  def sliceAtExtended(bound: ExtendedBound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V])

  /**
   * Returns sequence containing:
   * <tr>
   *   - segments {i ∈ [0, M-1]: (l,,i,,, min(u,,i,,, originalFirstBound)) -> v,,i,,}
   *   of `other` sequence for which l,,i,, `≤` originalFirstBound
   * </tr>
   * <tr>
   *   - segments {i ∈ [M, N-1]: (l,,i,,, u,,i,,) -> v,,i,,}
   *   of original sequence for which l,,i,, `>` originalFirstBound
   * </tr>
   * <tr>where</tr>
   * <tr>originalFirstBound - upper bound of first segment of original sequence;</tr>
   * <tr>l,,i,, - lower bound of segment i in sequence;</tr>
   * <tr>u,,i,, - upper bound of segment i in sequence;</tr>
   * <tr>v,,i,, - value of segment i in sequence;      </tr>
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *
   *   X------------------------](-------------X
   *                A                   B          - values
   *
   * other:
   *
   *   X-------)[--------](--------------------X
   *       C         D              E              - values
   *
   * original.prepend(other):
   *
   *   X-------)[--------](-----](-------------X
   *       C        D        E          B          - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. If original sequence is not uniform then:
   *   sequence.prepend(other) == sequence.prependBelowBound(sequence.firstSegment.upperBound, other)
   *   for any `other` sequence
   *
   *   2. If original sequence is uniform then:
   *   sequence.prepend(other) == other
   * }}}
   */
  def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  /**
   * Returns sequence containing:
   * <tr>
   *   - segments {i ∈ [0, M-1]: (l,,i,,, min(u,,i,,, U(`bound`))) -> v,,i,,}
   *   of `other` sequence for which l,,i,, `<` `bound`;
   * </tr>
   * <tr>
   *   - segments {i ∈ [M, N-1]: (max(l,,i,,, L(`bound`)), u,,i,,) -> v,,i,,}
   *   of original sequence for which u,,i,, `>` `bound`;
   * </tr>
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
   * <h3>Example 1</h3>
   * {{{
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
   * original.prependBelowBound(bound, other):
   *
   *                       bound
   *                         v
   *   X--------------)[----)[-----)[-----------X
   *            C         D     B          C       - values
   * }}}
   * <h3>Example 2</h3>
   * {{{
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
   * original.prependBelowBound(bound, other):
   *
   *                             bound
   *                               v
   *   X--------------)[-----------)[-----------X
   *           C             D            C        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.prependBelowBound(lowerBound, other) == sequence.getSegmentForBound(lowerBound).prepend(other)
   *   for any lower bound in sequence, i.e. for `lowerBound` such that:
   *   sequence.getSegmentForBound(lowerBound).hasLowerBound(lowerBound) == true
   *
   *   2. sequence == sequence.takeAboveBound(bound).prependBelowBound(bound, sequence.takeBelowBound(bound))
   *   for any bound
   *
   *   3. sequence.prependBelowBound(bound, other) == sequence.prependBelowBound(bound.flip, other)
   *   for any bound and `other` sequence
   *   
   *   4. sequence.prependBelowBound(bound, other) == other.appendAboveBound(bound, sequence)
   *   for any bound and `other` sequence
   * }}}
   */
  def prependBelowBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  /**
   * Adds support of unlimited bounds to [[prependBelowBound]]:
   * <tr>if `bound` is [[ExtendedBound.BelowAll]] returns current sequence;</tr>
   * <tr>if `bound` is [[ExtendedBound.AboveAll]] returns `other` sequence;</tr>
   * <tr>otherwise result is the same as for method [[prependBelowBound]].</tr>
   */
  def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Returns sequence containing:
   * <tr>
   *   - segments {i ∈ [0, M-1]: (l,,i,,, u,,i,,) -> v,,i,,}
   *   of original sequence for which u,,i,, `<` originalLastBound;
   * </tr>
   * <tr>
   *   - segments {i ∈ [M, N-1]: (max(l,,i,,, originalLastBound), u,,i,,) -> v,,i,,}
   *   of `other` sequence for which u,,i,, `≥` originalLastBound;
   * </tr>
   * <tr>where</tr>
   * <tr>originalLastBound - lower bound of last segment of original sequence;</tr>
   * <tr>l,,i,, - lower bound of segment i in sequence;</tr>
   * <tr>u,,i,, - upper bound of segment i in sequence;</tr>
   * <tr>v,,i,, - value of segment i in sequence;      </tr>
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *
   *   X--------](------------------------------X
   *        A                    B                 - values
   *
   * other:
   *
   *   X----------------)[-----------](--------X
   *           C               D           E       - values
   *
   * original.append(other):
   *
   *   X--------](------)[-----------](--------X
   *        A        C         D           E       - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. If original sequence is not uniform then:
   *   sequence.append(other) == sequence.appendAboveBound(sequence.lastSegment.lowerBound, other)
   *   for any `other` sequence
   *
   *   2. If original sequence is uniform then:
   *   sequence.append(other) == other
   * }}}
   */
  def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  /**
   * Returns sequence containing:
   * <tr>
   *   - segments {i ∈ [0, M-1]: (l,,i,,, min(u,,i,,, U(`bound`))) -> v,,i,,}
   *   of original sequence for which l,,i,, `<` `bound`;
   * </tr>
   * <tr>
   *   - segments {i ∈ [M, N-1]: (max(l,,i,,, L(`bound`)), u,,i,,) -> v,,i,,}
   *   of `other` sequence for which u,,i,, `>` `bound`;
   * </tr>
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
   * <h3>Example 1</h3>
   * {{{
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
   * original.appendAboveBound(bound, other):
   * 
   *                       bound
   *                         v
   *   X--------](----------)[-------)[---------X
   *        A          B         D         E      - values
   * }}}
   * <h3>Example 2</h3>
   * {{{
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
   * original.appendAboveBound(bound, other):
   *
   *                             bound
   *                               v
   *   X--------](-----------------)[---](-----X
   *        A              B          D     E     - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.appendAboveBound(upperBound, other) == sequence.getSegmentForBound(upperBound).append(other)
   *   for any upper bound in sequence, i.e. for `upperBound` such that:
   *   sequence.getSegmentForBound(upperBound).hasUpperBound(upperBound) == true
   *   
   *   2. sequence == sequence.takeBelowBound(bound).appendAboveBound(bound, sequence.takeAboveBound(bound))
   *   for any bound
   *   
   *   3. sequence.appendAboveBound(bound, other) == sequence.appendAboveBound(bound.flip, other)
   *   for any bound and `other` sequence
   *
   *   4. sequence.appendAboveBound(bound, other) == other.prependBelowBound(bound, sequence)
   *   for any bound and `other` sequence
   * }}}
   */
  def appendAboveBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Adds support of unlimited bounds to [[appendAboveBound]]:
   * <tr>if `bound` is [[ExtendedBound.BelowAll]] returns `other` sequence;</tr>
   * <tr>if `bound` is [[ExtendedBound.AboveAll]] returns current sequence;</tr>
   * <tr>otherwise result is the same as for method [[appendAboveBound]].</tr>
   */
  def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Builds lazy segment sequence using original sequence (current) and `lazySeq`.
   * <tr>
   *   `lazySeq` is a segment sequence with optional lazy values (functions that returns another segment sequences).
   * </tr>
   * <tr></tr>
   * <tr>
   *   If segment of `lazySeq` has [[None]] value then corresponding segments of output sequence have the same
   *   values as `baseSeq`.
   * </tr>
   * <tr>
   *   If segment of `lazySeq` has [[Some]] value with a function F: `() => segmentSeqF`, then corresponding
   *   segments of output sequence are lazy. Function F will be computed only if lazy segment is requested.
   *   Values of lazy segments are completely defined by `segmentSeqF` and corresponding values of original
   *   sequence are ignored.
   * </tr>
   * {{{
   *
   * original:
   *
   * X------------](-------------)[-------------X
   *         A             B              C       - values
   *
   * `lazySeq`:
   *
   * X------](-------------------------)[-------X
   *   None     Some(() => orderedMapF)    None   - values
   *
   * `segmentSeqF`:
   * 
   * X---](---------](---------)[----------](---X
   *   D        E         F           G       H   - values
   *
   * output sequence after
   * computation of all lazy values:
   *
   * X------](------](---------)[------)[-------X
   *     A       E         F        G        C    - values
   * }}}
   */
  def patchLazy(lazySeq: SegmentSeq[E, D, OptionalSeqSupplier.Type[E, D, V]]): SegmentSeq[E, D, V]

  /**
   * Returns lazy segment sequence which is equivalent to one received by applying [[SegmentLikeT.flatMap]] to
   * each segment of original sequence (current) with map function:
   *
   * () `=>` mapFunc(S,,i,,)
   *
   * where S,,i,, - segment of original sequence.
   *
   * <h3>Example</h3>
   *
   * Assume `mapFunc` returns:
   * <tr>- seq1 for segment S1</tr>
   * <tr>- seq2 for segment S2</tr>
   * <tr>- seq3 for segment S3</tr>
   * {{{
   *
   * original:
   *
   *   X------------)[-------------)[------------X
   *         S1             S2            S3       - segments
   *
   * seq1:
   *
   *   X-----](----------------------------------X
   *      A                    B                   - values
   *
   * seq2:
   *
   *   X------------------)[---------------------X
   *             B                    C            - values
   *
   * seq3:
   *
   *   X---------------------------------)[------X
   *                     C                    D    - values
   *
   * output lazy sequence:
   *
   *   X------------)[-------------)[------------X
   *  () => mapFunc(S1)      |              |      - values
   *                  () => mapFunc(S2)     |
   *                                () => mapFunc(S3)
   *
   * output lazy sequence after
   * computation of all lazy values:
   *
   *   X-----](-----------)[-------------)[------X
   *      A          B            C           D    - values
   * }}}
   */
  def flatMap[U](
    mapFunc: SegmentT[E, D, V, S] => SegmentSeq[E, D, U]
  )(
    implicit valueOps: ValueOps[U]
  ): SegmentSeq[E, D, U] = {
    val lazySeq = TreapOrderedMap.getFactory.unsafeBuildAsc(
      firstSegment.forwardIterable.map(s => (s.upperExtended, Some(() => mapFunc(s)))),
      domainOps,
      OptionalSeqSupplier.ValueOpsImpl.get
    )(
      SeqValidationPredicate.alwaysTrue,
      SeqValidationPredicate.alwaysTrue
    )(
      rngManager
    )
    LazyTreapOrderedMap.apply(
      UniformOrderedMap.default(valueOps.unit),
      lazySeq
    )(
      domainOps,
      valueOps,
      rngManager
    )
  }

  /**
   * Returns new segment sequence with values of segments:
   *
   * u,,i,, = mapFunc(S,,j,,)
   *
   * where S,,j,, - segment of original sequence.
   *
   * Note, the number of segments in original and output sequences can be different due to adjacent segments
   * with same values after mapping are merged.
   *
   * <h3>Example</h3>
   *
   * Assume `mapFunc` returns:
   * <tr>- value `A` for segment S1</tr>
   * <tr>- value `A` for segment S2</tr>
   * <tr>- value `C` for segment S3</tr>
   * {{{
   *
   * original:
   *
   *          S1            S2             S3      - segments
   *   X------------)[-------------)[------------X
   *          A             B              C       - values
   *
   * output mapped sequence:
   *
   *   X---------------------------)[------------X
   *                  A                    C       -values
   * }}}
   *
   * @see [[map]]
   */
  def mapSegments[U](
    mapFunc: Segment[E, D, V] => U
  )(
    implicit valueOps: ValueOps[U]
  ): SegmentSeq[E, D, U] =
    MappedOrderedMap.apply(this, mapFunc)(domainOps, valueOps, rngManager)

  /**
   * Returns new segment sequence with values of segments:
   *
   * u,,i,, = mapFunc(v,,j,,)
   *
   * where v,,j,, - value of segment of original sequence.
   *
   * Note, the number of segments in original and output sequences can be different due to adjacent segments
   * with same values after mapping are merged.
   *
   * <h3>Example</h3>
   *
   * Assume `mapFunc` returns:
   * <tr>- value `A` for input value `A`</tr>
   * <tr>- value `A` for input value `B`</tr>
   * <tr>- value `C` for input value `C`</tr>
   * {{{
   *
   * original:
   *
   *   X------------)[-------------)[------------X
   *          A             B              C       - values
   *
   * output mapped sequence:
   *
   *   X---------------------------)[------------X
   *                  A                    C       -values
   * }}}
   *
   * @see [[mapSegments]]
   */
  def map[U](
    mapFunc: V => U
  )(
    implicit valueOps: ValueOps[U]
  ): SegmentSeq[E, D, U] =
    MappedValueOrderedMap.apply(this, mapFunc)(domainOps, valueOps, rngManager)

  /**
   * Returns new segment sequence that combines with `zipFunc` function values of original sequence and `other`
   * sequence:
   *
   * w,,i,, = zipFunc(v,,j,,, u,,k,,)
   *
   * where
   * <tr>w,,i,, - value of segment of output sequence;   </tr>
   * <tr>v,,j,, - value of segment of original sequence; </tr>
   * <tr>v,,k,, - value of segment of `other` sequence.  </tr>
   * <tr></tr>
   *
   * Adjacent segments of output sequence with the same values are merged.
   *
   * <h3>Invariant functions</h3>
   *
   * Some value `v` is invariant iff result of `zipFunc(v, u)` doesn't depend on another argument or more formally iff:
   * {{{
   *   Ǝ C ∀ u: zipFunc(v, u) = C.
   * }}}
   *
   * Invariant functions MAY return `true` for invariant values. This allows to apply some performance optimizations.
   *
   * Note that invariant functions MUST NOT return `true` for non-invariant values. This may produce inconsistent
   * output sequences depending of traverse direction.
   *
   * <h3>Example</h3>
   *
   * Assume `zipFunc` is defined by table:
   * {{{
   *
   *       |  A  |  B  |  C  |  <- v
   * -------------------------
   *    A  |  A  |  A  |  C  |
   * -------------------------
   *    B  |  A  |  A  |  A  |
   * -------------------------
   *    C  |  C  |  A  |  B  |
   * -------------------------
   *    ^
   *    u
   * }}}
   * Note that `B` is invariant here and both `invariantFuncV` and `invariantFuncU` may return `true` for it.
   * {{{
   *
   * original:
   *
   *   X------------)[-------------)[------------X
   *          A             B              C       - values
   *
   * `other`:
   *
   *   X-------------------)[--------------------X
   *             A                     C           - values
   *
   *  output zipped sequence:
   *
   *   X---------------------------)[------------X
   *                 A                     B       - values
   * }}}
   *
   * @see [[zip]]
   */
  def zipOptimized[U, W, S1 >: S, S2](
    other: SegmentSeqT[E, D, U, S2],
    zipFunc: (V, U) => W,
    invariantFuncV: V => Boolean,
    invariantFuncU: U => Boolean
  )(
    implicit valueOps: ValueOps[W]
  ): ZippedSegmentSeq[E, D, V, U, W, S1, S2] =
    ZippedOrderedMap.apply(
      this, other, zipFunc, invariantFuncV, invariantFuncU
    )(
      domainOps, valueOps, rngManager
    )

  /**
   * Returns new segment sequence that combines with `zipFunc` function values of original sequence and `other`
   * sequence:
   *
   * w,,i,, = zipFunc(v,,j,,, u,,k,,)
   *
   * where
   * <tr>w,,i,, - value of segment of output sequence;   </tr>
   * <tr>v,,j,, - value of segment of original sequence; </tr>
   * <tr>v,,k,, - value of segment of `other` sequence.  </tr>
   * <tr></tr>
   *
   * Adjacent segments of output sequence with the same values are merged.
   * <tr></tr>
   * 
   * Method is a simplified version of [[zipOptimized]] that doesn't require to specify invariant functions
   * using `false` predicate instead of them.
   * <tr></tr>
   * 
   * @see [[zipOptimized]]
   */
  def zip[U, W, S1 >: S, S2](
    other: SegmentSeqT[E, D, U, S2],
    zipFunc: (V, U) => W
  )(
    implicit valueOps: ValueOps[W]
  ): ZippedSegmentSeq[E, D, V, U, W, S1, S2] =
    zipOptimized(
      other, zipFunc, BooleanUtil.falsePredicate1, BooleanUtil.falsePredicate1
    )(
      valueOps
    )

  /**
   * Returns new segment sequence that combines values of original and `other` sequences into tuples:
   *
   * w,,i,, = (v,,j,,, u,,k,,)
   *
   * where
   * <tr>w,,i,, - value of segment of output sequence;   </tr>
   * <tr>v,,j,, - value of segment of original sequence; </tr>
   * <tr>v,,k,, - value of segment of `other` sequence.  </tr>
   * <tr></tr>
   *
   * Note that each segment of output sequence is considered to be included in set (see [[ValueOps.valueIncl]]).
   * <tr></tr>
   * 
   * @see [[zip]]
   */
  def zipIntoTuple[U, S1 >: S, S2](other: SegmentSeqT[E, D, U, S2]): ZippedSegmentSeq[E, D, V, U, (V, U), S1, S2] =
    zip(
      other, (_, _)
    )(
      new ValueOps.Tuple2Impl(InclusionPredicate.alwaysIncluded, valueOps, other.valueOps)
    )
}
