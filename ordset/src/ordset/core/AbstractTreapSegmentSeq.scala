package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.tree.core.Fold
import ordset.tree.core.eval.{TreeStack, TreeVisitStack}
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.Treap
import ordset.tree.treap.immutable.transform.TreeSplit.{splitLeftFunc, splitRightFunc}
import ordset.tree.treap.immutable.transform.{BuildAsc, BuildDesc, SplitOutput, TreeMerge, TreeSplit}
import ordset.tree.treap.immutable.traverse.{NodeAside, NodeDownward, NodeUpward}
import ordset.tree.treap.immutable.{ImmutableTreap, NodeStackContext, NodeVisitContext}
import AbstractTreapSegmentSeq._

// TODO: class description.
abstract class AbstractTreapSegmentSeq[E, D <: Domain[E],  V]
  extends AbstractSegmentSeq[E, D, V, TreapSegmentBase[E, D, V]] {
  
  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = false

  final override def isUniversal: Boolean = false

  final override def isUniform: Boolean = false

  final override def contains(bound: Bound[E]): Boolean = getSegment(bound).isIncluded

  final override def containsElement(element: E): Boolean = super.containsElement(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds

  final override lazy val firstSegment: TreapInitialSegment[E, D, V] = makeInitialSegment()

  /**
   * @return second segment of sequence.
   */
  final def secondSegment: TreapSegmentWithPrev[E, D, V] = firstSegment.moveNext
  
  final override lazy val lastSegment: TreapTerminalSegment[E, D, V] = makeTerminalSegment()

  /**
   * @return penultimate segment of sequence.
   */
  final def penultimateSegment: TreapSegmentWithNext[E, D, V] = lastSegment.movePrev
  
  final override def getSegment(bound: Bound[E]): TreapSegment[E, D, V] = {
    // We need to find upper bound of segment which contains input `bound`.
    // But `NodeSearch.down` function can return either upper or lower bound
    // (or more precisely - upper bound of required segment or upper bound of previous segment).
    var contextExtract =
      NodeDownward.foldDefault[Bound.Upper[E], Bound[E], V, NodeVisitContext[Bound.Upper[E], V]](
          root,
          TreeVisitStack.contextOps.getEmptyContext,
          bound,
          TreeVisitStack.function
        )(
          domainOps.boundOrd
        )
    // Case 1: search key <= found node key
    //
    //    5  -                   A
    //    4  -             ↙        ↘
    //    3  -       B                 D
    //    2  -          ↘
    //    1  -             C
    //         |-----|-----|-----|-----|-----|
    //         1     2     3     4     5     6
    //                  ^  ^
    //         _________|  |_________
    //         search key  found node
    if (domainOps.boundOrd.compare(bound, contextExtract.tree.key) <= 0) {
      // We can use reference equality for immutable tree.
      if (firstSegment.node.eq(contextExtract.tree)) firstSegment
      else TreapInnerSegment(this, contextExtract.tree, contextExtract.context)
    }
    // Case 2: search key > found node key
    //
    //    5  -                   A
    //    4  -             ↙        ↘
    //    3  -       B                 D
    //    2  -          ↘
    //    1  -             C
    //         |-----|-----|-----|-----|
    //         1     2     3     4     5
    //                     ^  ^
    //            _________|  |_________
    //            found node  search key
    else {
      // We can use reference equality for immutable tree.
      if (lastSegment.node.eq(contextExtract.tree)) lastSegment
      else {
        // Move upward to the next key.
        contextExtract =
          NodeUpward.foldToNextKey[Bound.Upper[E], V, NodeVisitContext[Bound.Upper[E], V]](
            contextExtract.tree,
            contextExtract.context,
            TreeVisitStack.function
          )(
            TreeVisitStack.contextOps
          )
        TreapInnerSegment(this, contextExtract.tree, contextExtract.context)
      }
    }
  }

  final override def getSegmentForElement(element: E): TreapSegment[E, D, V] = super.getSegmentForElement(element)

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): TreapSegmentSeq[E, D, V] = sliced(bound)._2

  final override def takenBelow(bound: Bound[E]): TreapSegmentSeq[E, D, V] = sliced(bound)._1

  final override def sliced(bound: Bound[E]): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) = {
    val ord = domainOps.boundOrd
    if (ord.compare(bound, firstSegment.upperBound) <= 0) {
      (consUniform(firstSegment.value), this)

    } else if (ord.compare(bound, lastSegment.lowerBound) >= 0) {
      (this, consUniform(lastSegment.value))

    } else {
      val splitOutput =
        TreeSplit.foldNode[Bound.Upper[E], Bound[E], V](
          root,
          bound,
          splitLeft = false,
          SplitOutput.Mutable.Output.initial
        )(
          ord
        )
      val rightSeq = consFromTree(splitOutput.rightTree, lastValue)
      val leftSeq = consFromTree(splitOutput.leftTree, rightSeq.firstSegment.value)
      (leftSeq, rightSeq)
    }
  }

  final override def prepended(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = {
    val segment = secondSegment
    prependedInternal(segment.lowerBound, segment, other)
  }

  final override def prepended(bound: Bound[E], other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] =
    prependedInternal(bound, getSegment(bound.provideLower), other)

  final override def appended(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = {
    val segment = penultimateSegment
    appendedInternal(segment.upperBound, segment, other)
  }

  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] =
    appendedInternal(bound, getSegment(bound.provideUpper), other)
  
  // Protected section -------------------------------------------------------- //
  /**
   * Treap of segments upper bounds.
   */
  protected val root: ImmutableTreap.Node[Bound.Upper[E], V]

  /** Value of last segment (without upper bound). */
  protected val lastValue: V

  /**
   * Creates uniform segment sequence (empty or universal).
   *
   * Note current class not supports empty and universal sets so other implementations should be used.
   */
  protected def consUniform(value: V): UniformSegmentSeq[E, D, V]

  /**
   * Creates segment sequence from specified treap node.
   */
  protected def consFromNode(node: ImmutableTreap.Node[Bound.Upper[E], V], value: V): NonuniformTreapSegmentSeq[E, D, V]

  /**
   * Creates segment sequence from specified treap. If treap is empty, creates uniform sequence.
   */
  protected def consFromTree(tree: ImmutableTreap[Bound.Upper[E], V], value: V): TreapSegmentSeq[E, D, V] =
    tree match {
      case node: ImmutableTreap.Node[Bound.Upper[E], V] => consFromNode(node, value)
      case _ => consUniform(value)
    }
  
  /**
   * Same as [[SegmentSeqT.prepended]] but with additional argument `originalBoundSegment` such that:
   * {{{
   *   originalBoundSegment = this.getSegment(bound.provideLower)    (1)
   * }}}
   * This allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note if provided segment other then one defined by condition 1, the behaviour of method is undefined.
   */
  protected def prependedInternal(
    bound: Bound[E],
    originalBoundSegment: TreapSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] = {

    // CASE 1                                              // CASE 2
    // - bound doesn't match with segment bound            // - bound matches with segment bound
    // - values of bound segments don't match              // - values of bound segments don't match
    //                                                     //
    // original:                                           // original:
    //                bound  originalBoundSegment          //                      bound  originalBoundSegment
    //                   )   /                             //                        ]    /
    // X--false---](---true---](-----false----X            // X--false---](---true---](-----false----X
    //                                                     //
    // other:                                              // other:
    //                      otherBoundSegment              //                      otherBoundSegment
    //                       /                             //                       /
    // X--t--)[-------false------](---true----X            // X--f--)[--------true------](--false----X
    //                                                     //
    // original.prepended(bound, other):                   // original.prepended(bound, other):
    //                                                     //
    //                 bound                               //                      bound
    //                   v                                 //                        v
    // X--t--)[---false--)[-t-](-----false----X            // X--f--)[-----true------](-----false----X

    // CASE 3                                              // CASE 4
    // - bound doesn't match with segment bound            // - bound matches with segment bound
    // - values of bound segments match                    // - values of bound segments match
    //                                                     //
    // original:                                           // original:
    //                bound  originalBoundSegment          //                      bound  originalBoundSegment
    //                   )   /                             //                        ]    /
    // X--false---](---true---](-----false----X            // X--false---](---true---](-----false----X
    //                                                     //
    // other:                                              // other:
    //                      otherBoundSegment              //                      otherBoundSegment
    //                       /                             //                       /
    // X--f--)[--------true------](---false---X            // X--t--)[-------false------](----true---X
    //                                                     //
    // original.prepended(bound, other):                   // original.prepended(bound, other):
    //                                                     //
    // X--f--)[-----true------](-----false----X            // X--t--)[-------------false-------------X

    val upperBound = bound.provideUpper

    val otherBoundSegment = other.getSegment(upperBound)

    val boundValuesMatch = originalBoundSegment.hasValue(otherBoundSegment.value)

    val originalRightSequence = originalBoundSegment.takenAbove

    val boundOrd = domainOps.boundOrd
    val rng = rngManager.newUnsafeUniformRng()

    val originalBuffer = BuildDesc.leftFrontToBuffer(getRoot(originalRightSequence))

    val buffer =
      if (boundValuesMatch) originalBuffer
      else
        BuildDesc.addToBuffer[Bound.Upper[E], Bound[E], V](
          originalBuffer, upperBound, rng.nextInt(), otherBoundSegment.value
        )(
          boundOrd
        )

    val rightRoot = BuildDesc.finalizeBuffer(buffer)

    if (otherBoundSegment.isFirst) consFromTree(rightRoot, lastSegment.value)
    else otherBoundSegment match {
      case otherBoundSegment: TreapSegmentWithPrev[E, D, V] =>
        val leftSequence = otherBoundSegment.takenBelow
        val mergedRoot = TreeMerge.foldTreaps(leftSequence.root, rightRoot)(Treap.nodePriorityOrder(boundOrd))
        consFromTree(mergedRoot, lastSegment.value)
      case _ =>
        val buffer = otherBoundSegment.backwardIterable.drop(1).foldLeft(
          BuildDesc.leftFrontToBuffer[Bound.Upper[E], V](rightRoot)
        ) {
          (buf, seg) => seg match {
            case seg: Segment.WithNext[E, D, V] =>
              BuildDesc.addToBuffer[Bound.Upper[E], Bound[E], V](
                buf, seg.upperBound, rng.nextInt(), seg.value
              )(
                boundOrd
              )
            case _ => buf
          }
        }
        consFromTree(BuildDesc.finalizeBuffer(buffer), lastSegment.value)
    }
  }

  /**
   * Same as [[SegmentSeqT.appended]] but with additional argument `originalBoundSegment` such that:
   * {{{
   *   originalBoundSegment = this.getSegment(bound.provideUpper)    (1)
   * }}}
   * This allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note if provided segment other then one defined by condition 1, the behaviour of method is undefined.
   */
  protected def appendedInternal(
    bound: Bound[E],
    originalBoundSegment: TreapSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] = {

    // CASE 1                                              // CASE 2
    // - bound doesn't match with segment bound            // - bound matches with segment bound
    // - values of bound segments don't match              // - values of bound segments don't match
    //                                                     //
    // original:                                           // original:
    //                bound  originalBoundSegment          // originalBoundSegment  bound
    //                   )   /                             //                \       ]
    // X--false---](---true---](-----false----X            // X--false---](---true---](-----false----X
    //                                                     //
    // other:                                              // other:
    //                      otherBoundSegment              //                      otherBoundSegment
    //                       /                             //                       /
    // X--t--)[-------false------](---true----X            // X--t--)[-------false------](---true----X
    //                                                     //
    // original.appended(bound, other):                    // original.appended(bound, other):
    //                                                     //
    //                 bound                               //                      bound
    //                   v                                 //                        v
    // X--false---](--t--)[--f---](---true----X            // X--false---](---true---](f](---true----X

    // CASE 3                                              // CASE 4
    // - bound doesn't match with segment bound            // - bound matches with segment bound
    // - values of bound segments match                    // - values of bound segments match
    //                                                     //
    // original:                                           // original:
    //                bound  originalBoundSegment          // originalBoundSegment  bound
    //                   )   /                             //                \       ]
    // X--false---](---true---](-----false----X            // X--false---](---true---](-----false----X
    //                                                     //
    // other:                                              // other:
    //                      otherBoundSegment              //                      otherBoundSegment
    //                       /                             //                       /
    // X--f--)[--------true------](---false---X            // X--f--)[--------true------](---false---X
    //                                                     //
    // original.appended(bound, other):                    // original.appended(bound, other):
    //                                                     //
    // X--false---](----true-----](---false---X            // X--false---](----true-----](---false---X

    val upperBound = bound.provideUpper
    val lowerBound = bound.provideLower
    
    val otherBoundSegment = other.getSegment(lowerBound)

    val originalBoundMatch = originalBoundSegment.hasUpperBound(upperBound)
    val boundValuesMatch = originalBoundSegment.hasValue(otherBoundSegment.value)
    val skipBound = originalBoundMatch || boundValuesMatch

    val originalLeftSequence: TreapSegmentSeq[E, D, V] =
      if (originalBoundMatch && !boundValuesMatch) originalBoundSegment match {
        case s: TreapSegmentWithNext[E, D, V] => s.moveNext.takenBelow
        case _ =>
          // `originalBoundMatch` == `true` => `originalBoundSegment` has upper bound => impossible to get here
          throw new AssertionError(s"Expected segment $originalBoundSegment has next segment.")
      }
      else originalBoundSegment.takenBelow

    val boundOrd = domainOps.boundOrd
    val rng = rngManager.newUnsafeUniformRng()

    val originalBuffer = BuildAsc.rightFrontToBuffer(getRoot(originalLeftSequence))
    val buffer =
      if (skipBound) originalBuffer
      else
        BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], V](
          originalBuffer, upperBound, rng.nextInt(), originalBoundSegment.value
        )(
          boundOrd
        )

    val leftRoot = BuildAsc.finalizeBuffer(buffer)

    if (otherBoundSegment.isLast) consFromTree(leftRoot, otherBoundSegment.value)
    else otherBoundSegment match {
      case otherBoundSegment: TreapSegmentWithNext[E, D, V] =>
        val rightSequence = otherBoundSegment.takenAbove
        val mergedRoot = TreeMerge.foldTreaps(leftRoot, rightSequence.root)(Treap.nodePriorityOrder(boundOrd))
        consFromTree(mergedRoot, rightSequence.lastSegment.value)
      case _ =>
        val buffer = otherBoundSegment.forwardIterable.foldLeft(
          BuildAsc.rightFrontToBuffer[Bound.Upper[E], V](leftRoot)
        ) {
          (buf, seg) => seg match {
            case seg: Segment.WithNext[E, D, V] =>
              BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], V](
                buf, seg.upperBound, rng.nextInt(), seg.value
              )(
                boundOrd
              )
            case _ => buf
          }
        }
        consFromTree(BuildAsc.finalizeBuffer(buffer), other.lastSegment.value)
    }
  }

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isValueIncluded(value: V): Boolean = valueOps.isIncluded(value)

  /**
   * @return initial segment of sequence.
   */
  protected final def makeInitialSegment(): TreapInitialSegment[E, D, V] = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], V, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], V]](
        root,
        TreeVisitStack.contextOps.getEmptyContext
      )(
        NodeAside.minKeyFunc(TreeVisitStack.function)
      )
    TreapInitialSegment(this, contextExtract.tree, contextExtract.context)
  }

  /**
   * @return terminal segment of sequence.
   */
  protected final def makeTerminalSegment(): TreapTerminalSegment[E, D, V] = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], V, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], V]](
        root,
        TreeVisitStack.contextOps.getEmptyContext
      )(
        NodeAside.maxKeyFunc(TreeVisitStack.function)
      )
    TreapTerminalSegment(this, contextExtract.tree, contextExtract.context)
  }

  /**
   * @return segment which follows after input segment.
   */
  protected final def makeSegmentWithPrev(segment: TreapSegmentWithNext[E, D, V]): TreapSegmentWithPrev[E, D, V] =
    // We can use reference equality for immutable tree.
    if (lastSegment.node.eq(segment.node)) lastSegment
    else {
      val contextExtract =
        ContextExtract.foldAfter[Bound.Upper[E], V, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], V]](
          segment.node,
          segment.context
        )(
          NodeAside.nextKeyFunc[Bound.Upper[E], V, NodeVisitContext[Bound.Upper[E], V]](
            TreeVisitStack.function
          )(
            TreeVisitStack.contextOps
          )
        )
      TreapInnerSegment(this, contextExtract.tree, contextExtract.context)
    }

  /**
   * @return segment which follows before input segment.
   */
  protected final def makeSegmentWithNext(segment: TreapSegmentWithPrev[E, D, V]): TreapSegmentWithNext[E, D, V] = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], V, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], V]](
        segment.node,
        segment.context
      )(
        NodeAside.prevKeyFunc[Bound.Upper[E], V, NodeVisitContext[Bound.Upper[E], V]](
          TreeVisitStack.function
        )(
          TreeVisitStack.contextOps
        )
      )
    // We can use reference equality for immutable tree.
    if (firstSegment.node.eq(contextExtract.tree)) firstSegment
    else TreapInnerSegment(this, contextExtract.tree, contextExtract.context)
  }
}

object AbstractTreapSegmentSeq {

  type TreapSegment[E, D <: Domain[E], V] = SegmentT[E, D, V, TreapSegmentBase[E, D, V]] with TreapSegmentBase[E, D, V]

  def getRoot[E, D <: Domain[E], V](seq: TreapSegmentSeq[E, D, V]): ImmutableTreap[Bound.Upper[E], V] =
    seq match {
      case s: NonuniformTreapSegmentSeq[E, D, V] => s.root
      case _: UniformSegmentSeq[E, D, V] => ImmutableTreap.Empty
    }

  /**
   * Base trait for non single segments. It has either previous segment or next.
   */
  sealed trait TreapSegmentBase[E, D <: Domain[E], V]
    extends SegmentLikeT[E, D, V, TreapSegmentBase[E, D, V]] {

    // Inspection --------------------------------------------------------------- //
    val node: ImmutableTreap.Node[Bound.Upper[E], V]

    val context: NodeVisitContext[Bound.Upper[E], V]

    override val sequence: NonuniformTreapSegmentSeq[E, D, V]

    override def value: V = node.value

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: TreapInitialSegment[E, D, V] = sequence.firstSegment

    override def moveToLast: TreapTerminalSegment[E, D, V] = sequence.lastSegment

    override def moveTo(bound: Bound[E]): TreapSegment[E, D, V] = sequence.getSegment(bound)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: TreapSegmentSeq[E, D, V]

    override def takenBelow: TreapSegmentSeq[E, D, V]

    override def sliced: (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V])

    override def prepended(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = {
      // Default implementation for first segment. Must be overridden if segment has previous segment.
      sequence
    }

    override def appended(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = {
      // Default implementation for last segment. Must be overridden if segment has next segment.
      sequence
    }

    override def patched(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = {
      // We need to override method here to provide more concrete type.
      // But we can't make method abstract due to conflict with implementation in parent trait.
      // So we just throw exception here and real implemented is provided subclasses.
      // Trait is sealed so this is unreachable case.
      throw new AssertionError("Implementation is provided in subclasses of sealed trait.")
    }
  }

  object TreapSegmentBase {

    trait TruncationBase[E, D <: Domain[E], V] {
      self: SegmentLikeT.Truncation[E, D, V, TreapSegmentBase[E, D, V]] =>

      override def prepended(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] =
        segment.sequence.prependedInternal(bound, getPrependedBoundSegment, other)

      override def appended(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] =
        segment.sequence.appendedInternal(bound, getAppendedBoundSegment, other)
    }
  }

  /**
   * Segment which has next segment.
   */
  sealed trait TreapSegmentWithNext[E, D <: Domain[E], V] 
    extends SegmentT.WithNext[E, D, V, TreapSegmentBase[E, D, V]]
      with TreapSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def upperBound: Bound.Upper[E] = node.key

    // Navigation --------------------------------------------------------------- //
    override def moveNext: TreapSegmentWithPrev[E, D, V] = sequence.makeSegmentWithPrev(this)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: NonuniformTreapSegmentSeq[E, D, V]

    override def sliced: (TreapSegmentSeq[E, D, V], NonuniformTreapSegmentSeq[E, D, V])

    override def appended(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] =
      sequence.appendedInternal(upperBound, this, other)
  }

  /**
   * Segment which has previous segment.
   */
  sealed trait TreapSegmentWithPrev[E, D <: Domain[E], V] 
    extends SegmentT.WithPrev[E, D, V, TreapSegmentBase[E, D, V]]
      with TreapSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override lazy val lowerBound: Bound.Lower[E] = {
      val contextExtract =
        ContextExtract.foldAfter[Bound.Upper[E], V, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], V]](
          node,
          context
        )(
          NodeAside.prevKeyFunc[Bound.Upper[E], V, NodeVisitContext[Bound.Upper[E], V]](
            TreeVisitStack.function
          )(
            TreeVisitStack.contextOps
          )
        )
      contextExtract.tree.key.flipUpper
    }

    // Navigation --------------------------------------------------------------- //
    override def movePrev: TreapSegmentWithNext[E, D, V] = sequence.makeSegmentWithNext(this)

    // Transformation ----------------------------------------------------------- //
    override def takenBelow: NonuniformTreapSegmentSeq[E, D, V]

    override def sliced: (NonuniformTreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V])

    override def prepended(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] =
      sequence.prependedInternal(lowerBound, this, other)
  }

  /**
   * Initial segment of sequence.
   * 
   * @param sequence treap based segment sequence.
   * @param node treap node defining segment upper bound.
   * @param context path from treap root to `node`.
   */
  final case class TreapInitialSegment[E, D <: Domain[E], V] (
    override val sequence: NonuniformTreapSegmentSeq[E, D, V],
    override val node: ImmutableTreap.Node[Bound.Upper[E], V],
    override val context: NodeVisitContext[Bound.Upper[E], V]
  ) extends SegmentT.Initial[E, D, V, TreapSegmentBase[E, D, V]]
    with TreapSegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: TreapInitialSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: NonuniformTreapSegmentSeq[E, D, V] = sequence

    override def takenBelow: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def sliced: (UniformSegmentSeq[E, D, V], NonuniformTreapSegmentSeq[E, D, V]) =
      (takenBelow, takenAbove)

    override def patched(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = moveNext.prepended(other)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: TreapInitialSegment[E, D, V] = this

    override def truncation(bound: Bound[E]): TreapInitialSegment.Truncation[E, D, V] =
      new TreapInitialSegment.Truncation(this, bound)
  }

  object TreapInitialSegment {

    final class Truncation[E, D <: Domain[E], V](
      override val segment: TreapInitialSegment[E, D, V],
      inputBound: Bound[E],
    ) extends SegmentT.Initial.Truncation[E, D, V, TreapSegmentBase[E, D, V]](
      segment,
      inputBound,
    ) with TreapSegmentBase.TruncationBase[E, D, V]
  }

  /**
   * Terminal segment of sequence.
   *
   * @param sequence treap based segment sequence.
   * @param node treap node defining segment <u>lower</u> bound.
   * @param context path from treap root to `node`.
   */
  final case class TreapTerminalSegment[E, D <: Domain[E], V](
    override val sequence: NonuniformTreapSegmentSeq[E, D, V],
    override val node: ImmutableTreap.Node[Bound.Upper[E], V],
    override val context: NodeVisitContext[Bound.Upper[E], V]
  ) extends SegmentT.Terminal[E, D, V, TreapSegmentBase[E, D, V]]
    with TreapSegmentWithPrev[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def value: V = sequence.lastValue

    override lazy val lowerBound: Bound.Lower[E] = node.key.flipUpper

    override def self: TreapTerminalSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: TreapTerminalSegment[E, D, V] = this
    
    override def movePrev: TreapSegmentWithNext[E, D, V] =
      if (sequence.root.isLeaf) TreapInitialSegment(sequence, node, context)
      else TreapInnerSegment(sequence, node, context)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def takenBelow: NonuniformTreapSegmentSeq[E, D, V] = sequence

    override def sliced: (NonuniformTreapSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) =
      (takenBelow, takenAbove)

    override def patched(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] = movePrev.appended(other)

    override def truncation(bound: Bound[E]): TreapTerminalSegment.Truncation[E, D, V] =
      new TreapTerminalSegment.Truncation(this, bound)
  }

  object TreapTerminalSegment {

    final class Truncation[E, D <: Domain[E], V](
      override val segment: TreapTerminalSegment[E, D, V],
      inputBound: Bound[E],
    ) extends SegmentT.Terminal.Truncation[E, D, V, TreapSegmentBase[E, D, V]](
      segment,
      inputBound,
    ) with TreapSegmentBase.TruncationBase[E, D, V]
  }

  /**
   * Inner segment of sequence.
   *
   * @param sequence treap based segment sequence.
   * @param node treap node defining segment upper bound.
   * @param context path from treap root to `node`.
   */
  final case class TreapInnerSegment[E, D <: Domain[E], V](
    override val sequence: NonuniformTreapSegmentSeq[E, D, V],
    override val node: ImmutableTreap.Node[Bound.Upper[E], V],
    override val context: NodeVisitContext[Bound.Upper[E], V]
  ) extends SegmentT.Inner[E, D, V, TreapSegmentBase[E, D, V]]
    with TreapSegmentWithNext[E, D, V]
    with TreapSegmentWithPrev[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: TreapInnerSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: NonuniformTreapSegmentSeq[E, D, V] = sliced._2

    override def takenBelow: NonuniformTreapSegmentSeq[E, D, V] = sliced._1

    override def sliced: (NonuniformTreapSegmentSeq[E, D, V], NonuniformTreapSegmentSeq[E, D, V]) = {
      // Generally we can't just fold `context` of current node with split function.
      // Before we need to move down to get correct stack for split operation.
      //
      // If for example `A` is a current node then we need to move down to `C`.
      // The result stack will contain nodes `B`, `A`, ... And now we can move upward
      // and fold received stack with the split function.
      //
      //    7  -                             ↙
      //    6  -                         A
      //    5  -                    ↙    |  ↘
      //    4  -              ↙                ↘
      //    3  -         B               |       D
      //    2  -             ↘
      //    1  -         |       C       |       |
      //         |-------|-------|-------|-------|
      //         1       2       3       4       5
      //                         ^   ^   ^
      //            lower bound _|   |   |_ upper bound
      //                          segment
      val contextExtract =
        NodeDownward.foldForRightSplit[Bound.Upper[E], Bound[E], V, NodeStackContext[Bound.Upper[E], V]](
          node,
          context.stack.map(_.tree),
          upperBound,
          TreeStack.function
        )(
          domainOps.boundOrd
        )
      val splitFunc = 
        TreeSplit.splitRightFunc[Bound.Upper[E], Bound[E], V, NodeStackContext[Bound.Upper[E], V]](
          upperBound
        )(
          domainOps.boundOrd
        )
      val splitOutput =
        Fold.before(
          contextExtract.tree,
          contextExtract.context,
          SplitOutput.Mutable.Output.initial
        )(
          NodeUpward.defaultFunc[Bound.Upper[E], V, NodeStackContext[Bound.Upper[E], V]](
            NodeUpward.StopPredicate.never,
            TreeStack.function
          )(
            TreeStack.contextOps
          ),
          splitFunc
        )
      // Segment is inner => `splitOutput.rightTree` and `splitOutput.leftTree` have at least one bound =>
      // they are not empty trees => cast is safe.
      val rightNode = splitOutput.rightTree.asInstanceOf[ImmutableTreap.Node[Bound.Upper[E], V]]
      val leftNode = splitOutput.leftTree.asInstanceOf[ImmutableTreap.Node[Bound.Upper[E], V]]
      
      val rightSeq = sequence.consFromNode(rightNode, sequence.lastValue)
      val leftSeq = sequence.consFromNode(leftNode, rightSeq.firstSegment.value)
      (leftSeq, rightSeq)
    }

    override def patched(other: SegmentSeq[E, D, V]): TreapSegmentSeq[E, D, V] =
      moveNext.prepended(movePrev.appended(other))

    override def truncation(bound: Bound[E]): TreapInnerSegment.Truncation[E, D, V] =
      new TreapInnerSegment.Truncation(this, bound)
  }

  object TreapInnerSegment {

    final class Truncation[E, D <: Domain[E], V](
      override val segment: TreapInnerSegment[E, D, V],
      inputBound: Bound[E],
    ) extends SegmentT.Inner.Truncation[E, D, V, TreapSegmentBase[E, D, V]](
      segment,
      inputBound,
    ) with TreapSegmentBase.TruncationBase[E, D, V]
  }
}