package ordset

import ordset.domain.{Domain, DomainOps}
import ordset.tree.core.eval.TreeVisitStack
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.eval.NodeVisitContext
import ordset.tree.treap.immutable.traverse.NodeSearch

// TODO: class description.
abstract class AbstractTreapSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  protected final type TreapSegment = TreapSegmentBase with Segment[E, D, W]

  /** @return true if sequence is empty i.e. contains no elements. */
  final override def isEmpty: Boolean = false

  /** @return true if sequence is universal i.e. contains all elements of domain. */
  final override def isUniversal: Boolean = false

  /** @return true if sequence contains `bound`. */
  final override def contains(bound: Bound[E]): Boolean =
    belongsToSet(getSegment(bound).value)

  /** @return true if sequence contains `element`. */
  final override def contains(element: E): Boolean =
    super.contains(element)

  /** @return first segment of sequence. */
  final override lazy val firstSegment: TreapInitialSegment =
    makeInitialSegment()

  /** @return last segment of sequence. */
  final override lazy val lastSegment: TreapTerminalSegment =
    makeTerminalSegment()

  /** @return segment which contains specified `bound`. */
  final override def getSegment(bound: Bound[E]): TreapSegment = {
    // We need to find upper bound of segment which contains input `bound`.
    // But `NodeSearch.down` function can return either upper or lower bound
    // (or more precisely - upper bound of required segment or upper bound of previous segment).
    var contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
        root,
        TreeVisitStack.contextOps.getEmptyContext
      )(
        NodeSearch.down[Bound.Upper[E], Bound[E], W, NodeVisitContext[Bound.Upper[E], W]](
          bound,
          TreeVisitStack.function[Bound.Upper[E], W, ImmutableTreap.Node]()
        )(
          domainOps.boundOrd
        )
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
      else TreapInnerSegment(contextExtract.tree, contextExtract.context)
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
          ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
            contextExtract.tree,
            contextExtract.context
          )(
            NodeSearch.up[Bound.Upper[E], W, NodeVisitContext[Bound.Upper[E], W]](
              NodeSearch.UpwardStopPredicate.toNextKey,
              TreeVisitStack.function()
            )(
              TreeVisitStack.contextOps
            )
          )
        TreapInnerSegment(contextExtract.tree, contextExtract.context)
      }
    }
  }

  /** @return segment which contains specified `element`. */
  final override def getSegment(element: E): Segment[E, D, W] =
    super.getSegment(element)

  /**
   * Treap of segments upper bounds.
   */
  protected val root: ImmutableTreap.Node[Bound.Upper[E], W]

  /** Value of last segment (without upper bound). */
  protected val lastValue: W

  /**
   * @return true if `value` belongs to set.
   */
  protected def belongsToSet(value: W): Boolean

  /**
   * @return initial segment of sequence.
   */
  protected final def makeInitialSegment(): TreapInitialSegment = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
        root,
        TreeVisitStack.contextOps.getEmptyContext
      )(
        NodeSearch.minKey(TreeVisitStack.function())
      )
    TreapInitialSegment(contextExtract.tree, contextExtract.context)
  }

  /**
   * @return terminal segment of sequence.
   */
  protected final def makeTerminalSegment(): TreapTerminalSegment = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
        root,
        TreeVisitStack.contextOps.getEmptyContext
      )(
        NodeSearch.maxKey(TreeVisitStack.function())
      )
    TreapTerminalSegment(contextExtract.tree, contextExtract.context)
  }

  /**
   * @return segment which follows after input segment.
   */
  protected final def makeSegmentWithPrev(segment: TreapSegmentWithNext): TreapSegmentWithPrev =
    // We can use reference equality for immutable tree.
    if (lastSegment.node.eq(segment.node)) lastSegment
    else {
      val contextExtract =
        ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
          segment.node,
          segment.context
        )(
          NodeSearch.nextKey(
            TreeVisitStack.function[Bound.Upper[E], W, ImmutableTreap.Node]()
          )(
            TreeVisitStack.contextOps
          )
        )
      TreapInnerSegment(contextExtract.tree, contextExtract.context)
    }

  /**
   * @return segment which follows before input segment.
   */
  protected final def makeSegmentWithNext(segment: TreapSegmentWithPrev): TreapSegmentWithNext = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
        segment.node,
        segment.context
      )(
        NodeSearch.prevKey(
          TreeVisitStack.function[Bound.Upper[E], W, ImmutableTreap.Node]()
        )(
          TreeVisitStack.contextOps
        )
      )
    // We can use reference equality for immutable tree.
    if (firstSegment.node.eq(contextExtract.tree)) firstSegment
    else TreapInnerSegment(contextExtract.tree, contextExtract.context)
  }

  /**
   * Base trait for non single segments. It has either previous segment or next.
   */
  protected sealed trait TreapSegmentBase extends SegmentLike[E, D, W] {

    val node: ImmutableTreap.Node[Bound.Upper[E], W]

    val context: NodeVisitContext[Bound.Upper[E], W]

    override def domainOps: DomainOps[E, D] = seq.domainOps

    override def value: W = node.value

    override def moveToFirst: FirstSegment = firstSegment

    override def moveToLast: LastSegment = lastSegment

    override def moveTo(bound: Bound[E]): GenSegment = getSegment(bound)
  }

  /**
   * Segment which has next segment.
   */
  protected sealed trait TreapSegmentWithNext extends TreapSegmentBase with SegmentWithNext {

    override def upperBound: Bound.Upper[E] = node.key

    override def moveNext: SegmentWithPrev = makeSegmentWithPrev(this)
  }

  /**
   * Segment which has previous segment.
   */
  protected sealed trait TreapSegmentWithPrev extends TreapSegmentBase with SegmentWithPrev {

    override lazy val lowerBound: Bound.Lower[E] = {
      val contextExtract =
        ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
          node,
          context
        )(
          NodeSearch.prevKey(
            TreeVisitStack.function[Bound.Upper[E], W, ImmutableTreap.Node]()
          )(
            TreeVisitStack.contextOps
          )
        )
      contextExtract.tree.key.flipUpper
    }

    override def movePrev: SegmentWithNext = makeSegmentWithNext(this)
  }

  /**
   * Initial segment of sequence.
   *
   * @param node treap node defining segment upper bound.
   * @param context path from treap root to current `node`.
   */
  protected sealed case class TreapInitialSegment(
    override val node: ImmutableTreap.Node[Bound.Upper[E], W],
    override val context: NodeVisitContext[Bound.Upper[E], W]
  ) extends TreapSegmentWithNext with InitialSegment

  /**
   * Inner segment of sequence.
   *
   * @param node treap node defining segment upper bound.
   * @param context path from treap root to current `node`.
   */
  protected sealed case class TreapInnerSegment(
    override val node: ImmutableTreap.Node[Bound.Upper[E], W],
    override val context: NodeVisitContext[Bound.Upper[E], W]
  ) extends TreapSegmentWithNext with TreapSegmentWithPrev with InnerSegment

  /**
   * Terminal segment of sequence.
   *
   * @param node treap node defining segment <u>lower</u> bound.
   * @param context path from treap root to current `node`.
   */
  protected sealed case class TreapTerminalSegment(
    override val node: ImmutableTreap.Node[Bound.Upper[E], W],
    override val context: NodeVisitContext[Bound.Upper[E], W]
  ) extends TreapSegmentWithPrev with TerminalSegment {

    override def domainOps: DomainOps[E, D] = seq.domainOps

    override def value: W = lastValue

    override lazy val lowerBound: Bound.Lower[E] = node.key.flipUpper

    override def movePrev: SegmentWithNext =
      if (root.isLeaf) TreapInitialSegment(node, context)
      else TreapInnerSegment(node, context)
  }
}
