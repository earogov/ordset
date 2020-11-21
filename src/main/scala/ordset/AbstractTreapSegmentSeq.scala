//package ordset
//
//import ordset.domain.{Domain, DomainOps}
//import ordset.treap.Treap
//import ordset.treap.eval.NodeVisitStack
//import ordset.treap.reduce.ContextExtract
//import ordset.treap.traverse.KeySearch
//
///**
// * {{{
// *                                __________________________  *
// *                             ↙                                  ↘
// *                __________  * _________________                   \
// *             ↙                                   ↘                 *
// *           *  _                                    \
// *                ↘                       __________ *
// *                 \                   ↙
// *                  \                *  ___
// *                   *                      ↘
// *                                           *
// *
// *       0   |   1   |   2   |   3   |   4   |   5   |   6   |   7   |
// *    -------|-------|-------|-------|-------|-------|-------|-------|-------
// *
// * }}}
// */
//abstract class AbstractTreapSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>
//
//  /** @return true if sequence is empty i.e. contains no elements. */
//  override def isEmpty: Boolean = treap.isEmpty && !belongsToSet(lastValue)
//
//  /** @return true if sequence is universal i.e. contains all elements of domain. */
//  override def isUniversal: Boolean = treap.isEmpty && belongsToSet(lastValue)
//
//  /** @return true if sequence contains `bound`. */
//  override def contains(bound: Bound[E]): Boolean = ???
//
//  /** @return true if sequence contains `element`. */
//  override def contains(element: E): Boolean = ???
//
//  /** @return first segment of sequence. */
//  override def firstSegment: Segment.First[E, D, W] =
//    if (treap.isEmpty) makeSingleSegment() else makeInitialSegment()
//
//  /** @return last segment of sequence. */
//  override def lastSegment: Segment.Last[E, D, W] =
//    if (treap.isEmpty) makeSingleSegment() else makeTerminalSegment()
//
//  /** @return segment which contains specified `bound`. */
//  override def getSegment(bound: Bound[E]): Segment[E, D, W] = ???
//
//  /** @return segment which contains specified `element`. */
//  override def getSegment(element: E): Segment[E, D, W] = ???
//
//  /**
//   * Treap of segments upper bounds.
//   *
//   * Empty and universal segment sequences are represented by [[Treap.Empty]] with some [[lastValue]].
//   * If that value doesn't belong to set sequence is empty, otherwise - universal.
//   */
//  protected val treap: Treap[Bound.Upper[E], D, W]
//
//  /** Value of last segment (without upper bound). */
//  protected val lastValue: W
//
//  protected lazy val firstBound: Bound.Upper[E] = {
//    val contextExtract = ContextExtract.reduceAfter(
//      treap,
//      NodeVisitStack.contextOps[Bound.Upper[E], D, W].getEmptyContext
//    )(
//      KeySearch.minKey(NodeVisitStack[Bound.Upper[E], D, W]())
//    )
//    contextExtract.tree match {
//      case node: Treap.Node[Bound.Upper[E], D, W] => node.key
//      case _ => throwEmptyOrUniversalSequence("Unable to define first bound")
//    }
//  }
//
//  protected lazy val lastBound: Bound.Upper[E] = {
//    val contextExtract = ContextExtract.reduceAfter(
//      treap,
//      NodeVisitStack.contextOps[Bound.Upper[E], D, W].getEmptyContext
//    )(
//      KeySearch.maxKey(NodeVisitStack[Bound.Upper[E], D, W]())
//    )
//    contextExtract.tree match {
//      case node: Treap.Node[Bound.Upper[E], D, W] => node.key
//      case _ => throwEmptyOrUniversalSequence("Unable to define last bound")
//    }
//  }
//
//  protected def belongsToSet(value: W): Boolean
//
//  /**
//   * Preconditions:
//   *
//   * 1. `treap` is instance of [[Treap.Node]].
//   *
//   * @return initial segment of sequence.
//   */
//  protected final def makeInitialSegment(): TreapInitialSegment = {
//    val contextExtract =
//      ContextExtract.reduceAfter(
//        treap,
//        NodeVisitStack.contextOps[Bound.Upper[E], D, W].getEmptyContext
//      )(
//        KeySearch.minKey(NodeVisitStack[Bound.Upper[E], D, W]())
//      )
//    contextExtract.tree match {
//      case node: Treap.Node[Bound.Upper[E], D, W] => TreapInitialSegment(node, contextExtract.context)
//      case _ => throwEmptyOrUniversalSequence("Unable to create initial segment")
//    }
//  }
//
//  /**
//   * Preconditions:
//   *
//   * 1. `treap` is instance of [[Treap.Node]].
//   *
//   * @return terminal segment of sequence.
//   */
//  protected final def makeTerminalSegment(): TreapTerminalSegment = {
//    val contextExtract =
//      ContextExtract.reduceAfter(
//        treap,
//        NodeVisitStack.contextOps[Bound.Upper[E], D, W].getEmptyContext
//      )(
//        KeySearch.maxKey(NodeVisitStack[Bound.Upper[E], D, W]())
//      )
//    contextExtract.tree match {
//      case node: Treap.Node[Bound.Upper[E], D, W] => TreapTerminalSegment(node, contextExtract.context)
//      case _ => throwEmptyOrUniversalSequence("Unable to create terminal segment")
//    }
//  }
//
//  /**
//   * Preconditions:
//   *
//   * 1. `treap` is instance of [[Treap.Empty]].
//   *
//   * @return single segment of sequence.
//   */
//  protected final def makeSingleSegment(): TreapSingleSegment = {
//    if (treap.isEmpty) TreapSingleSegment()
//    else throw new IllegalArgumentException("Unable to create single segment: sequence is not empty nor universal.")
//  }
//
//  /**
//   * Preconditions:
//   *
//   * 1. `treap` is instance of [[Treap.Node]].
//   *
//   * @return segment which follows after input segment.
//   */
//  protected final def makeSegmentWithPrev(segment: TreapSegmentWithNext): TreapSegmentWithPrev = {
//    val contextExtract =
//      ContextExtract.reduceAfter(
//        segment.node,
//        segment.context
//      )(
//        KeySearch.nextKey(NodeVisitStack[Bound.Upper[E], D, W]())(NodeVisitStack.contextOps[Bound.Upper[E], D, W])
//      )
////    contextExtract.tree match {
////      case node: Treap.Node[Bound.Upper[E], D, W] =>
////        if (segment.node == contextExtract.tree) {
////          // We have not moved to the next node => segment.node is last.
////          TreapTerminalSegment(node, contextExtract.context)
////        } else {
////          TreapInnerSegment(node, contextExtract.context)
////        }
////      case _ => throwEmptyOrUniversalSequence(s"Unable to create next segment for $segment")
////    }
//  }
//
//
//  /**
//   * Preconditions:
//   *
//   * 1. `treap` is instance of [[Treap.Node]].
//   *
//   * @return segment which follows before input segment.
//   */
//  protected final def makeSegmentWithNext(segment: TreapSegmentWithPrev): TreapSegmentWithNext = {
//    val contextExtract =
//      ContextExtract.reduceAfter(
//        segment.node,
//        segment.context
//      )(
//        KeySearch.prevKey(NodeVisitStack[Bound.Upper[E], D, W]())(NodeVisitStack.contextOps[Bound.Upper[E], D, W])
//      )
////    contextExtract.tree match {
////      case node: Treap.Node[Bound.Upper[E], D, W] =>
////        if (segment.node == contextExtract.tree) {
////          // We have not moved to the previous node => segment.node is first.
////          TreapInitialSegment(node, contextExtract.context)
////        } else {
////          TreapInnerSegment(node, contextExtract.context)
////        }
////      case _ => throwEmptyOrUniversalSequence(s"Unable to create previous segment for $segment")
////    }
//  }
//
//  protected final def throwEmptyOrUniversalSequence(msg: String): Nothing = {
//    val seq = if (isEmpty) "empty" else if (isUniversal) "universal" else ""
//    val reason = if (seq.isEmpty) "" else s"sequence is $seq"
//    val message = if (reason.isEmpty) msg else s"$msg: $reason."
//    throw new IllegalArgumentException(message)
//  }
//
//  protected sealed trait TreapSegmentBase extends SegmentLike[E, D, W] {
//
//    val node: Treap.Node[Bound.Upper[E], D, W]
//
//    val context: NodeVisitStack.Context[Bound.Upper[E], D, W]
//
//    override def domainOps: DomainOps[E, D] = seq.domainOps
//
//    override def value: W = node.value
//
//    override def moveToFirst: FirstSegment = ???
//
//    override def moveToLast: LastSegment = ???
//
//    override def moveTo(bound: Bound[E]): GenSegment = ???
//  }
//
//  protected sealed trait TreapSegmentWithNext extends TreapSegmentBase with SegmentWithNext {
//
//    override def upperBound: Bound.Upper[E] = node.key
//
//    override def moveNext: SegmentWithPrev = ??? ///makeSegmentWithPrev(ind + 1)
//  }
//
//  protected sealed trait TreapSegmentWithPrev extends TreapSegmentBase with SegmentWithPrev {
//
//    override def lowerBound: Bound.Lower[E] = ???
//
//    override def movePrev: SegmentWithNext = ??? ///makeSegmentWithNext(ind - 1)
//  }
//
//  protected sealed case class TreapInitialSegment(
//    override val node: Treap.Node[Bound.Upper[E], D, W],
//    override val context: NodeVisitStack.Context[Bound.Upper[E], D, W]
//  ) extends TreapSegmentBase with InitialSegment {
//
//  }
//
//  protected sealed case class TreapInnerSegment(
//    override val node: Treap.Node[Bound.Upper[E], D, W],
//    override val context: NodeVisitStack.Context[Bound.Upper[E], D, W]
//  ) extends TreapSegmentWithNext with TreapSegmentWithPrev with InnerSegment {
//
//  }
//
//  protected sealed case class TreapTerminalSegment(
//    override val node: Treap.Node[Bound.Upper[E], D, W],
//    override val context: NodeVisitStack.Context[Bound.Upper[E], D, W]
//  ) extends TreapSegmentWithPrev with TerminalSegment {
//
//    override def domainOps: DomainOps[E, D] = seq.domainOps
//
//    override def value: W = lastValue
//
//    override def lowerBound: Bound.Lower[E] = node.key.flipUpper
//
//    override def movePrev: SegmentWithNext =
//      if (treap.isLeaf) TreapInitialSegment(node, context)
//      else TreapInnerSegment(node, context)
//  }
//
//  protected sealed case class TreapSingleSegment() extends SingleSegment {
//
//    override def domainOps: DomainOps[E, D] = seq.domainOps
//
//    override def value: W = lastValue
//  }
//}
