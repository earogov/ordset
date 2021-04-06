package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.tree.core.eval.{TreeStack, TreeVisitStack}
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.Treap
import ordset.tree.treap.immutable.transform.{BuildAsc, SplitOutput, TreeMerge, TreeSplit}
import ordset.tree.treap.immutable.traverse.{NodeAside, NodeDownward, NodeUpward}
import ordset.tree.treap.immutable.{ImmutableTreap, NodeStackContext, NodeVisitContext}

// TODO: class description.
abstract class AbstractTreapSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = false

  final override def isUniversal: Boolean = false

  final override def isUniform: Boolean = false

  final override def contains(bound: Bound[E]): Boolean = getSegment(bound).isIncluded

  final override def contains(element: E): Boolean = super.contains(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = forwardUpperBoundsFromSegment(firstSegment)

  final override lazy val firstSegment: TreapInitialSegment = makeInitialSegment()

  final override lazy val lastSegment: TreapTerminalSegment = makeTerminalSegment()

  final override def getSegment(bound: Bound[E]): TreapSegment = {
    // We need to find upper bound of segment which contains input `bound`.
    // But `NodeSearch.down` function can return either upper or lower bound
    // (or more precisely - upper bound of required segment or upper bound of previous segment).
    var contextExtract =
      NodeDownward.foldDefault[Bound.Upper[E], Bound[E], W, NodeVisitContext[Bound.Upper[E], W]](
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
          NodeUpward.foldToNextKey[Bound.Upper[E], W, NodeVisitContext[Bound.Upper[E], W]](
            contextExtract.tree,
            contextExtract.context,
            TreeVisitStack.function
          )(
            TreeVisitStack.contextOps
          )
        TreapInnerSegment(contextExtract.tree, contextExtract.context)
      }
    }
  }

  final override def getSegment(element: E): Segment[E, D, W] =
    super.getSegment(element)

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): SegmentSeq[E, D, W] = sliced(bound)._2

  final override def takenBelow(bound: Bound[E]): SegmentSeq[E, D, W] = sliced(bound)._1

  final override def sliced(bound: Bound[E]): (SegmentSeq[E, D, W], SegmentSeq[E, D, W]) = {
    val ord = domainOps.boundOrd
    if (ord.compare(bound, firstSegment.upperBound) <= 0) {
      (consUniform(firstSegment.value), this)

    } else if (ord.compare(bound, lastSegment.lowerBound) >= 0) {
      (this, consUniform(lastSegment.value))

    } else {
      val splitOutput =
        TreeSplit.foldNode[Bound.Upper[E], Bound[E], W](
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

  final override def appended(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = other match {
    case other: AbstractTreapSegmentSeq[E, D, W] => appendedTreapSeq(other)
    case _ => appendedSegmentSeq(other)
  }

  // TODO implement `appended` method.
  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = ???

  // Protected section -------------------------------------------------------- //
  protected final type TreapSegment = TreapSegmentBase with GenSegment

  /**
   * Treap of segments upper bounds.
   */
  protected val root: ImmutableTreap.Node[Bound.Upper[E], W]

  /** Value of last segment (without upper bound). */
  protected val lastValue: W

  /**
   * Creates uniform segment sequence (empty or universal).
   *
   * Note current class not supports empty and universal sets so other implementations should be used.
   */
  protected def consUniform(value: W): SegmentSeq[E, D, W]

  /**
   * Creates segment sequence from specified treap node.
   */
  protected def consFromNode(node: ImmutableTreap.Node[Bound.Upper[E], W], value: W): SegmentSeq[E, D, W]

  /**
   * Creates segment sequence from specified treap. If treap is empty, creates uniform sequence.
   */
  protected def consFromTree(tree: ImmutableTreap[Bound.Upper[E], W], value: W): SegmentSeq[E, D, W] = tree match {
    case node: ImmutableTreap.Node[Bound.Upper[E], W] => consFromNode(node, value)
    case _ => consUniform(value)
  }

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `W` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isIncludedInSet(value: W): Boolean

  /**
   * @return initial segment of sequence.
   */
  protected final def makeInitialSegment(): TreapInitialSegment = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
        root,
        TreeVisitStack.contextOps.getEmptyContext
      )(
        NodeAside.minKeyFunc(TreeVisitStack.function)
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
        NodeAside.maxKeyFunc(TreeVisitStack.function)
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
          NodeAside.nextKeyFunc[Bound.Upper[E], W, NodeVisitContext[Bound.Upper[E], W]](
            TreeVisitStack.function
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
        NodeAside.prevKeyFunc[Bound.Upper[E], W, NodeVisitContext[Bound.Upper[E], W]](
          TreeVisitStack.function
        )(
          TreeVisitStack.contextOps
        )
      )
    // We can use reference equality for immutable tree.
    if (firstSegment.node.eq(contextExtract.tree)) firstSegment
    else TreapInnerSegment(contextExtract.tree, contextExtract.context)
  }

  protected final def appendedTreapSeq(other: AbstractTreapSegmentSeq[E, D, W]): SegmentSeq[E, D, W] = {
    val originalPenultimateSegment = lastSegment.movePrev

    val appendedRoot =
      TreeSplit.foldNode[Bound.Upper[E], Bound[E], W](
        other.root,
        originalPenultimateSegment.upperBound,
        splitLeft = true,
        SplitOutput.Mutable.Output.initial
      )(
        domainOps.boundOrd
      ).rightTree

    appendedRoot match {
      case appendedRoot: ImmutableTreap.Node[Bound.Upper[E], W] =>
        val appendedExtract =
          ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeStackContext[Bound.Upper[E], W]](
            appendedRoot,
            TreeStack.contextOps[Bound.Upper[E], W, ImmutableTreap.Node].getEmptyContext
          )(
            NodeAside.minKeyFunc(TreeStack.function)
          )
        val appendedStack = TreeStack.contextOps.addToStack(appendedExtract.context, appendedExtract.tree)

        if (valueEq.eqv(originalPenultimateSegment.value, appendedExtract.tree.value)) {
          val originalRoot =
            TreeSplit.foldNode[Bound.Upper[E], Bound[E], W](
              root,
              originalPenultimateSegment.upperBound,
              splitLeft = false,
              SplitOutput.Mutable.Output.initial
            )(
              domainOps.boundOrd
            ).leftTree

          originalRoot match {
            case originalRoot: ImmutableTreap.Node[Bound.Upper[E], W] =>
              val originalExtract =
                ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeStackContext[Bound.Upper[E], W]](
                  originalRoot,
                  TreeStack.contextOps[Bound.Upper[E], W, ImmutableTreap.Node].getEmptyContext
                )(
                  NodeAside.maxKeyFunc(TreeStack.function)
                )
              val originalStack = TreeStack.contextOps.addToStack(originalExtract.context, originalExtract.tree)

              val mergedRoot =
                TreeMerge.merge[Bound.Upper[E], Bound[E], W](
                  originalStack,
                  appendedStack,
                  ImmutableTreap.Empty
                )(
                  Treap.nodePriorityOrder(domainOps.boundOrd)
                )
              consFromTree(mergedRoot, other.lastValue)

            case _ => other
          }
        } else {
          val originalStack = TreeStack.contextOps.addToStack(lastSegment.context.stack.map(_.tree), lastSegment.node)

          val mergedRoot =
            TreeMerge.merge[Bound.Upper[E], Bound[E], W](
              originalStack,
              appendedStack,
              ImmutableTreap.Empty
            )(
              Treap.nodePriorityOrder(domainOps.boundOrd)
            )
          consFromTree(mergedRoot, other.lastValue)
        }
      case _ =>
        if (valueEq.eqv(originalPenultimateSegment.value, other.lastValue))
          takenBelow(originalPenultimateSegment.upperBound)
        else
          this
    }
  }

  protected final def appendedSegmentSeq(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = {
    val originalPenultimateSegment = lastSegment.movePrev
    if (other.isUniform)
      if (valueEq.eqv(originalPenultimateSegment.value, other.firstSegment.value))
        takenBelow(originalPenultimateSegment.upperBound)
      else
        this
    else {
      val ord = domainOps.boundOrd
      val rng = rngManager.newUnsafeUniformRng()

      val appendedFirstSegment = other.getSegment(originalPenultimateSegment.upperBound.flip)

      val originalRoot =
        if (valueEq.eqv(originalPenultimateSegment.value, appendedFirstSegment.value))
          TreeSplit.foldNode[Bound.Upper[E], Bound[E], W](
            root,
            originalPenultimateSegment.upperBound,
            splitLeft = false,
            SplitOutput.Mutable.Output.initial
          )(
            domainOps.boundOrd
          ).leftTree
        else
          root

      val buffer = appendedFirstSegment.forwardIterable().foldLeft(
        BuildAsc.rightFrontToBuffer[Bound.Upper[E], W](originalRoot)
      ) {
        (buf, seg) => seg match {
          case seg: Segment.WithNext[E, D, W] =>
            BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], W](buf, seg.upperBound, rng.nextInt(), seg.value)(ord)
          case _ => buf
      }}
      consFromTree(BuildAsc.finalizeBuffer(buffer), other.lastSegment.value)
    }
  }

  /**
   * Base trait for non single segments. It has either previous segment or next.
   */
  protected sealed trait TreapSegmentBase extends SegmentLike[E, D, W] {

    val node: ImmutableTreap.Node[Bound.Upper[E], W]

    val context: NodeVisitContext[Bound.Upper[E], W]

    override def sequence: SegmentSeq[E, D, W] = seq
    
    override def domainOps: DomainOps[E, D] = seq.domainOps

    override def value: W = node.value

    override def isIncluded: Boolean = isIncludedInSet(value)

    override def moveToFirst: FirstSegment = firstSegment

    override def moveToLast: LastSegment = lastSegment

    override def moveTo(bound: Bound[E]): GenSegment = getSegment(bound)
  }

  /**
   * Segment which has next segment.
   */
  protected sealed trait TreapSegmentWithNext extends TreapSegmentBase with SegmentWithNext {

    override def upperBound: Bound.Upper[E] = node.key

    override def moveNext: TreapSegmentWithPrev = makeSegmentWithPrev(this)
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
          NodeAside.prevKeyFunc[Bound.Upper[E], W, NodeVisitContext[Bound.Upper[E], W]](
            TreeVisitStack.function
          )(
            TreeVisitStack.contextOps
          )
        )
      contextExtract.tree.key.flipUpper
    }

    override def movePrev: TreapSegmentWithNext = makeSegmentWithNext(this)
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

    override def movePrev: TreapSegmentWithNext =
      if (root.isLeaf) TreapInitialSegment(node, context)
      else TreapInnerSegment(node, context)
  }
}
