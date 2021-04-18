package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}
import ordset.tree.core.Fold
import ordset.tree.core.eval.{TreeStack, TreeVisitStack}
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.Treap
import ordset.tree.treap.immutable.transform.TreeSplit.{splitLeftFunc, splitRightFunc}
import ordset.tree.treap.immutable.transform.{BuildAsc, SplitOutput, TreeMerge, TreeSplit}
import ordset.tree.treap.immutable.traverse.{NodeAside, NodeDownward, NodeUpward}
import ordset.tree.treap.immutable.{ImmutableTreap, NodeStackContext, NodeVisitContext}

// TODO: class description.
abstract class AbstractTreapSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  import AbstractTreapSegmentSeq._
  
  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = false

  final override def isUniversal: Boolean = false

  final override def isUniform: Boolean = false

  final override def contains(bound: Bound[E]): Boolean = getSegment(bound).isIncluded

  final override def contains(element: E): Boolean = super.contains(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = forwardUpperBoundsFromSegment(firstSegment)

  final override lazy val firstSegment: TreapInitialSegment[E, D, W] = makeInitialSegment()

  final override lazy val lastSegment: TreapTerminalSegment[E, D, W] = makeTerminalSegment()

  final override def getSegment(bound: Bound[E]): TreapSegmentBase[E, D, W] with GenSegment = {
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
          NodeUpward.foldToNextKey[Bound.Upper[E], W, NodeVisitContext[Bound.Upper[E], W]](
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

  final override def getSegment(element: E): Segment[E, D, W] =
    super.getSegment(element)

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): TreapSegmentSeq[E, D, W] = sliced(bound)._2

  final override def takenBelow(bound: Bound[E]): TreapSegmentSeq[E, D, W] = sliced(bound)._1

  final override def sliced(bound: Bound[E]): (TreapSegmentSeq[E, D, W], TreapSegmentSeq[E, D, W]) = {
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
  
  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = {
    
    // CASE 1                                              // CASE 2
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

    val originalBoundSegment = getSegment(upperBound)
    val otherBoundSegment = other.getSegment(lowerBound)

    val originalBoundMatch = originalBoundSegment.hasUpperBound(upperBound)
    val boundValuesMatch = originalBoundSegment.hasValue(otherBoundSegment.value)
    val skipBound = originalBoundMatch || boundValuesMatch
    
    val originalLeftSequence: TreapSegmentSeq[E, D, W] =
      if (originalBoundMatch && !boundValuesMatch)
        originalBoundSegment match {
          case s: TreapSegmentWithNext[E, D, W] => s.moveNext.takenBelow
          // `originalBoundMatch` == `true` => `originalBoundSegment` has upper bound => impossible to get here
          case _ => throw new AssertionError("Unreachable case: next segment expected.")
        }
      else originalBoundSegment.takenBelow
    
    val boundOrd = domainOps.boundOrd
    val rng = rngManager.newUnsafeUniformRng()

    val originalBuffer = BuildAsc.rightFrontToBuffer(getRoot(originalLeftSequence))
    val buffer =
      if (skipBound) originalBuffer
      else
        BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], W](
          originalBuffer, upperBound, rng.nextInt(), originalBoundSegment.value
        )(
          boundOrd
        )

    val leftRoot = BuildAsc.finalizeBuffer(buffer)

    leftRoot match {
      case leftRoot: ImmutableTreap.Node[Bound.Upper[E], W] =>
        if (otherBoundSegment.isLast) consFromNode(leftRoot, otherBoundSegment.value)
        else otherBoundSegment match {
          case otherBoundSegment: TreapSegmentWithNext[E, D, W] =>
            val rightSequence = otherBoundSegment.takenAbove
            val mergedRoot = TreeMerge.foldNodes(leftRoot, rightSequence.root)(Treap.nodePriorityOrder(boundOrd))
            consFromNode(mergedRoot, rightSequence.lastSegment.value)
          case _ =>
            val buffer = otherBoundSegment.forwardIterable().foldLeft(
              BuildAsc.rightFrontToBuffer[Bound.Upper[E], W](leftRoot)
            ) {
              (buf, seg) => seg match {
                case seg: Segment.WithNext[E, D, W] =>
                  BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], W](
                    buf, seg.upperBound, rng.nextInt(), seg.value
                  )(
                    boundOrd
                  )
                case _ => buf
              }
            }
            consFromTree(BuildAsc.finalizeBuffer(buffer), other.lastSegment.value)
        }
      case _ => otherBoundSegment.takenAbove
    }
  }

  // Protected section -------------------------------------------------------- //
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
  protected def consUniform(value: W): UniformSegmentSeq[E, D, W]

  /**
   * Creates segment sequence from specified treap node.
   */
  protected def consFromNode(node: ImmutableTreap.Node[Bound.Upper[E], W], value: W): AbstractTreapSegmentSeq[E, D, W]

  /**
   * Creates segment sequence from specified treap. If treap is empty, creates uniform sequence.
   */
  protected def consFromTree(tree: ImmutableTreap[Bound.Upper[E], W], value: W): TreapSegmentSeq[E, D, W] = 
    tree match {
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
  protected final def makeInitialSegment(): TreapInitialSegment[E, D, W] = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
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
  protected final def makeTerminalSegment(): TreapTerminalSegment[E, D, W] = {
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], W, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], W]](
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
  protected final def makeSegmentWithPrev(segment: TreapSegmentWithNext[E, D, W]): TreapSegmentWithPrev[E, D, W] =
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
      TreapInnerSegment(this, contextExtract.tree, contextExtract.context)
    }

  /**
   * @return segment which follows before input segment.
   */
  protected final def makeSegmentWithNext(segment: TreapSegmentWithPrev[E, D, W]): TreapSegmentWithNext[E, D, W] = {
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
    else TreapInnerSegment(this, contextExtract.tree, contextExtract.context)
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

        if (valueOps.eqv(originalPenultimateSegment.value, appendedExtract.tree.value)) {
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
        if (valueOps.eqv(originalPenultimateSegment.value, other.lastValue))
          takenBelow(originalPenultimateSegment.upperBound)
        else
          this
    }
  }

  protected final def appendedSegmentSeq(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = {
    val originalPenultimateSegment = lastSegment.movePrev
    if (other.isUniform)
      if (valueOps.eqv(originalPenultimateSegment.value, other.firstSegment.value))
        takenBelow(originalPenultimateSegment.upperBound)
      else
        this
    else {
      val ord = domainOps.boundOrd
      val rng = rngManager.newUnsafeUniformRng()

      val appendedFirstSegment = other.getSegment(originalPenultimateSegment.upperBound.flip)

      val originalRoot =
        if (valueOps.eqv(originalPenultimateSegment.value, appendedFirstSegment.value))
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
}

object AbstractTreapSegmentSeq {
  
  def getRoot[E, D <: Domain[E], W](seq: TreapSegmentSeq[E, D, W]): ImmutableTreap[Bound.Upper[E], W] = seq match {
    case s: AbstractTreapSegmentSeq[E, D, W] => s.root
    case s: UniformSegmentSeq[E, D, W] => ImmutableTreap.Empty
  }
  
  /**
   * Base trait for non single segments. It has either previous segment or next.
   */
  sealed trait TreapSegmentBase[E, D <: Domain[E], W] extends SegmentLike[E, D, W] {

    // Inspection --------------------------------------------------------------- //
    val node: ImmutableTreap.Node[Bound.Upper[E], W]

    val context: NodeVisitContext[Bound.Upper[E], W]

    override val sequence: AbstractTreapSegmentSeq[E, D, W]
    
    override def value: W = node.value

    override def isIncluded: Boolean = sequence.isIncludedInSet(value)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: TreapInitialSegment[E, D, W] = sequence.firstSegment

    override def moveToLast: TreapTerminalSegment[E, D, W] = sequence.lastSegment

    override def moveTo(bound: Bound[E]): TreapSegmentBase[E, D, W] with Segment[E, D, W] = 
      sequence.getSegment(bound)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: TreapSegmentSeq[E, D, W]

    override def takenBelow: TreapSegmentSeq[E, D, W]

    override def sliced: (TreapSegmentSeq[E, D, W], TreapSegmentSeq[E, D, W])
  }

  /**
   * Segment which has next segment.
   */
  sealed trait TreapSegmentWithNext[E, D <: Domain[E], W] 
    extends TreapSegmentBase[E, D, W]
      with Segment.WithNext[E, D, W] {

    // Inspection --------------------------------------------------------------- //
    override def upperBound: Bound.Upper[E] = node.key

    // Navigation --------------------------------------------------------------- //
    override def moveNext: TreapSegmentWithPrev[E, D, W] = sequence.makeSegmentWithPrev(this)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractTreapSegmentSeq[E, D, W]

    override def sliced: (TreapSegmentSeq[E, D, W], AbstractTreapSegmentSeq[E, D, W])
  }

  /**
   * Segment which has previous segment.
   */
  sealed trait TreapSegmentWithPrev[E, D <: Domain[E], W] 
    extends TreapSegmentBase[E, D, W] 
      with Segment.WithPrev[E, D, W] {

    // Inspection --------------------------------------------------------------- //
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

    // Navigation --------------------------------------------------------------- //
    override def movePrev: TreapSegmentWithNext[E, D, W] = sequence.makeSegmentWithNext(this)

    // Transformation ----------------------------------------------------------- //
    override def takenBelow: AbstractTreapSegmentSeq[E, D, W]

    override def sliced: (AbstractTreapSegmentSeq[E, D, W], TreapSegmentSeq[E, D, W])
  }

  /**
   * Initial segment of sequence.
   * 
   * @param sequence treap based segment sequence.
   * @param node treap node defining segment upper bound.
   * @param context path from treap root to `node`.
   */
  final case class TreapInitialSegment[E, D <: Domain[E], W] (
    override val sequence: AbstractTreapSegmentSeq[E, D, W],
    override val node: ImmutableTreap.Node[Bound.Upper[E], W],
    override val context: NodeVisitContext[Bound.Upper[E], W]
  ) extends TreapSegmentWithNext[E, D, W]  
    with Segment.Initial[E, D, W] {

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: TreapInitialSegment[E, D, W] = this

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractTreapSegmentSeq[E, D, W] = sequence

    override def takenBelow: AbstractUniformSegmentSeq[E, D, W] = sequence.consUniform(value)

    override def sliced: (AbstractUniformSegmentSeq[E, D, W], AbstractTreapSegmentSeq[E, D, W]) =
      (takenBelow, takenAbove)
  }

  /**
   * Terminal segment of sequence.
   *
   * @param sequence treap based segment sequence.
   * @param node treap node defining segment <u>lower</u> bound.
   * @param context path from treap root to `node`.
   */
  final case class TreapTerminalSegment[E, D <: Domain[E], W](
    override val sequence: AbstractTreapSegmentSeq[E, D, W],
    override val node: ImmutableTreap.Node[Bound.Upper[E], W],
    override val context: NodeVisitContext[Bound.Upper[E], W]
  ) extends TreapSegmentWithPrev[E, D, W]
    with Segment.Terminal[E, D, W] {

    // Inspection --------------------------------------------------------------- //
    override def value: W = sequence.lastValue

    override lazy val lowerBound: Bound.Lower[E] = node.key.flipUpper

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: TreapTerminalSegment[E, D, W] = this
    
    override def movePrev: TreapSegmentWithNext[E, D, W] =
      if (sequence.root.isLeaf) TreapInitialSegment(sequence, node, context)
      else TreapInnerSegment(sequence, node, context)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractUniformSegmentSeq[E, D, W] = sequence.consUniform(value)

    override def takenBelow: AbstractTreapSegmentSeq[E, D, W] = sequence

    override def sliced: (AbstractTreapSegmentSeq[E, D, W], AbstractUniformSegmentSeq[E, D, W]) =
      (takenBelow, takenAbove)
  }
  
  /**
   * Inner segment of sequence.
   *
   * @param sequence treap based segment sequence.
   * @param node treap node defining segment upper bound.
   * @param context path from treap root to `node`.
   */
  final case class TreapInnerSegment[E, D <: Domain[E], W](
    override val sequence: AbstractTreapSegmentSeq[E, D, W],
    override val node: ImmutableTreap.Node[Bound.Upper[E], W],
    override val context: NodeVisitContext[Bound.Upper[E], W]
  ) extends TreapSegmentWithNext[E, D, W]   
    with TreapSegmentWithPrev[E, D, W]  
    with Segment.Inner[E, D, W] {

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractTreapSegmentSeq[E, D, W] = sliced._2

    override def takenBelow: AbstractTreapSegmentSeq[E, D, W] = sliced._1

    override def sliced: (AbstractTreapSegmentSeq[E, D, W], AbstractTreapSegmentSeq[E, D, W]) = {
      // Generally we cann't just fold `context` of current node with split function.
      // Before we need to move down to get correct stack for split operation.
      //                                           
      //                                            segment
      //                                          |         |    ↙
      //                                          |         |  ↙
      //                                          |        node  -  split will be wrong if we just move upward
      //                                          |       ↙ |  ↘    from current node and apply split function
      //                                          |    ↙    |     ↘
      //   insteed we need to move upward   -   left        |       right
      //   from some child node to get          child       |       child
      //   correct split                          |         | 
      //                                        lower      upper
      //                                        bound      bound
      val contextExtract =
        NodeDownward.foldForRightSplit[Bound.Upper[E], Bound[E], W, NodeStackContext[Bound.Upper[E], W]](
          node,
          context.stack.map(_.tree),
          upperBound,
          TreeStack.function
        )(
          domainOps.boundOrd
        )
      val splitFunc = 
        TreeSplit.splitRightFunc[Bound.Upper[E], Bound[E], W, NodeStackContext[Bound.Upper[E], W]](
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
          NodeUpward.defaultFunc[Bound.Upper[E], W, NodeStackContext[Bound.Upper[E], W]](
            NodeUpward.StopPredicate.never,
            TreeStack.function
          )(
            TreeStack.contextOps
          ),
          splitFunc
        )
      // Segment is inner => `splitOutput.rightTree` and `splitOutput.leftTree` have at least one bound =>
      // they are not empty trees => cast is safe.
      val rightNode = splitOutput.rightTree.asInstanceOf[ImmutableTreap.Node[Bound.Upper[E], W]]
      val leftNode = splitOutput.leftTree.asInstanceOf[ImmutableTreap.Node[Bound.Upper[E], W]]
      
      val rightSeq = sequence.consFromNode(rightNode, sequence.lastValue)
      val leftSeq = sequence.consFromNode(leftNode, rightSeq.firstSegment.value)
      (leftSeq, rightSeq)
    }
  }
}