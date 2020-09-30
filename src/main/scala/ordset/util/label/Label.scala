package ordset.util.label

import ordset.{Hash, Order, Show}

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, TreeSet}

/**
 * Labels are used to identify abstract entities such that orders, domains etc. Label assigned to entity may be used
 * for equality checks and hash calculations. Also it can be included in string representation of entity.
 *
 * Labels support two composition operations:
 * - Unordered
 *   Creates set of labels:
 *                               a <+> b == b <+> a
 *                               a <+> a == a
 * - Ordered
 *   Creates sequence of labels:
 *                               a +> b != b +> a    (if a != b, a != Empty, b != Empty)
 *                               a +> a != a
 *
 * @note
 * - Empty label is a zero element for all composition operations:
 *                               Empty <+> a == a <+> Empty == a
 *                               Empty +> a == a +> Empty == a
 *
 * - Labels set is not equivalent to sequence even if all their elements are the same:
 *                               a <+> b != a +> b
 *
 * @example
 * 1. We can assign labels to different properties of domains: Bounded, Discrete, Continuous ...
 *    Order of properties doesn't matter for a new domain, so its label should be created with unordered composition:
 *    Bounded <+> Continuous
 * 2. Consider some tuple: (personId, personName, Salary)
 *    Suppose we have orderings for each field with labels: ById_Asc, ByName_Asc, BySalary_Desc.
 *    Composing them in different ways we will get different orderings. So label of composed ordering should have
 *    the same property:
 *    BySalary_Desc +> ByName_Asc != ByName_Asc +> BySalary_Desc
 */
sealed trait Label extends Comparable[Label] {

  def tokens: LazyList[Token]

  def <+>(that: Label): Label

  def +>(that: Label): Label

  override def toString: String = Label.defaultShow.show(this)

  override def compareTo(o: Label): Int = Label.defaultOrder.compare(this, o)

  override def equals(obj: Any): Boolean = obj match {
    case obj: Label => Label.defaultOrder.eqv(this, obj)
    case _ => false
  }

  override def hashCode(): Int = Label.defaultOrder.hash(this)
}

object Label {

  def empty: Label = EmptyLabel

  def apply(value: String): Label = new UnitLabel(value)

  def defaultShow: Show[Label] = labelBuilderShow

  implicit lazy val labelBuilderShow: Show[Label] =
    new DefaultShow(Token.LabelBuilderShow)

  lazy val setBuilderShow: Show[Label] =
    new DefaultShow(Token.SetBuilderShow)

  lazy val commaSeparatedShow: Show[Label] =
    new DefaultShow(Token.CommaSeparatedShow)

  implicit lazy val defaultOrder: Order[Label] with Hash[Label] =
    new DefaultOrder(Token.defaultOrder, Token.defaultOrder)

  final class DefaultOrder(
    val tokenOrder: Order[Token],
    val tokenHash: Hash[Token]
  ) extends Order[Label] with Hash[Label] {

    import ordset.instances.LazyList.{lazyListHash, lazyListOrder}

    private val tokensOrder: Order[LazyList[Token]] = lazyListOrder(tokenOrder)

    private val tokensHash: Hash[LazyList[Token]] = lazyListHash(tokenHash)

    override def compare(x: Label, y: Label): Int = tokensOrder.compare(x.tokens, y.tokens)

    override def eqv(x: Label, y: Label): Boolean = tokensOrder.eqv(x.tokens, y.tokens)

    override def hash(x: Label): Int = tokensHash.hash(x.tokens)
  }

  final class DefaultShow(
    val tokenShow: Show[Token]
  ) extends Show[Label] {

    override def show(t: Label): String = {
      val builder = new StringBuilder
      t.tokens.foreach(x => builder.append(Token.defaultShow.show(x)))
      builder.result()
    }
  }

  private object EmptyLabel extends Label {

    override def tokens: LazyList[Token] = LazyList.empty

    override def <+>(that: Label): Label = that

    override def +>(that: Label): Label = that
  }

  private class UnitLabel(val value: String) extends Label {

    override def tokens: LazyList[Token] = LazyList(Token.Element(value))

    override def <+>(that: Label): Label = that match {
      case EmptyLabel => this
      case that: LabelSet => LabelSet.fromLabelSet(that, this)
      case _ => LabelSet.fromLabels(this, that)
    }

    override def +>(that: Label): Label = that match {
      case EmptyLabel => this
      case that: LabelSeq => LabelSeq.fromLabelsSeqPrepending(this, that)
      case _ => LabelSeq.fromLabels(this, that)
    }
  }

  private abstract class ComposedLabel(val labels: Iterable[Label]) extends Label {

    override def tokens: LazyList[Token] = {
      @tailrec
      def merge(head: LazyList[Token], iterator: Iterator[Label], index: Int): LazyList[Token] =
        if (iterator.hasNext) {
          val ind = index + 1
          val ll = LazyList.from(iterator.next().tokens)
          if (index == 0) merge(head.appendedAll(ll), iterator, ind)
          else merge(head.appended(separatorToken).appendedAll(ll), iterator, ind)
        } else head
      merge(LazyList(openToken), labels.iterator, 0).appended(closeToken)
    }

    protected def openToken: Token

    protected def closeToken: Token

    protected def separatorToken: Token
  }

  private class LabelSet(override val labels: TreeSet[Label]) extends ComposedLabel(labels) {

    override def <+>(that: Label): Label = that match {
      case EmptyLabel => this
      case that: LabelSet => LabelSet.fromLabelSets(this, that)
      case _ => LabelSet.fromLabelSet(this, that)
    }

    override def +>(that: Label): Label = that match {
      case EmptyLabel => this
      case that: LabelSeq => LabelSeq.fromLabelsSeqPrepending(this, that)
      case _ => LabelSeq.fromLabels(this, that)
    }

    override protected def openToken: Token = Token.SetOpen

    override protected def closeToken: Token = Token.SetClose

    override protected def separatorToken: Token = Token.SetSeparator
  }

  private object LabelSet {

    private lazy val DefaultOrdering: Ordering[Label] = defaultOrder.toOrdering

    def fromLabels(x: Label, y: Label): Label =
      if (DefaultOrdering.equiv(x, y)) x
      else new LabelSet(TreeSet.empty[Label](DefaultOrdering).incl(x).incl(y))

    def fromLabelSet(x: LabelSet, y: Label): Label = new LabelSet(x.labels.incl(y))

    def fromLabelSets(x: LabelSet, y: LabelSet): Label = new LabelSet(x.labels.concat(y.labels))
  }

  private class LabelSeq(override val labels: Queue[Label]) extends ComposedLabel(labels) {

    override def <+>(that: Label): Label = that match {
      case EmptyLabel => this
      case that: LabelSet => LabelSet.fromLabelSet(that, this)
      case _ => LabelSet.fromLabels(this, that)
    }

    override def +>(that: Label): Label = that match {
      case EmptyLabel => this
      case that: LabelSeq => LabelSeq.fromLabelSeqs(this, that)
      case _ => LabelSeq.fromLabelSeqAppending(this, that)
    }

    override protected def openToken: Token = Token.SeqOpen

    override protected def closeToken: Token = Token.SeqClose

    override protected def separatorToken: Token = Token.SeqSeparator
  }

  private object LabelSeq {

    def fromLabels(x: Label, y: Label): Label = new LabelSeq(Queue.empty[Label].appended(x).appended(y))

    def fromLabelsSeqPrepending(x: Label, y: LabelSeq): Label = new LabelSeq(y.labels.prepended(x))

    def fromLabelSeqAppending(x: LabelSeq, y: Label): Label = new LabelSeq(x.labels.appended(y))

    def fromLabelSeqs(x: LabelSeq, y: LabelSeq): Label = new LabelSeq(x.labels.appendedAll(y.labels))
  }
}