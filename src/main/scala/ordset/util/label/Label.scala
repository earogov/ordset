package ordset.util.label

import ordset.{Hash, Order, Show}

import scala.collection.immutable.{Queue, TreeSet}

/**
 * Labels are used to distinguish abstract entities such that orders, domains etc. Each abstract instance is equipped
 * with some label which is used for equality checks and hash calculations. Also label can be included in string
 * representation of entity.
 * Labels support two composition operations:
 * - unordered:   a <+> b == b <+> a
 * - ordered:     a +> b != b +> a    (if a != b)
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
sealed trait Label {

  def values: List[String]

  def <+>(that: Label): Label

  def +>(that: Label): Label
}

object Label {

  lazy val Empty: Label = new EmptyLabel

  def apply(value: String): Label = new UnitLabel(value)

  implicit lazy val defaultOrder: Order[Label] with Hash[Label] = new DefaultOrder

  implicit lazy val defaultShow: Show[Label] = Show.fromToString

  val commaSeparatedShow: Show[Label] = new CommaSeparatedShow

  final class DefaultOrder extends Order[Label] with Hash[Label] {

    private val valuesOrdering: Ordering[List[String]] = Ordering.Implicits.seqOrdering(Ordering.String)

    override def compare(x: Label, y: Label): Int = valuesOrdering.compare(x.values, y.values)

    override def eqv(x: Label, y: Label): Boolean = valuesOrdering.equiv(x.values, y.values)

    override def hash(x: Label): Int = x.values.hashCode()
  }

  final class CommaSeparatedShow extends Show[Label] {

    override def show(t: Label): String = t.values.mkString(",")
  }

  private class EmptyLabel() extends Label {

    override def values: List[String] = List.empty

    override def <+>(that: Label): Label = LabelSet(that)

    override def +>(that: Label): Label = LabelSeq(that)

    override def toString: String = ""
  }

  private class UnitLabel(val value: String) extends Label {

    override val values: List[String] = List(value)

    override def <+>(that: Label): Label = LabelSet(this, that)

    override def +>(that: Label): Label = LabelSeq(this, that)

    override def toString: String = value
  }

  private class LabelSet(val labels: TreeSet[Label]) extends Label {

    override lazy val values: List[String] = labels.toList.flatMap(_.values)

    override def <+>(that: Label): Label = that match {
      case that: LabelSet => new LabelSet(that.labels.concat(this.labels))
      case _ => new LabelSet(this.labels.incl(that))
    }

    override def +>(that: Label): Label = that match {
      case that: LabelSeq => new LabelSeq(that.labels.prepended(this))
      case _ => LabelSeq(this, that)
    }

    override def toString: String = values.mkString(" <+> ")
  }

  private object LabelSet {

    private lazy val DefaultOrdering: Ordering[Label] = defaultOrder.toOrdering

    def apply(x: Label): LabelSet = new LabelSet(TreeSet.empty[Label](DefaultOrdering).incl(x))

    def apply(x: Label, y: Label): LabelSet = new LabelSet(TreeSet.empty[Label](DefaultOrdering).incl(x).incl(y))
  }

  private class LabelSeq(val labels: Queue[Label]) extends Label {

    override lazy val values: List[String] = labels.toList.flatMap(_.values)

    override def <+>(that: Label): Label = that match {
      case that: LabelSet => new LabelSet(that.labels.incl(this))
      case _ => LabelSet(this, that)
    }

    override def +>(that: Label): Label = that match {
      case that: LabelSeq => new LabelSeq(that.labels.appendedAll(this.labels))
      case _ => new LabelSeq(this.labels.appended(that))
    }

    override def toString: String = values.mkString(" +> ")
  }

  private object LabelSeq {

    def apply(x: Label): LabelSeq = new LabelSeq(Queue.empty[Label].appended(x))

    def apply(x: Label, y: Label): LabelSeq = new LabelSeq(Queue.empty[Label].appended(x).appended(y))
  }
}