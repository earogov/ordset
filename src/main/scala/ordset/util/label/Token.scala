package ordset.util.label

import ordset.{Hash, Order, Show}

sealed trait Token extends Comparable[Token] {

  override def toString: String = Token.defaultShow.show(this)

  override def compareTo(o: Token): Int = Token.defaultOrder.compare(this, o)
}

object Token {

  def apply(value: String): Element = Element(value)

  implicit def defaultShow: Show[Token] = LabelBuilderShow

  implicit def defaultOrder: Order[Token] with Hash[Token] = DefaultOrder

  case object SetOpen extends Token

  case object SetClose extends Token

  case object SetSeparator extends Token

  case object SeqOpen extends Token

  case object SeqClose extends Token

  case object SeqSeparator extends Token

  case class Element(value: String) extends Token

  object SetBuilderShow extends Show[Token] {

    override def show(t: Token): String = t match {
      case SetOpen => "{"
      case SetClose => "}"
      case SetSeparator => ", "
      case SeqOpen => "("
      case SeqClose => ")"
      case SeqSeparator => ", "
      case Element(v) => v
    }
  }

  object LabelBuilderShow extends Show[Token] {

    override def show(t: Token): String = t match {
      case SetOpen => "("
      case SetClose => ")"
      case SetSeparator => " <+> "
      case SeqOpen => "("
      case SeqClose => ")"
      case SeqSeparator => " +> "
      case Element(v) => v
      case _ => ""
    }
  }

  object CommaSeparatedShow extends Show[Token] {

    override def show(t: Token): String = t match {
      case SetSeparator => ", "
      case SeqSeparator => ", "
      case Element(v) => v
      case _ => ""
    }
  }

  object DefaultOrder extends Order[Token] with Hash[Token] {

    import ordset.instances.Int._
    import ordset.instances.String._

    override def compare(x: Token, y: Token): Int = (x, y) match {
      case (Element(vx), Element(vy)) => stringOrder.compare(vx, vy)
      case _ => intOrder.compare(getIndex(x), getIndex(y))
    }

    override def eqv(x: Token, y: Token): Boolean = x.equals(y)

    override def hash(x: Token): Int = x.hashCode()

    private def getIndex(t: Token): Int = t match {
      case SetOpen => 1
      case SetSeparator => 2
      case SetClose => 3
      case SeqOpen => 4
      case SeqSeparator => 5
      case SeqClose => 6
      case Element(_) => 7
    }
  }
}
