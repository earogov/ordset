package ordset.domain

import ordset.util.label.Label
import ordset.{Bound, Hash, Show}

trait Domain[E] {

  def label: Label

  implicit def elementOrd: AscOrder[E]

  implicit def boundOrd: AscOrder[Bound[E]]
}

object Domain {

  implicit def defaultDomain[E](implicit elementOrd: AscOrder[E]): Domain[E] =
    Domains.ContinuousUnbounded(elementOrd)

  implicit def defaultHash[E, D <: Domain[E]](implicit ordHash: Hash[AscOrder[E]]): Hash[D] =
    new DefaultHash[E, D](ordHash)

  implicit def defaultShow[E, D <: Domain[E]](implicit orderShow: Show[AscOrder[E]]): Show[D] =
    new DefaultShow[E, D](Label.setBuilderShow, orderShow)

  trait Wrapper[E, D <: Domain[E]] extends Domain[E] {

    implicit val domain: D

    override def label: Label = domain.label

    override implicit def elementOrd: AscOrder[E] = domain.elementOrd

    override implicit def boundOrd: AscOrder[Bound[E]] = domain.boundOrd
  }

  final class DefaultImpl[E](override val label: Label, override val elementOrd: AscOrder[E]) extends Domain[E] {

    override implicit lazy val boundOrd: AscOrder[Bound[E]] = Bound.defaultAscOrder(elementOrd)
  }

  final class DefaultHash[E, D <: Domain[E]](ordHash: Hash[AscOrder[E]]) extends Hash[D] {

    import ordset.util.Hash._

    protected val labelHash: Hash[Label] = Label.defaultOrder

    override def eqv(x: D, y: D): Boolean = ordHash.eqv(x.elementOrd, y.elementOrd) && labelHash.eqv(x.label, y.label)

    override def hash(x: D): Int = product2Hash(ordHash.hash(x.elementOrd), labelHash.hash(x.label))
  }

  final class DefaultShow[E, D <: Domain[E]](labelShow: Show[Label], orderShow: Show[AscOrder[E]]) extends Show[D] {

    private val domainConst: String = "domain"
    private val orderConst: String = "order"
    private val withConst: String = "with"

    override def show(t: D): String = {
      var labelStr = labelShow.show(t.label)
      labelStr = if (labelStr.isEmpty) domainConst else s"$labelStr $domainConst"
      val orderStr = orderShow.show(t.elementOrd)
      if (orderStr.isEmpty) labelStr
      else s"$labelStr $withConst $orderStr $orderConst"
    }
  }
}