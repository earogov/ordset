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

  implicit def defaultHash[E](implicit ordHash: Hash[AscOrder[E]]): Hash[Domain[E]] =
    defaultHashInstance.asInstanceOf[Hash[Domain[E]]]

  implicit def defaultShow[E](implicit ordShow: Show[AscOrder[E]]): Show[Domain[E]] =
    defaultShowInstance.asInstanceOf[Show[Domain[E]]]

  trait Wrapper[E, D <: Domain[E]] extends Domain[E] {

    implicit val domain: D

    override def label: Label = domain.label

    override implicit def elementOrd: AscOrder[E] = domain.elementOrd

    override implicit def boundOrd: AscOrder[Bound[E]] = domain.boundOrd
  }

  final class DefaultImpl[E](
    override val label: Label,
    override val elementOrd: AscOrder[E]
  ) extends Domain[E] {

    override implicit lazy val boundOrd: AscOrder[Bound[E]] = Bound.defaultAscOrder(elementOrd)
  }

  final class DefaultHash[E, D <: Domain[E]](
    labelHash: Hash[Label],
    ordHash: Hash[AscOrder[E]]
  ) extends Hash[D] {

    import ordset.util.Hash._

    override def eqv(x: D, y: D): Boolean = ordHash.eqv(x.elementOrd, y.elementOrd) && labelHash.eqv(x.label, y.label)

    override def hash(x: D): Int = product2Hash(ordHash.hash(x.elementOrd), labelHash.hash(x.label))
  }

  final class DefaultShow[E, D <: Domain[E]](
    labelShow: Show[Label],
    orderShow: Show[AscOrder[E]]
  ) extends Show[D] {

    override def show(t: D): String =
      s"Domain(label: ${labelShow.show(t.label)}, order: ${orderShow.show(t.elementOrd)})"
  }

  private lazy val defaultHashInstance: Hash[Domain[Any]] =
    new DefaultHash[Any, Domain[Any]](Label.defaultOrder, DirectedOrder.defaultHash[Any, AscDir])

  private lazy val defaultShowInstance: Show[Domain[Any]] =
    new DefaultShow[Any, Domain[Any]](Label.defaultShow, DirectedOrder.defaultShow[Any, AscDir])
}