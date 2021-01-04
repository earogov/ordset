package ordset.domain

import ordset.util.label.Label
import ordset.{Bound, Hash, Show}

trait Domain[E] extends DomainLike[E]

object Domain {

  implicit def defaultDomain[E](implicit elementOrd: AscOrder[E]): Domain[E] =
    Domains.ContinuousUnbounded(elementOrd)

  implicit def defaultHash[E](implicit ordHash: Hash[AscOrder[E]]): Hash[Domain[E]] =
    defaultHashInstance.asInstanceOf[Hash[Domain[E]]]

  implicit def defaultShow[E](implicit ordShow: Show[AscOrder[E]]): Show[Domain[E]] =
    defaultShowInstance.asInstanceOf[Show[Domain[E]]]

  trait Wrapper[E, D <: Domain[E]] extends Domain[E] with DomainLike.Wrapper[E, D]

  final class DefaultImpl[E](
    override val label: Label,
    override val elementOrd: AscOrder[E],
  ) extends Domain[E] {

    override implicit val intOrd: AscOrder[Int] = ordset.instances.Int.intAscOrder

    override implicit val longOrd: AscOrder[Long] = ordset.instances.Long.longAscOrder

    override implicit val boundOrd: AscOrder[Bound[E]] = Bound.defaultAscOrder(elementOrd, intOrd)
  }

  final class DefaultHash[E, D <: Domain[E]](
    labelHash: Hash[Label],
    orderHash: Hash[DirectedOrder[_, _ <: OrderDir]]
  ) extends Hash[D] {

    import ordset.util.HashUtil._

    override def eqv(x: D, y: D): Boolean =
      orderHash.eqv(x.elementOrd, y.elementOrd) &&
        orderHash.eqv(x.intOrd, y.intOrd) &&
        orderHash.eqv(x.longOrd, y.longOrd) &&
        labelHash.eqv(x.label, y.label)

    override def hash(x: D): Int =
      product4Hash(
        orderHash.hash(x.elementOrd),
        orderHash.hash(x.intOrd),
        orderHash.hash(x.longOrd),
        labelHash.hash(x.label)
      )
  }

  final class DefaultShow[E, D <: Domain[E]](
    labelShow: Show[Label],
    orderShow: Show[DirectedOrder[_, _ <: OrderDir]]
  ) extends Show[D] {

    override def show(t: D): String =
      s"Domain(" +
        s"label: ${labelShow.show(t.label)}, " +
        s"elementOrd: ${orderShow.show(t.elementOrd)}, " +
        s"intOrd: ${orderShow.show(t.intOrd)}, " +
        s"longOrd: ${orderShow.show(t.longOrd)}, " +
        s"boundOrd: ${orderShow.show(t.boundOrd)}" +
        s")"
  }

  private lazy val defaultHashInstance: Hash[Domain[Any]] =
    new DefaultHash[Any, Domain[Any]](Label.defaultOrder, DirectedOrder.defaultHash)

  private lazy val defaultShowInstance: Show[Domain[Any]] =
    new DefaultShow[Any, Domain[Any]](Label.defaultShow, DirectedOrder.defaultShow)
}