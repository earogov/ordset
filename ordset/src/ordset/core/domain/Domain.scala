package ordset.core.domain

import ordset.core.Bound
import ordset.util.label.Label
import ordset.{Hash, Show}

trait Domain[E] extends DomainLike[E]

object Domain {

  implicit def defaultDomain[E](implicit elementOrd: AscOrder[E]): Domain[E] =
    Domains.ContinuousUnbounded(elementOrd)

  implicit def defaultHash[E](): Hash[Domain[E]] =
    defaultHashInstance.asInstanceOf[Hash[Domain[E]]]

  implicit def defaultShow[E](): Show[Domain[E]] =
    defaultShowInstance.asInstanceOf[Show[Domain[E]]]

  trait Wrapper[E, D <: Domain[E]] extends Domain[E] with DomainLike.Wrapper[E, D]

  final class DefaultImpl[E](
    override val labels: Set[Label],
    override val elementOrd: AscOrder[E],
  ) extends Domain[E] {

    override implicit val intOrd: AscOrder[Int] = ordset.core.instances.Int.intAscOrder

    override implicit val longOrd: AscOrder[Long] = ordset.core.instances.Long.longAscOrder

    override implicit val boundOrd: AscOrder[Bound[E]] = Bound.defaultAscOrder(elementOrd, intOrd)
  }

  final class DefaultHash[E, D <: Domain[E]](
    orderHash: Hash[DirectedOrder[_, _ <: OrderDir]]
  ) extends Hash[D] {

    import ordset.util.HashUtil._

    override def eqv(x: D, y: D): Boolean =
      orderHash.eqv(x.elementOrd, y.elementOrd) &&
        orderHash.eqv(x.intOrd, y.intOrd) &&
        orderHash.eqv(x.longOrd, y.longOrd) &&
        Label.defaultSetHash.eqv(x.labels, y.labels)

    override def hash(x: D): Int =
      product4Hash(
        orderHash.hash(x.elementOrd),
        orderHash.hash(x.intOrd),
        orderHash.hash(x.longOrd),
        Label.defaultSetHash.hash(x.labels)
      )
  }

  final class DefaultShow[E, D <: Domain[E]](
    labelsShow: Show[Set[Label]],
    orderShow: Show[DirectedOrder[_, _ <: OrderDir]]
  ) extends Show[D] {

    override def show(t: D): String =
      s"Domain(" +
        s"labels: ${labelsShow.show(t.labels)}, " +
        s"elementOrd: ${orderShow.show(t.elementOrd)}, " +
        s"intOrd: ${orderShow.show(t.intOrd)}, " +
        s"longOrd: ${orderShow.show(t.longOrd)}, " +
        s"boundOrd: ${orderShow.show(t.boundOrd)}" +
        s")"
  }

  private lazy val defaultHashInstance: Hash[Domain[Any]] =
    new DefaultHash[Any, Domain[Any]](DirectedOrder.defaultHash)

  private lazy val defaultShowInstance: Show[Domain[Any]] =
    new DefaultShow[Any, Domain[Any]](Label.defaultSetShow, DirectedOrder.defaultShow)
}