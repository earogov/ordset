package ordset.domain

import ordset.util.label.Label
import ordset.{Hash, Interval}

trait BoundedDomain[E, D <: Domain[E]] extends Domain.Wrapper[E, D] {

  val bounds: Interval[E, D]

  override def label: Label = DomainLabels.Bounded <+> super.label
}

object BoundedDomain {

  def apply[E, D <: Domain[E]](bounds: Interval[E, D])(implicit domain: D): BoundedDomain[E, D] =
    new DefaultImpl(bounds, domain)

  implicit def defaultHash[E, D <: Domain[E]](
    implicit boundsHash: Hash[Interval[E, D]], domainHash: Hash[D]): Hash[BoundedDomain[E, D]] =
    new DefaultHash[E, D]()(boundsHash, domainHash)

  final class DefaultImpl[E, D <: Domain[E]](
    override val bounds: Interval[E, D],
    override val domain: D
  ) extends BoundedDomain[E, D]

  final class DefaultHash[E, D <: Domain[E]]()(
    implicit val boundsHash: Hash[Interval[E, D]],
    val domainHash: Hash[D]
  ) extends Hash[BoundedDomain[E, D]] {

    import ordset.util.HashUtil._

    override def eqv(x: BoundedDomain[E, D], y: BoundedDomain[E, D]): Boolean =
      boundsHash.eqv(x.bounds, y.bounds) && domainHash.eqv(x.domain, y.domain)

    override def hash(x: BoundedDomain[E, D]): Int =
      product2Hash(boundsHash.hash(x.bounds), domainHash.hash(x.domain))
  }
}