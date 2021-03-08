package ordset.core.domain

import ordset.util.label.Label
import ordset.Hash

trait DiscreteDomain[E, D <: Domain[E]] extends Domain.Wrapper[E, D] {

  val discrete: Discrete[E]

  override def labels: Set[Label] = super.labels + DomainLabels.Discrete
}

object DiscreteDomain {

  def apply[E, D <: Domain[E]](discrete: Discrete[E])(implicit domain: D): DiscreteDomain[E, D] =
    new DefaultImpl(discrete, domain)

  implicit def defaultHash[E, D <: Domain[E]](
    implicit discreteHash: Hash[Discrete[E]], domainHash: Hash[D]): Hash[DiscreteDomain[E, D]] =
    new DefaultHash[E, D]()(discreteHash, domainHash)

  final class DefaultImpl[E, D <: Domain[E]](
    override val discrete: Discrete[E],
    override val domain: D
  ) extends DiscreteDomain[E, D]

  final class DefaultHash[E, D <: Domain[E]]()(
    val discreteHash: Hash[Discrete[E]],
    val domainHash: Hash[D]
  ) extends Hash[DiscreteDomain[E, D]] {

    import ordset.util.HashUtil._

    override def eqv(x: DiscreteDomain[E, D], y: DiscreteDomain[E, D]): Boolean =
      discreteHash.eqv(x.discrete, y.discrete) && domainHash.eqv(x.domain, y.domain)

    override def hash(x: DiscreteDomain[E, D]): Int =
      product2Hash(discreteHash.hash(x.discrete), domainHash.hash(x.domain))
  }
}