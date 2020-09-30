package ordset.domain

import ordset.util.label.Label
import ordset.{Discrete, Interval, domain}

object Domains {

  def ContinuousUnbounded[E](
    implicit elementOrd: AscOrder[E]): Domain[E] =
    new Domain.DefaultImpl[E](DomainLabels.Continuous <+> DomainLabels.Unbounded, elementOrd)

  def ContinuousBounded[E](bounds: Interval[E, Domain[E]])(
    implicit elementOrd: AscOrder[E]): BoundedDomain[E, Domain[E]] =
    new BoundedDomain.DefaultImpl(
      bounds,
      new Domain.DefaultImpl[E](DomainLabels.Continuous, elementOrd)
    )

  def DiscreteUnbounded[E](discrete: Discrete[E])(
    implicit elementOrd: AscOrder[E]): DiscreteDomain[E, Domain[E]] =
    new DiscreteDomain.DefaultImpl(
      discrete,
      new Domain.DefaultImpl[E](DomainLabels.Unbounded, elementOrd)
    )

  def DiscreteBounded[E](discrete: Discrete[E], bounds: Interval[E, Domain[E]])(
    implicit elementOrd: AscOrder[E]): DiscreteDomain[E, BoundedDomain[E, Domain[E]]] =
    new domain.DiscreteDomain.DefaultImpl(
      discrete,
      new BoundedDomain.DefaultImpl(
        bounds,
        new Domain.DefaultImpl(Label.empty, elementOrd)
      )
    )
}
