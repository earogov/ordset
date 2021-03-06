package ordset.core.domain

import ordset.core.Interval

object Domains {

  def continuousUnbounded[E](
    implicit elementOrd: AscOrder[E]
  ): Domain[E] =
    new Domain.DefaultImpl[E](Set(DomainLabels.Continuous, DomainLabels.Unbounded), elementOrd)

  def continuousBounded[E](bounds: Interval[E, Domain[E]])(
    implicit elementOrd: AscOrder[E]
  ): BoundedDomain[E, Domain[E]] =
    new BoundedDomain.DefaultImpl(
      bounds,
      new Domain.DefaultImpl[E](Set(DomainLabels.Continuous), elementOrd)
    )

  def discreteUnbounded[E](discrete: Discrete[E])(
    implicit elementOrd: AscOrder[E]
  ): DiscreteDomain[E, Domain[E]] =
    new DiscreteDomain.DefaultImpl(
      discrete,
      new Domain.DefaultImpl[E](Set(DomainLabels.Unbounded), elementOrd)
    )

  def discreteBounded[E](discrete: Discrete[E], bounds: Interval[E, Domain[E]])(
    implicit elementOrd: AscOrder[E]
  ): DiscreteDomain[E, BoundedDomain[E, Domain[E]]] =
    new ordset.core.domain.DiscreteDomain.DefaultImpl(
      discrete,
      new BoundedDomain.DefaultImpl(
        bounds,
        new Domain.DefaultImpl(Set.empty, elementOrd)
      )
    )
}
