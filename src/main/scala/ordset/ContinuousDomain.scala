package ordset

trait ContinuousDomain[E] extends Domain[E]

object ContinuousDomain {

  def apply[E]()(implicit elementOrd: AscOrder[E]): ContinuousDomain[E] = DomainImpl(elementOrd)

  private case class DomainImpl[E](
      override final val elementOrd: AscOrder[E]
  ) extends ContinuousDomain[E]
}
