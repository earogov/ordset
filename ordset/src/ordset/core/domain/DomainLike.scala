package ordset.core.domain

import ordset.core.{Bound, ExtendedBound}
import ordset.core.interval.{Interval, IntervalBuilder}
import ordset.util.label.{Label, LabeledEntity}

trait DomainLike[E] extends LabeledEntity {

  def lowerExtendedBound: ExtendedBound.Lower[E]

  def upperExtendedBound: ExtendedBound.Upper[E]

  implicit def elementOrd: AscOrder[E]

  implicit def intOrd: AscOrder[Int]

  implicit def longOrd: AscOrder[Long]

  implicit def boundOrd: AscOrder[Bound[E]]

  implicit def extendedOrd: AscOrder[ExtendedBound[E]]
}

object DomainLike {

  trait Proxy[E, D <: Domain[E]] extends DomainLike[E] {

    def domain: D

    override def lowerExtendedBound: ExtendedBound.Lower[E] = domain.lowerExtendedBound

    override def upperExtendedBound: ExtendedBound.Upper[E] = domain.upperExtendedBound

    override def labels: Set[Label] = domain.labels

    override implicit def elementOrd: AscOrder[E] = domain.elementOrd

    override implicit def intOrd: AscOrder[Int] = domain.intOrd

    override implicit def longOrd: AscOrder[Long] = domain.longOrd

    override implicit def boundOrd: AscOrder[Bound[E]] = domain.boundOrd

    override implicit def extendedOrd: AscOrder[ExtendedBound[E]] = domain.extendedOrd
  }

  final case class ProxyImpl[E, D <: Domain[E]](override val domain: D) extends Proxy[E, D]
}