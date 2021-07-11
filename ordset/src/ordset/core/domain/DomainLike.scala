package ordset.core.domain

import ordset.core.{Bound, ExtendedBound}
import ordset.util.label.Label

trait DomainLike[E] {

  def labels: Set[Label]

  implicit def elementOrd: AscOrder[E]

  implicit def intOrd: AscOrder[Int]

  implicit def longOrd: AscOrder[Long]

  implicit def boundOrd: AscOrder[Bound[E]]

  implicit def extendedOrd: AscOrder[ExtendedBound[E]]
}

object DomainLike {

  trait Wrapper[E, D <: Domain[E]] extends DomainLike[E] {

    def domain: D

    override def labels: Set[Label] = domain.labels

    override implicit def elementOrd: AscOrder[E] = domain.elementOrd

    implicit def intOrd: AscOrder[Int] = domain.intOrd

    implicit def longOrd: AscOrder[Long] = domain.longOrd

    override implicit def boundOrd: AscOrder[Bound[E]] = domain.boundOrd

    override implicit def extendedOrd: AscOrder[ExtendedBound[E]] = domain.extendedOrd
  }
}