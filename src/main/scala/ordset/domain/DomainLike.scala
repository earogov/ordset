package ordset.domain

import ordset.Bound
import ordset.util.label.Label

trait DomainLike[E] {

  def label: Label

  implicit def elementOrd: AscOrder[E]

  implicit def boundOrd: AscOrder[Bound[E]]
}

object DomainLike {

  trait Wrapper[E, D <: Domain[E]] extends DomainLike[E] {

    def domain: D

    override def label: Label = domain.label

    override implicit def elementOrd: AscOrder[E] = domain.elementOrd

    override implicit def boundOrd: AscOrder[Bound[E]] = domain.boundOrd
  }
}