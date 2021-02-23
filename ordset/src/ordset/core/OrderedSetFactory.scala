package ordset.core

import ordset.core.domain.Domain

trait OrderedSetFactory[E, D <: Domain[E]] {

  @throws[SegmentSeqException]("if unable to build valid set with specified bounds")
  def buildUnsafe(bounds: IterableOnce[Bound.Upper[E]], complementary: Boolean): OrderedSet[E, D]
}
