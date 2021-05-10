package ordset.core.set

import ordset.core.domain.Domain
import ordset.core.{Bound, SegmentSeqException}

trait OrderedSetFactory[E, D <: Domain[E]] {

  @throws[SegmentSeqException]("if unable to build valid set with specified bounds")
  def buildUnsafe(bounds: IterableOnce[Bound.Upper[E]], complementary: Boolean): OrderedSet[E, D]
}
