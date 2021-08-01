package test.ordset.core.samples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

abstract class LazyTreapShuffledSeqSample[E, D <: Domain[E], V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager
) extends LazyTreapSeqSample[E, D, V] {

  /**
   * Returns lazy sequence.
   * 
   * Each time method called it returns equivalent lazy sequence but with random internal state:
   * different parts of sequence may be lazy or eager.
   */
  override def sequence: LazyTreapSeqSample.LazyTreapSegmentSeq[E, D, V] =
    LazyTreapSeqUtil.shuffleLazySeq(initializeSequence, extendedBounds)
}

