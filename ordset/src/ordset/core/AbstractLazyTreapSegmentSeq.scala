package ordset.core

import ordset.core.domain.Domain

abstract class AbstractLazyTreapSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  // Protected section -------------------------------------------------------- //
  protected val masterSeq: AbstractTreapSegmentSeq[E, D, MasterValue]


  protected sealed trait MasterValue

  protected sealed class EagerValue(private val value: W) extends MasterValue {

    def get(): W = value
  }

  protected sealed class LazySegmentSeq(private val fn: () => SegmentSeq[E, D, W]) extends MasterValue {

    def get(): SegmentSeq[E, D, W] = fn.apply()
  }

  protected sealed trait EagerSegmentBase extends SegmentLike[E, D, W] with MasterValue {

    val masterSegment: Segment[E, D, MasterValue]
  }
}