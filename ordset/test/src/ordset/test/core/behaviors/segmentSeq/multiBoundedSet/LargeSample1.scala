package ordset.test.core.behaviors.segmentSeq.multiBoundedSet

 import ordset.core.domain.Domain
 import ordset.core.{Bound, ExtendedBound, SegmentSeq}
 import ordset.core.interval.IntervalRelation
 import ordset.test.core.samples.segmentSeq.SegmentSeqSample

trait LargeSample1[D <: Domain[Int]] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  private val minIndex: Int = 1
  private val maxIndex: Int = 100
  private val step: Int = 100

  override val sample: String = "Large1"
  
  override val complementary: Boolean = false

  override val reference: Seq[GenIntervalRelation] =
    (minIndex to maxIndex).map { i => 
      val value = (i % 2) == 0
      val interval = 
        if (i == minIndex) domainOps.intervals.factory.belowBound(Bound.Upper(elementByIndex(i), false))
        else if (i == maxIndex) domainOps.intervals.factory.aboveBound(Bound.Lower(elementByIndex(i), true))
        else domainOps.intervals.factory.betweenBounds(Bound.Lower(i, true), Bound.Upper(elementByIndex(i + 1), false))
      IntervalRelation(interval, value)
    }

  private def elementByIndex(i: Int): Int = i * step
}
