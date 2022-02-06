package ordset.test.core.behaviors

import ordset.Hash
import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.test.core.SegmentSeqAssertions.assertSameSegmentSeq

class SegmentSeqValidator[E, D[X] <: Domain[X], V](
  val sequence: SegmentSeq[E, D, V]
) {

  implicit private val domainOps: DomainOps[E, D] = sequence.domainOps
  implicit private val valueOps: ValueOps[V] = sequence.valueOps

  /**
   * Checks that result of [[ordset.core.SegmentSeqT.takeBelowExtended]] is equal to
   * [[ordset.core.SegmentSeqT.takeBelowBound]] in case when `bound` is limited.
   * If successful returns the received sequence.
   */
  def takeBelowExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] = {
    val result1 = sequence.takeBelowExtended(bound)
    bound match {
      case bound: Bound[E] =>
        val result2 = sequence.takeBelowBound(bound)
        assertSameSegmentSeq(result1, result2)
      case _ => // nothing to check
    }
    result1
  }

  /**
   * Checks that result of [[ordset.core.SegmentSeqT.takeAboveExtended]] is equal to
   * [[ordset.core.SegmentSeqT.takeAboveBound]] in case when `bound` is limited.
   * If successful returns the received sequence.
   */
  def takeAboveExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] = {
    val result1 = sequence.takeAboveExtended(bound)
    bound match {
      case bound: Bound[E] =>
        val result2 = sequence.takeAboveBound(bound)
        assertSameSegmentSeq(result1, result2)
      case _ => // nothing to check
    }
    result1
  }

  /**
   * Checks that result of [[ordset.core.SegmentSeqT.sliceAtExtended]] is equal to
   * [[ordset.core.SegmentSeqT.sliceAtBound]] in case when `bound` is limited.
   * If successful returns the received sequences.
   */
  def sliceAtExtended(bound: ExtendedBound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = {
    val result1 = sequence.sliceAtExtended(bound)
    bound match {
      case bound: Bound[E] =>
        val result2 = sequence.sliceAtBound(bound)
        assertSameSegmentSeq(result1._1, result2._1)
        assertSameSegmentSeq(result1._2, result2._2)
      case _ => // nothing to check
    }
    result1
  }

  /**
   * Checks that result of [[ordset.core.SegmentSeqT.prependBelowExtended]] is equal to
   * [[ordset.core.SegmentSeqT.prependBelowBound]] in case when `bound` is limited.
   * If successful returns the received sequence.
   */
  def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
    val result1 = sequence.prependBelowExtended(bound, other)
    bound match {
      case bound: Bound[E] =>
        val result2 = sequence.prependBelowBound(bound, other)
        assertSameSegmentSeq(result1, result2, getInfo(bound))
      case _ => // nothing to check
    }
    result1
  }

  /**
   * Checks that result of [[ordset.core.SegmentSeqT.appendAboveExtended]] is equal to
   * [[ordset.core.SegmentSeqT.appendAboveBound]] in case when `bound` is limited.
   * If successful returns the received sequence.
   */
  def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
    val result1 = sequence.appendAboveExtended(bound, other)
    bound match {
      case bound: Bound[E] =>
        val result2 = sequence.appendAboveBound(bound, other)
        assertSameSegmentSeq(result1, result2, getInfo(bound))
      case _ => // nothing to check
    }
    result1
  }
  
  private def getInfo(bound: Bound[E]): String =
    s"bound $bound is limited, so expected the same result for methods that receives $Bound and $ExtendedBound"
}
