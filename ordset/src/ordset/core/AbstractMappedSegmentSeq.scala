//package ordset.core
//
//import ordset.core.domain.Domain
//
///**
// *         A              B                C
// * X-------------](--------------)[---------------X
// *         |              |               |
// *         V              V               V
// *         
// *         A              A               C
// * X-----------------------------)[---------------X
// */
//abstract class AbstractMappedSegmentSeq[E, D <: Domain[E], V, U] 
//  extends AbstractSegmentSeq[E, D, AbstractMappedSegmentSeq.MappedSegment[E, D, V, U]] {
//  seq =>
//  
//  protected val mapFunc: V => U
//}
//
//object AbstractMappedSegmentSeq {
//
//  type MappedSegment[E, D <: Domain[E], V, U] =
//    SegmentT[E, D, V, MappedSegmentBase[E, D, V, U]] with MappedSegmentBase[E, D, V, U]
//  
//  sealed trait MappedSegmentBase[E, D <: Domain[E], V, U]
//    extends SegmentLikeT[E, D, V, MappedSegmentBase[E, D, V, U]] {
//    
//    val front: Segment[E, D, V]
//  }
//}