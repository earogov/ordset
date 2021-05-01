//package ordset.core
//
//import ordset.core.domain.Domain
//
//import java.util.concurrent.atomic.AtomicReference
//import javax.swing.UIDefaults.LazyValue
//
//abstract class AbstractLazyTreapSegmentSeq[E, D <: Domain[E], V] extends AbstractSegmentSeq[E, D, V] { seq =>
//
//  /**
//   * {{{
//   *                full-defined
//   *  partial-defined    |   partial-defined
//   *               v     v    v
//   *   X----?---][-A-][-B-][-C-][-----?------X
//   *        ^                         ^
//   *     undefined                 undefined
//   * }}}
//   */
//
//  // Protected section -------------------------------------------------------- //
//  protected final type MasterValue = AbstractLazyTreapSegmentSeq.MasterValue[E, D, V]
//  protected final type EagerValue = AbstractLazyTreapSegmentSeq.EagerValue[E, D, V]
//  protected final type LazySegmentSeq = AbstractLazyTreapSegmentSeq.LazySegmentSeq[E, D, V]
//  
//  protected final type MasterSegment = Segment[E, D, MasterValue]
//  protected final type MasterSegmentWithPrev = Segment.WithPrev[E, D, MasterValue]
//  protected final type MasterSegmentWithNext = Segment.WithNext[E, D, MasterValue]
//  
//  protected final type OutputSegment = OutputSegmentBase with GenSegment
//  
//  protected val masterRef: AtomicReference[AbstractTreapSegmentSeq[E, D, MasterValue]]
//
//  protected final def eval(lazySegment: Segment[E, D, LazySegmentSeq]) = {
////    val segmentSeq = lazySegment.value.eval()
////    val masterSeg = masterRef.get();
////    
////    val (leftMasterSeq, tailMasterSeq) = masterSeg.sliced(lazySegment.l)
//  }
//  
//  protected final def makeSegmentWithPrev(masterSegment: MasterSegment): OutputSegmentWithPrev = 
////    masterSegment match {
////      case masterSegment: SegmentWithNext =>
////        val next = masterSegment.moveNext
////        next.value match {
////          case nextValue: EagerValue[E, D, V] => ???
////          case nextValue: LazySegmentSeq[E, D, V] => ???  
////        }
////      case _ => ???
//  
//  protected sealed trait OutputSegmentBase extends SegmentLike[E, D, V] {
//
//    val masterSegment: MasterSegment
//  }
//  
//  protected sealed trait OutputSegmentWithNext extends OutputSegmentBase with SegmentWithNext {
//    
//  }
//
//  protected sealed trait OutputSegmentWithPrev extends OutputSegmentBase with SegmentWithPrev {
//
//  }
//}
//
//object AbstractLazyTreapSegmentSeq {
//
//  sealed trait MasterValue[E, D <: Domain[E], V]
//  
//  trait EagerValue[E, D <: Domain[E], V] extends MasterValue[E, D, V] {
//    
//    def get(): V
//  }
//  
//  trait LazySegmentSeq[E, D <: Domain[E], V] extends MasterValue[E, D, V] {
//    
//    def eval(): SegmentSeq[E, D, V]
//  }
//}