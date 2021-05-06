//package ordset.core
//
//import ordset.core.domain.Domain
//
//import java.util.concurrent.atomic.AtomicReference
//
//abstract class AbstractLazyTreapSegmentSeq[E, D <: Domain[E], V] extends AbstractSegmentSeq[E, D, V] { seq =>
//
//  import AbstractLazyTreapSegmentSeq._
//
//  /**
//   * {{{
//   *               eager stable
//   *   eager unstable   |    eager unstable
//   *               v    v     v
//   *   X----?---][-A-][-B-][-C-][-----?------X
//   *        ^                         ^
//   *      lazy                      lazy
//   * }}}
//   */
//
//  // Protected section -------------------------------------------------------- //
//  protected val masterRef: AtomicReference[AbstractTreapSegmentSeq[E, D, MasterValue[E, D, V]]]
//
//  protected final def eval(lazySegment: Segment[E, D, LazySegmentSeq[E, D, V]]) = {
//    val segmentSeq = lazySegment.value.eval()
//    
//    masterRef.synchronized {
//      val masterSeq = masterRef.get();
//      
//    }
//  }
//
//  protected final def makeSegmentWithPrev(masterSegment: MasterSegment): OutputSegmentWithPrev = ???
//}
//
//object AbstractLazyTreapSegmentSeq {
//
//  sealed trait MasterValue[E, D <: Domain[E], V] {
//
//    def isStable: Boolean
//
//    def isUnstable: Boolean
//
//    def isEager: Boolean
//
//    def isLazy: Boolean
//  }
//
//  protected sealed trait EagerValue[E, D <: Domain[E], V] extends MasterValue[E, D, V] {
//
//    final override def isEager: Boolean = true
//
//    final override def isLazy: Boolean = false
//
//    def value: V
//  }
//
//  protected final case class StableEagerValue[E, D <: Domain[E], V](
//    final override val value: V
//  ) extends EagerValue[E, D, V] {
//
//    override def isStable: Boolean = true;
//
//    override def isUnstable: Boolean = false
//  }
//
//  protected final case class UnstableEagerValue[E, D <: Domain[E], V](
//    final override val value: V
//  ) extends EagerValue[E, D, V] {
//
//    override def isStable: Boolean = false;
//
//    override def isUnstable: Boolean = true
//  }
//
//  protected final case class LazySegmentSeq[E, D <: Domain[E], V](
//    private val seq: => SegmentSeq[E, D, V]
//  ) extends MasterValue[E, D, V] {
//
//    override def isStable: Boolean = false;
//
//    override def isUnstable: Boolean = true
//
//    def eval(): SegmentSeq[E, D, V] = seq
//  }
//}