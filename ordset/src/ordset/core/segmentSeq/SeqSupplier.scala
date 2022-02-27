package ordset.core.segmentSeq

import ordset.{Hash, Show}
import ordset.core.domain.Domain
import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.core.domain.DomainOps
import ordset.core.segmentSeq.map.UniformOrderedMap
import ordset.core.segmentSeq.map.TreapOrderedMap
import ordset.core.segmentSeq.map.OrderedMapFactory
import ordset.util.HashUtil.product1Hash
import ordset.random.RngManager

/**
 * Contains classes and operations related to [[SeqSupplier]] type.
 * 
 * [[SeqSupplier]] is a optional function without arguments that returns segment sequence.
 */ 
object SeqSupplier {

  /**
   * Utility methods for segment sequence of [[SeqSupplier]].
   */ 
  object SegmentSeq {

    /**
     * Returns factory for segment sequence of [[SeqSupplier]].
     */ 
    def getFactory[E, D[X] <: Domain[X], V]: OrderedMapFactory[E, D, SeqSupplier[E, D, V], SupplierSegmentSeq[E, D, V]] =
      TreapOrderedMap.getFactory

    /**
     * Returns factory for segment sequence of [[SeqSupplier]] with partially provided parameters.
     * 
     * Output factory has disabled validation of bounds and values, so the client must guarantee that preconditions
     * of [[ordset.core.segmentSeq.map.OrderedMapFactory]] are satisfied.
     */ 
    def provideUncheckedFactory[E, D[X] <: Domain[X], V](
      implicit
      domainOps: DomainOps[E, D],
      rngManager: RngManager
    ): OrderedMapFactory[E, D, SeqSupplier[E, D, V], SupplierSegmentSeq[E, D, V]]#Partial =
      getFactory.provided(domainOps, ValueOpsImpl.get, rngManager)
  }

  /**
   * [[Hash]] typeclass implementation for [[SeqSupplier]].
   */ 
  final class HashImpl[E, D[X] <: Domain[X], V] extends Hash[SeqSupplier[E, D, V]] {

    override def hash(x: SeqSupplier[E, D, V]): Int = x match {
      case None => None.##
      case Some(f) => product1Hash(System.identityHashCode(f))
    }

    override def eqv(x: SeqSupplier[E, D, V], y: SeqSupplier[E, D, V]): Boolean = (x, y) match {
      case (None, None) => true
      case (Some(fx), Some(fy)) => fx.eq(fy)
      case _ => false
    }
  }

  object HashImpl {

    def get[E, D[X] <: Domain[X], V]: Hash[SeqSupplier[E, D, V]] = instance.asInstanceOf[Hash[SeqSupplier[E, D, V]]]

    // Private section ---------------------------------------------------------- //
    private lazy val instance: HashImpl[Any, Domain, Any] = new HashImpl()
  }

  object ShowImpl {

    def get[E, D[X] <: Domain[X], V]: Show[SeqSupplier[E, D, V]] = instance.asInstanceOf[Show[SeqSupplier[E, D, V]]]

    // Private section ---------------------------------------------------------- //
    private lazy val instance: Show[SeqSupplier[Any, Domain, Any]] = Show.fromToString
  }

  /**
   * [[ ordset.core.value.ValueOps]] typeclass implementation for [[SeqSupplier]].
   */ 
  object ValueOpsImpl {

    def get[E, D[X] <: Domain[X], V]: ValueOps[SeqSupplier[E, D, V]] = 
      instance.asInstanceOf[ValueOps[SeqSupplier[E, D, V]]]

    // Private section ---------------------------------------------------------- //
    private lazy val instance: ValueOps[SeqSupplier[Any, Domain, Any]] =
      new ValueOps.DefaultImpl(
        None,
        HashImpl.get,
        InclusionPredicate.optionInclusion,
        ShowImpl.get
      )
  }
}
