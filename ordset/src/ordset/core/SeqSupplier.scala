package ordset.core

import ordset.Hash
import ordset.core.domain.Domain
import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.util.HashUtil.product1Hash

object SeqSupplier {

  final class HashImpl[E, D <: Domain[E], V] extends Hash[SeqSupplier[E, D, V]] {

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

    def get[E, D <: Domain[E], V]: Hash[SeqSupplier[E, D, V]] = instance.asInstanceOf[Hash[SeqSupplier[E, D, V]]]

    // Private section ---------------------------------------------------------- //
    private lazy val instance: HashImpl[Any, Domain[Any], Any] = new HashImpl()
  }

  object ValueOpsImpl {

    def get[E, D <: Domain[E], V]: ValueOps[SeqSupplier[E, D, V]] = 
      instance.asInstanceOf[ValueOps[SeqSupplier[E, D, V]]]

    // Private section ---------------------------------------------------------- //
    private lazy val instance: ValueOps[SeqSupplier[Any, Domain[Any], Any]] =
      new ValueOps.DefaultImpl(
        None,
        HashImpl.get,
        InclusionPredicate.optionInclusion
      )
  }
}
