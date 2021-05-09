package ordset.core

import ordset.core.domain.Domain

abstract class AbstractSegmentSeq[E, D <: Domain[E], V, +S] extends SegmentSeqT[E, D, V, S] {

  // Protected section -------------------------------------------------------- //
  
  /**
   * Casts type constructor `F[Boolean]` to `F[V]` if sequence represents a set and returns `null` otherwise.
   */
  protected final def castBoolean[F[_]](f: F[Boolean]): F[V] | Null =
    if (isSet) f.asInstanceOf[F[V]]
    else null
}
