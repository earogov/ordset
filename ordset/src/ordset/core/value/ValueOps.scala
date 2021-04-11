package ordset.core.value

import ordset.Eq

trait ValueOps[V] {

  def valueEq: Eq[V]
  
  def valueIncl: InclusionPredicate[V]
  
  final def eqv(x: V, y: V): Boolean = valueEq.eqv(x, y)
  
  final def neqv(x: V, y: V): Boolean = valueEq.neqv(x, y)
  
  final def isIncluded(x: V): Boolean = valueIncl.apply(x)
}

object ValueOps {
  
  implicit lazy val booleanValueOps: ValueOps[Boolean] = 
    new DefaultImpl[Boolean](
      ordset.instances.boolean.booleanHash,
      InclusionPredicate.booleanInclusion
    )
  
  final class DefaultImpl[V](
    override val valueEq: Eq[V],
    override val valueIncl: InclusionPredicate[V]
  ) extends ValueOps[V]
}
