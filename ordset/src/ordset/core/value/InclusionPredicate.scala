package ordset.core.value

@FunctionalInterface
trait InclusionPredicate[-V] extends Function1[V, Boolean]

object InclusionPredicate {
  
  implicit lazy val booleanInclusion: InclusionPredicate[Boolean] = identity[Boolean]
  
  implicit def optionInclusion[V]: InclusionPredicate[Option[V]] = optionInclusionInstance
  
  private lazy val optionInclusionInstance = new InclusionPredicate[Option[Any]] {
    override def apply(v1: Option[Any]): Boolean = v1.isDefined
  }
}
