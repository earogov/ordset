package ordset.core.value

@FunctionalInterface
trait InclusionPredicate[-V] extends Function1[V, Boolean]

object InclusionPredicate {

  lazy val alwaysIncluded: InclusionPredicate[Any] = _ => true

  lazy val neverIncluded: InclusionPredicate[Any] = _ => false

  implicit lazy val booleanInclusion: InclusionPredicate[Boolean] = identity[Boolean]
  
  implicit def optionInclusion[V]: InclusionPredicate[Option[V]] = optionInclusionInstance

  // Private section ---------------------------------------------------------- //
  private lazy val optionInclusionInstance = new InclusionPredicate[Option[Any]] {
    override def apply(v1: Option[Any]): Boolean = v1.isDefined
  }
}
