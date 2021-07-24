package ordset.core.value

@FunctionalInterface
trait InclusionPredicate[-V] extends Function1[V, Boolean]

object InclusionPredicate {

  lazy val alwaysIncluded: InclusionPredicate[Any] = _ => true

  lazy val neverIncluded: InclusionPredicate[Any] = _ => false

  implicit lazy val booleanInclusion: InclusionPredicate[Boolean] = identity[Boolean]
  
  implicit def optionInclusion[V]: InclusionPredicate[Option[V]] = optionInclusionInstance

  final class MapImpl[-V1, -V2](
    inclPredicate: InclusionPredicate[V1],
    mapFunc: V2 => V1
  ) extends InclusionPredicate[V2] {
    
    override def apply(v: V2): Boolean = inclPredicate.apply(mapFunc.apply(v))
  }
  
  // Private section ---------------------------------------------------------- //
  private lazy val optionInclusionInstance = new InclusionPredicate[Option[Any]] {
    override def apply(v1: Option[Any]): Boolean = v1.isDefined
  }
}
