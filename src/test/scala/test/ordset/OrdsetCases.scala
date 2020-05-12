package test.ordset

trait OrdsetCases[T, V] {

  import ordset._

  type IFlow = IntervalFlow[T, V]

  val emptyCase: Option[IFlow]
  val universalCase: Option[IFlow]
  val singleBoundedCase: Option[IFlow]
  val multiBoundedCase: Option[IFlow]
  val degenerateCase: Option[IFlow]
}

