package ordset

import ordset.util.SingleValue

trait OrderWithDir[E, Dir <: OrderDir] extends Order[E] {

  val direction: Dir

  def sign: Int = if (isAscending) 1 else -1

  def invertedSign: Int = if (isAscending) -1 else 1

  def isAscending: Boolean = OrderDirection.isAscending(direction)

  def isDescending: Boolean = OrderDirection.isDescending(direction)
}

object OrderWithDir { outer =>

  abstract class Abstract[E, Dir <: OrderDir](
      implicit protected val dirValue: SingleValue[Dir]
  ) extends OrderWithDir[E, Dir] {

    final override val direction: Dir = dirValue.get

    final override val sign: Int = super.sign

    final override val invertedSign: Int = super.invertedSign

    final override val isAscending: Boolean = super.isAscending

    final override val isDescending: Boolean = super.isDescending
  }

  class Wrapper[E, Dir <: OrderDir](
      val elementOrd: Order[E],
  )(
      implicit protected override val dirValue: SingleValue[Dir]
  ) extends Abstract[E, Dir] {

    override def compare(x: E, y: E): Int = elementOrd.compare(x, y)
  }

  implicit val intAscOrderWithDir: AscOrder[Int] = new outer.Wrapper(Instances.intAscOrder)
  implicit val intDescOrderWithDir: DescOrder[Int] = new outer.Wrapper(Instances.intDescOrder)

  private object Instances {
    import cats.kernel.instances
    import cats.kernel.Order.reverse

    val intAscOrder: Order[Int] = instances.int.catsKernelStdOrderForInt
    val intDescOrder: Order[Int] = reverse(intAscOrder)
  }
}