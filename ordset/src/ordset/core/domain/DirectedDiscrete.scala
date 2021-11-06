package ordset.core.domain

import ordset.{Order, Hash, Show}
import ordset.core.domain.OrderDir
import ordset.core.domain.entity.DirectedLabeledEntity
import ordset.core.domain.OrderDirection.Reversed
import ordset.util.label.Label

/**
 * Discrete operations with assigned direction - ascending or descending.
 * 
 * The main point of direction parameter is <u>to provide consistency</u> between different instances. 
 * For example, we can require both ascending instances of order and discrete operations to construct domain.
 * 
 * For some domains direction choice is trivial. For example, integers 1, 2, 3, ... follow in ascending order.
 * In other cases direction may be chosen arbitrary.
 * 
 * @tparam E element type parameter
 * @tparam Dir direction type parameter
 */
trait DirectedDiscrete[@specialized(Specializable.Integral) E, +Dir <: OrderDir] extends DirectedLabeledEntity[Dir] {

  /**
   * Return the successor of element.
   */
  def successor(x: E): E

  /**
   * Returns the predecessor of element.
   */
  def predecessor(x: E): E
}

object DirectedDiscrete {

  implicit def defaultHash[E]: Hash[DirectedDiscrete[E, _ <: OrderDir]] = DirectedLabeledEntity.defaultHash

  implicit lazy val defaultShow: Show[DirectedDiscrete[_, _ <: OrderDir]] =
    new DirectedLabeledEntity.DefaultShow(
      "DirectedDiscrete",
      Label.defaultSetShow,
      OrderDirection.defaultShow
    )

  /**
   * Returns reversed discrete operations such that
   * {{{
   *   if (discrete.isAscending == true) then reverse(discrete).isAscending == false
   *   if (discrete.isAscending == false) then reverse(discrete).isAscending == true
   * }}}
   * Output instance has flipped [[successor]] and [[predecessor]] operations.
   */
  def reverse[E, Dir <: OrderDir](discrete: DirectedDiscrete[E, Dir]): DirectedDiscrete[E, Reversed[Dir]] = {
    val revDir = ValueOf(OrderDirection.reverse(discrete.direction))
    discrete match {
      case discrete: DefaultImpl[E, _] =>
        new DefaultImpl(discrete.labels, discrete.predecessorFunc, discrete.successorFunc)(revDir)
      case _ =>
        new DefaultImpl(discrete.labels, discrete.predecessor, discrete.successor)(revDir)
    }
  }

  /**
   * Base class for discrete operations.
   */
  abstract class Abstract[E, +Dir <: OrderDir](
    implicit dirValue: ValueOf[Dir]
  ) extends DirectedDiscrete[E, Dir] {

    final override val direction: Dir = dirValue.value
  }

  /**
   * Default implementation of discrete operations.
   */
  final class DefaultImpl[E, +Dir <: OrderDir](
    override val labels: Set[Label],
    val successorFunc: E => E,
    val predecessorFunc: E => E
  )(
    implicit dirValue: ValueOf[Dir]
  ) extends Abstract[E, Dir] {

    override def successor(x: E): E = successorFunc(x)

    override def predecessor(x: E): E = predecessorFunc(x)
  }
}