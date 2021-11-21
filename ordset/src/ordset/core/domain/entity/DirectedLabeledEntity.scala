package ordset.core.domain.entity

import ordset.{Hash, Show}
import ordset.core.domain.{OrderDir, OrderDirection}
import ordset.util.label.{Label, LabeledEntity}
import scala.annotation.static

/**
 * Base trait for abstract entities with direction and labels like order, etc.
 *
 * @tparam Dir direction type parameter
 */
trait DirectedLabeledEntity[+Dir <: OrderDir] extends DirectedEntity[Dir] with LabeledEntity

object DirectedLabeledEntity {

  def defaultHash[Ent <: DirectedLabeledEntity[_ <: OrderDir]]: Hash[Ent] = defaultHashInstance.asInstanceOf

  final case class DefaultHash[Ent <: LabeledEntity & DirectedEntity[_ <: OrderDir]](
    val labelsHash: Hash[Set[Label]],
    val dirHash: Hash[OrderDir]
  ) extends Hash[Ent] {

    import ordset.util.HashUtil._

    override def eqv(x: Ent, y: Ent): Boolean =
      (x eq y) || 
      (labelsHash.eqv(x.labels, y.labels) && dirHash.eqv(x.direction, y.direction))

    override def hash(x: Ent): Int =
      product2Hash(labelsHash.hash(x.labels), dirHash.hash(x.direction))
  }

  final case class DefaultShow[Ent <: DirectedLabeledEntity[_ <: OrderDir]](
    val name: String,
    val labelsShow: Show[Set[Label]],
    val dirShow: Show[OrderDir]
  ) extends Show[Ent] {

    import ordset.util.ShowUtil.{standardShow, fieldShow}

    private val stdShow: Show[Ent] = standardShow(name){ ent =>
      List(
        ("labels", fieldShow(labelsShow).show(ent.labels)), 
        ("direction", fieldShow(dirShow).show(ent.direction))
      )
    }

    override def show(t: Ent): String = stdShow.show(t)
  }

  // Private section ---------------------------------------------------------- //
  private val defaultHashInstance: Hash[DirectedLabeledEntity[_ <: OrderDir]] = 
    new DefaultHash(Label.defaultSetHash, OrderDirection.defaultHash)
}