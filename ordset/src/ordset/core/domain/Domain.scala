package ordset.core.domain

import ordset.core.{Bound, ExtendedBound}
import ordset.util.label.Label
import ordset.{Hash, Show}
import scala.annotation.meta.field

sealed trait Domain[E] extends DomainLike[E] {

  override def labels: Set[Label] = Set.empty

  override implicit val intOrd: AscOrder[Int] = ordset.core.instances.int.intAscOrder

  override implicit val longOrd: AscOrder[Long] = ordset.core.instances.long.longAscOrder
}

object Domain {

  sealed trait Unbounded[E] extends Domain[E] {

    override def labels: Set[Label] = super.labels + DomainLabels.Unbounded

    final override val lowerExtendedBound: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll

    final override val upperExtendedBound: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll
  }

  sealed trait Bounded[E] extends Domain[E] {

    override def labels: Set[Label] = super.labels + DomainLabels.Bounded
  }

  sealed trait Continuous[E] extends Domain[E] {

    override def labels: Set[Label] = super.labels + DomainLabels.Continuous

    override implicit val boundOrd: AscOrder[Bound[E]] = Bound.defaultAscOrder(elementOrd, intOrd)

    override implicit val extendedOrd: AscOrder[ExtendedBound[E]] = ExtendedBound.defaultAscOrder(boundOrd, intOrd)

    override def toString: String = Continuous.getDefaultShowInstance.show(this)
  }

  object Continuous {

    implicit def defaultShow[E, D <: Continuous[E]](
      implicit 
      boundsShow: Show[ExtendedBound[E]],
      labelsShow: Show[Set[Label]],
      orderShow: Show[DirectedOrder[_, _ <: OrderDir]]
    ): Show[D] =
      new DefaultShow(boundsShow, labelsShow, orderShow)

    final case class DefaultShow[E, D <: Continuous[E]](
      val boundsShow: Show[ExtendedBound[E]],
      val labelsShow: Show[Set[Label]],
      val orderShow: Show[DirectedOrder[_, _ <: OrderDir]]
    ) extends Show[D] {

      import ordset.util.ShowUtil.{standardShow, fieldShow}

      private val stdShow: Show[D] = standardShow("Domain"){ d =>
        val boundsFieldShow = fieldShow(boundsShow)
        val labelsFieldShow = fieldShow(labelsShow)
        val orderFieldShow = fieldShow(orderShow)
        List(
          ("labels", labelsFieldShow.show(d.labels)), 
          ("extendedLowerBound", boundsFieldShow.show(d.lowerExtendedBound)),
          ("extendedUpperBound", boundsFieldShow.show(d.upperExtendedBound)),
          ("elementOrd", orderFieldShow.show(d.elementOrd)),
          ("intOrd", orderFieldShow.show(d.intOrd)),
          ("longOrd", orderFieldShow.show(d.longOrd)),
          ("boundOrd", orderFieldShow.show(d.boundOrd)),
          ("extendedOrd", orderFieldShow.show(d.extendedOrd))
        )
      }

      override def show(t: D): String = stdShow.show(t)
    }

    // Private section ---------------------------------------------------------- //
    private def getDefaultShowInstance[E]: Show[Continuous[E]] = defaultShowInstance.asInstanceOf

    private lazy val defaultShowInstance: Show[Continuous[Any]] =
      defaultShow(
        ExtendedBound.defaultShow(Show.fromToString), 
        Label.defaultSetShow, 
        DirectedOrder.defaultShow
      )
  }

  sealed trait Discrete[E] extends Domain[E] {

    override def labels: Set[Label] = super.labels + DomainLabels.Discrete

    implicit def discrete: AscDiscrete[E]

    // TODO: implement discrete order and replace it here.
    override implicit val boundOrd: AscOrder[Bound[E]] = Bound.defaultAscOrder(elementOrd, intOrd)

    override implicit val extendedOrd: AscOrder[ExtendedBound[E]] = ExtendedBound.defaultAscOrder(boundOrd, intOrd)

    override def toString: String = Discrete.getDefaultShowInstance.show(this)
  }

  object Discrete {

    implicit def defaultShow[E, D <: Discrete[E]](
      implicit 
      boundsShow: Show[ExtendedBound[E]],
      labelsShow: Show[Set[Label]],
      orderShow: Show[DirectedOrder[_, _ <: OrderDir]],
      discreteShow: Show[DirectedDiscrete[_, _ <: OrderDir]],
    ): Show[D] =
      new DefaultShow(boundsShow, labelsShow, orderShow, discreteShow)

    final case class DefaultShow[E, D <: Discrete[E]](
      val boundsShow: Show[ExtendedBound[E]],
      val labelsShow: Show[Set[Label]],
      val orderShow: Show[DirectedOrder[_, _ <: OrderDir]],
      val discreteShow: Show[DirectedDiscrete[_, _ <: OrderDir]],
    ) extends Show[D] {

      import ordset.util.ShowUtil.{standardShow, fieldShow}

      private val stdShow: Show[D] = standardShow("Domain"){ d =>
        val boundsFieldShow = fieldShow(boundsShow)
        val labelsFieldShow = fieldShow(labelsShow)
        val orderFieldShow = fieldShow(orderShow)
        val discreteFieldShow = fieldShow(discreteShow)
        List(
          ("labels", labelsFieldShow.show(d.labels)), 
          ("extendedLowerBound", boundsFieldShow.show(d.lowerExtendedBound)),
          ("extendedUpperBound", boundsFieldShow.show(d.upperExtendedBound)),
          ("elementOrd", orderFieldShow.show(d.elementOrd)),
          ("intOrd", orderFieldShow.show(d.intOrd)),
          ("longOrd", orderFieldShow.show(d.longOrd)),
          ("boundOrd", orderFieldShow.show(d.boundOrd)),
          ("extendedOrd", orderFieldShow.show(d.extendedOrd)),
          ("discrete", discreteFieldShow.show(d.discrete))
        )
      }

      override def show(t: D): String = stdShow.show(t)
    }

    // Private section ---------------------------------------------------------- //
    private def getDefaultShowInstance[E]: Show[Discrete[E]] = defaultShowInstance.asInstanceOf

    private lazy val defaultShowInstance: Show[Discrete[Any]] =
      defaultShow(
        ExtendedBound.defaultShow(Show.fromToString), 
        Label.defaultSetShow, 
        DirectedOrder.defaultShow, 
        DirectedDiscrete.defaultShow
      )
  }

  trait UnboundedContinuous[E] extends Unbounded[E] with Continuous[E]

  object UnboundedContinuous {

    def apply[E](implicit elementOrd: AscOrder[E]): UnboundedContinuous[E] = new DefaultImpl(elementOrd)

    implicit def defaultDomain[E](implicit elementOrd: AscOrder[E]): UnboundedContinuous[E] = apply

    implicit def defaultHash[E, D <: UnboundedContinuous[E]](
      implicit 
      orderHash: Hash[DirectedOrder[E, _ <: OrderDir]],
      labelsHash: Hash[Set[Label]]
    ): Hash[D] = 
      new DefaultHash(orderHash, labelsHash)

    final case class DefaultImpl[E](
      override val elementOrd: AscOrder[E],
      val customLabels: Set[Label] = Set()
    ) extends UnboundedContinuous[E] {

      override val labels: Set[Label] = super.labels ++ customLabels
    }

    final case class DefaultHash[E, D <: UnboundedContinuous[E]](
      val orderHash: Hash[DirectedOrder[E, _ <: OrderDir]],
      val labelsHash: Hash[Set[Label]]
    ) extends Hash[D] {

      import ordset.util.HashUtil._

      override def eqv(x: D, y: D): Boolean =
        (x eq y) ||
        (
          orderHash.eqv(x.elementOrd, y.elementOrd) &&
          labelsHash.eqv(x.labels, y.labels)
        )

      override def hash(x: D): Int =
        product2Hash(
          orderHash.hash(x.elementOrd),
          labelsHash.hash(x.labels)
        )
    }
  }

  trait UnboundedDiscrete[E] extends Unbounded[E] with Discrete[E]

  object UnboundedDiscrete {

    def apply[E](
      implicit
      elementOrd: AscOrder[E],
      discrete: AscDiscrete[E],
    ): UnboundedDiscrete[E] =
      new DefaultImpl(elementOrd, discrete)

    implicit def defaultHash[E, D <: UnboundedDiscrete[E]](
      implicit 
      orderHash: Hash[DirectedOrder[E, _ <: OrderDir]],
      labelsHash: Hash[Set[Label]],
      discreteHash: Hash[DirectedDiscrete[E, _ <: OrderDir]]
    ): Hash[D] = 
      new DefaultHash(orderHash, labelsHash, discreteHash)

    final case class DefaultImpl[E](
      override val elementOrd: AscOrder[E],
      override val discrete: AscDiscrete[E],
      val customLabels: Set[Label] = Set()
    ) extends UnboundedDiscrete[E] {

      override val labels: Set[Label] = super.labels ++ customLabels
    }

    final case class DefaultHash[E, D <: UnboundedDiscrete[E]](
      val orderHash: Hash[DirectedOrder[E, _ <: OrderDir]],
      val labelsHash: Hash[Set[Label]],
      val discreteHash: Hash[DirectedDiscrete[E, _ <: OrderDir]]
    ) extends Hash[D] {

      import ordset.util.HashUtil._

      override def eqv(x: D, y: D): Boolean =
        (x eq y) ||
        (
          orderHash.eqv(x.elementOrd, y.elementOrd) &&
          discreteHash.eqv(x.discrete, y.discrete) &&
          labelsHash.eqv(x.labels, y.labels)
        )

      override def hash(x: D): Int =
        product3Hash(
          orderHash.hash(x.elementOrd),
          discreteHash.hash(x.discrete),
          labelsHash.hash(x.labels),
        )
    }
  }

  trait BoundedContinuous[E] extends Bounded[E] with Continuous[E]

  object BoundedContinuous {

    def apply[E](
      lowerExtendedBound: ExtendedBound.Lower[E],
      upperExtendedBound: ExtendedBound.Upper[E],
    )(
      implicit
      elementOrd: AscOrder[E],
      discrete: AscDiscrete[E],
    ): BoundedContinuous[E] =
      new DefaultImpl(elementOrd, lowerExtendedBound, upperExtendedBound)

    implicit def defaultHash[E, D <: BoundedContinuous[E]](
      implicit 
      orderHash: Hash[DirectedOrder[E, _ <: OrderDir]],
      labelsHash: Hash[Set[Label]]
    ): Hash[D] = 
      new DefaultHash(orderHash, labelsHash)

    final case class DefaultImpl[E](
      override val elementOrd: AscOrder[E],
      override val lowerExtendedBound: ExtendedBound.Lower[E],
      override val upperExtendedBound: ExtendedBound.Upper[E],
      val customLabels: Set[Label] = Set()
    ) extends BoundedContinuous[E] {

      override val labels: Set[Label] = super.labels ++ customLabels
    }

    final case class DefaultHash[E, D <: BoundedContinuous[E]](
      val orderHash: Hash[DirectedOrder[E, _ <: OrderDir]],
      val labelsHash: Hash[Set[Label]]
    ) extends Hash[D] {

      import ordset.util.HashUtil._

      override def eqv(x: D, y: D): Boolean =
        (x eq y) ||
        (
          orderHash.eqv(x.elementOrd, y.elementOrd) &&
          x.extendedOrd.eqv(x.lowerExtendedBound, y.lowerExtendedBound) &&
          x.extendedOrd.eqv(x.upperExtendedBound, y.upperExtendedBound) &&
          labelsHash.eqv(x.labels, y.labels)
        )

      override def hash(x: D): Int =
        product4Hash(
          orderHash.hash(x.elementOrd),
          x.extendedOrd.hash(x.lowerExtendedBound),
          x.extendedOrd.hash(x.upperExtendedBound),
          labelsHash.hash(x.labels),
        )
    }
  }

  trait BoundedDiscrete[E] extends Bounded[E] with Discrete[E]

  object BoundedDiscrete {

    def apply[E](
      lowerExtendedBound: ExtendedBound.Lower[E],
      upperExtendedBound: ExtendedBound.Upper[E],
    )(
      implicit
      elementOrd: AscOrder[E],
      discrete: AscDiscrete[E],
    ): BoundedDiscrete[E] =
      new DefaultImpl(elementOrd, discrete, lowerExtendedBound, upperExtendedBound)

    implicit def defaultHash[E, D <: BoundedDiscrete[E]](
      implicit 
      orderHash: Hash[DirectedOrder[E, _ <: OrderDir]],
      labelsHash: Hash[Set[Label]],
      discreteHash: Hash[DirectedDiscrete[E, _ <: OrderDir]]
    ): Hash[D] = 
      new DefaultHash(orderHash, labelsHash, discreteHash)

    final case class DefaultImpl[E](
      override val elementOrd: AscOrder[E],
      override val discrete: AscDiscrete[E],
      override val lowerExtendedBound: ExtendedBound.Lower[E],
      override val upperExtendedBound: ExtendedBound.Upper[E],
      val customLabels: Set[Label] = Set()
    ) extends BoundedDiscrete[E] {

      override val labels: Set[Label] = super.labels ++ customLabels
    }

    final case class DefaultHash[E, D <: BoundedDiscrete[E]](
      val orderHash: Hash[DirectedOrder[E, _ <: OrderDir]],
      val labelsHash: Hash[Set[Label]],
      val discreteHash: Hash[DirectedDiscrete[E, _ <: OrderDir]]
    ) extends Hash[D] {

      import ordset.util.HashUtil._

      override def eqv(x: D, y: D): Boolean =
        (x eq y) ||
        (
          orderHash.eqv(x.elementOrd, y.elementOrd) &&
          discreteHash.eqv(x.discrete, y.discrete) &&
          x.extendedOrd.eqv(x.lowerExtendedBound, y.lowerExtendedBound) &&
          x.extendedOrd.eqv(x.upperExtendedBound, y.upperExtendedBound) &&
          labelsHash.eqv(x.labels, y.labels)
        )

      override def hash(x: D): Int =
        product5Hash(
          orderHash.hash(x.elementOrd),
          discreteHash.hash(x.discrete),
          x.extendedOrd.hash(x.lowerExtendedBound),
          x.extendedOrd.hash(x.upperExtendedBound),
          labelsHash.hash(x.labels),
        )
    }
  }
}