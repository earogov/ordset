package ordset.core

import ordset.Order
import ordset.core.domain.{DirectedOrder, DirectedDiscrete}
import ordset.core.domain.{DirectedOrder, AscOrder, DescOrder, AscDiscrete, DescDiscrete}
import ordset.util.label.Label

package object instances {

  object OrderLabels {

    val booleanDefault: Label = Label("BooleanDefault")

    val intDefault: Label = Label("IntDefault")

    val longDefault: Label = Label("LongDefault")

    val stringDefault: Label = Label("StringDefault")
  }

  object DiscreteLabels {

    val integralDefault: Label = Label("IntegralDefault")
  }

  object boolean {

    import ordset.instances.boolean._

    implicit lazy val booleanAscOrder: AscOrder[Boolean] =
      new DirectedOrder.DefaultImpl(Set(OrderLabels.booleanDefault), booleanOrder, booleanHash)

    lazy val booleanDescOrder: DescOrder[Boolean] = DirectedOrder.reverse(booleanAscOrder)
  }

  object int {

    import ordset.instances.int._

    implicit lazy val intAscOrder: AscOrder[Int] =
      new DirectedOrder.DefaultImpl(Set(OrderLabels.intDefault), intOrder, intHash)

    lazy val intDescOrder: DescOrder[Int] = DirectedOrder.reverse(intAscOrder)
  }

  object long {

    import ordset.instances.long._

    implicit lazy val longAscOrder: AscOrder[Long] =
      new DirectedOrder.DefaultImpl(Set(OrderLabels.longDefault), longOrder, longHash)

    lazy val longDescOrder: DescOrder[Long] = DirectedOrder.reverse(longAscOrder)
  }

  object integral {

    implicit def integralAscDiscrete[E](implicit i: Integral[E]): AscDiscrete[E] =
      new DirectedDiscrete.DefaultImpl(
        Set(DiscreteLabels.integralDefault),
        e => i.plus(e, i.one),
        e => i.minus(e, i.one)
      )
  }

  object string {

    import ordset.instances.string._

    implicit lazy val stringAscOrder: AscOrder[String] =
      new DirectedOrder.DefaultImpl(Set(OrderLabels.stringDefault), stringOrder, stringHash)

    lazy val stringDescOrder: Order[String] = DirectedOrder.reverse(stringAscOrder)
  }
}
