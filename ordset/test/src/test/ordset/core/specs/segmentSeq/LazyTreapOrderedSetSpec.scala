package test.ordset.core.specs.segmentSeq

import ordset.core.AbstractLazyTreapSegmentSeq.{ControlTupleOps, ControlValueOps, ControlValue, LazyValue, ZSegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{TreapOrderedMap, ZippedOrderedMap}
import ordset.core.set.{ArrayOrderedSet, OrderedSet, TreapOrderedSet}
import ordset.core.value.ValueOps
import ordset.core.{AbstractLazyTreapSegmentSeq, Bound, Segment, SegmentSeq, SegmentSeqOps, TreapSegmentSeq}
import ordset.random.RngManager
import ordset.util.IterableUtil
import ordset.{Order, core}
import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class LazyTreapOrderedSetSpec extends AnyFunSpec {

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._
  import ordset.core.syntax.BoundSyntax._
  import ordset.core.syntax.SetBuilderNotation._
  import ordset.instances.list._
  import ordset.instances.tuple2._
  import test.ordset.core.SegmentSeqAssert._
  import test.ordset.core.TestRngUtil.Implicits._

  type Dom = Domain[Int]

  private val valueOps: ValueOps[Boolean] = implicitly[ValueOps[Boolean]]
  private val domainOps: DomainOps[Int, Dom] = implicitly[DomainOps[Int, Dom]]
  private val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom](domainOps)

  it("test 1") {

    val seq1 = TreapOrderedSet.unsafeBuildAsc(
      ArraySeq(-10 `)[`, -5 `](`),
      complementary = true,
      domainOps
    )()

    val seq2 = TreapOrderedSet.unsafeBuildAsc(
      ArraySeq(2 `)[`, 8 `](`),
      complementary = false,
      domainOps
    )()

    val seq3 = TreapOrderedSet.unsafeBuildAsc(
      ArraySeq(15 `)[`, 20 `](`),
      complementary = false,
      domainOps
    )()

    val lazySeq = new LazyTreapOrderedSet(
      List(
        (0 `)`, () => seq1),
        (10 `]`, () => seq2),
        (null, () => seq3)
      )
    )

    val segment = lazySeq.getSegment(5 `]`)
    println(segment)
  }

  private class LazyTreapOrderedSet[E, D <: Domain[E], V](
    initControlSeq: Iterable[(Bound.Upper[E], () => SegmentSeq[E, D, V])]
  )(
    implicit
    final override val domainOps: DomainOps[E, D],
    final override val valueOps: ValueOps[V],
    final override val rngManager: RngManager
  ) extends AbstractLazyTreapSegmentSeq[E, D, V] {

    baseSeq = TreapOrderedMap.unsafeBuildAsc(
      List((null, valueOps.unit)),
      domainOps,
      valueOps
    )()

    controlSeq = TreapOrderedMap.unsafeBuildAsc(
      initControlSeq.map(p => (p._1, LazyValue(p._2))),
      domainOps,
      ControlValueOps.get
    )()

    zippedSeq = ZippedOrderedMap.apply(
      baseSeq, controlSeq, Tuple2.apply, _ => false, _ => false
    )(
      domainOps, ControlTupleOps.get(valueOps), rngManager
    )
  }
}
