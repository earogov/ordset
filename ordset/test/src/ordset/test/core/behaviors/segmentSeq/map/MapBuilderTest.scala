package ordset.test.core.behaviors.segmentSeq.map

import ordset.core.interval.IntervalRelation

import ordset.ContravariantShow
import ordset.random.RngManager
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.interval.IntervalRelation
import ordset.core.segmentSeq.SegmentSeqException
import ordset.core.segmentSeq.map.{OrderedMap, TreapOrderedMap}
import ordset.test.core.SegmentSeqAssertions._
import org.scalatest.Assertions._

case class MapBuilderTest[E, D[X] <: Domain[X], V](
  val validationCases: Iterable[MapBuilderTest.TestCase[E, D, V]]
) {

  def run: Unit = validationCases.foreach { _.run }
}

object MapBuilderTest {

  sealed trait TestCase[E, D[X] <: Domain[X], V] {

    implicit def show: ContravariantShow[Iterable[IntervalRelation[E, D, V]]]

    def defaultValue: V

    def intervals: Iterable[IntervalRelation[E, D, V]]

    def run: Unit

    protected lazy val debugInfo: String = this.toString()
  }

  case class SuccessCase[E, D[X] <: Domain[X], V](
    override val defaultValue: V,
    override val intervals: Iterable[IntervalRelation[E, D, V]],
    val expected: OrderedMap[E, D, V]
  )(
    override implicit val show: ContravariantShow[Iterable[IntervalRelation[E, D, V]]]
  ) extends TestCase[E, D, V] {

    private val builder = 
      TreapOrderedMap.getBuilder.provided(expected.domainOps, expected.valueOps, expected.rngManager)

    override def run: Unit = {
      val actual = builder.unsafeBuild(defaultValue, intervals)
      assertSameSegmentSeq(expected, actual, debugInfo)(expected.domainOps, expected.valueOps)
    }

    override def toString(): String = s"BuilderTest.SuccessCase(intervals = ${show.show(intervals)}"
  }

  case class FailureCase[E, D[X] <: Domain[X], V](
    override val defaultValue: V,
    override val intervals: Iterable[IntervalRelation[E, D, V]],
    val error: String
  )(
    override implicit val show: ContravariantShow[Iterable[IntervalRelation[E, D, V]]],
    implicit val domainOps: DomainOps[E, D],
    implicit val valueOps: ValueOps[V],
    implicit val rngManager: RngManager
  ) extends TestCase[E, D, V] {

    private val builder = 
      TreapOrderedMap.getBuilder.provided(domainOps, valueOps, rngManager)

    override def run: Unit =
      try {
        builder.unsafeBuild(defaultValue, intervals)
        fail(s"Ordered map has been successfully built, but expected fail. $debugInfo")
      } catch {
        case e: SegmentSeqException =>
          val cause = e.getCause()
          assert(cause != null, debugInfo)
          assertResult(error, debugInfo)(cause.nn.getMessage)
        case _ @ e => 
          fail(s"Expected ${classOf[SegmentSeqException].getName}, but got ${e.getClass.getName}. $debugInfo")
      }

    override def toString(): String = s"BuilderTest.FailureCase(intervals = ${show.show(intervals)}"
  }
}
