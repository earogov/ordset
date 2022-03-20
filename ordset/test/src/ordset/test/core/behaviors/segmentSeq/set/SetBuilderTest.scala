package ordset.test.core.behaviors.segmentSeq.set

import ordset.ContravariantShow
import ordset.random.RngManager
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.interval.Interval
import ordset.core.segmentSeq.SegmentSeqException
import ordset.core.segmentSeq.set.{OrderedSet, TreapOrderedSet}
import ordset.test.core.SegmentSeqAssertions._
import org.scalatest.Assertions._

case class SetBuilderTest[E, D[X] <: Domain[X]](
  val validationCases: Iterable[SetBuilderTest.TestCase[E, D]]
) {

  def run: Unit = validationCases.foreach { _.run }
}

object SetBuilderTest {

  sealed trait TestCase[E, D[X] <: Domain[X]] {

    implicit def show: ContravariantShow[Iterable[Interval[E, D]]]

    def intervals: Iterable[Interval[E, D]]

    def run: Unit

    protected lazy val debugInfo: String = this.toString()
  }

  case class SuccessCase[E, D[X] <: Domain[X]](
    override val intervals: Iterable[Interval[E, D]],
    val expected: OrderedSet[E, D]
  )(
    override implicit val show: ContravariantShow[Iterable[Interval[E, D]]]
  ) extends TestCase[E, D] {

    private val builder = TreapOrderedSet.getBuilder.provided(expected.domainOps, expected.rngManager)

    override def run: Unit = {
      val actual = builder.unsafeBuild(intervals)
      assertSameSegmentSeq(expected, actual, debugInfo)(expected.domainOps, expected.valueOps)
    }

    override def toString(): String = s"BuilderTest.SuccessCase(intervals = ${show.show(intervals)}"
  }

  case class FailureCase[E, D[X] <: Domain[X]](
    override val intervals: Iterable[Interval[E, D]],
    val error: String
  )(
    override implicit val show: ContravariantShow[Iterable[Interval[E, D]]],
    implicit val domainOps: DomainOps[E, D],
    implicit val rngManager: RngManager
  ) extends TestCase[E, D] {

    private val builder = TreapOrderedSet.getBuilder.provided(domainOps, rngManager)

    override def run: Unit =
      try {
        builder.unsafeBuild(intervals)
        fail(s"Ordered set has been successfully built, but expected fail. $debugInfo")
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
