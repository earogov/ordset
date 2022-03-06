package ordset.test.core.behaviors.segmentSeq

import ordset.ContravariantShow
import ordset.core.segmentSeq.validation.ValidatingIterable
import ordset.test.LazyClue
import org.scalatest.Assertions._
import ordset.core.segmentSeq.validation.ValidationException

case class ValidationTest[E](
  val validationCases: Iterable[ValidationTest.TestCase[E]]
) {

  def run: Unit = validationCases.foreach { _.run }
}

object ValidationTest {

  sealed trait TestCase[E] {

    implicit def show: ContravariantShow[Iterable[E]]

    def iterable: ValidatingIterable[E]

    def run: Unit

    protected val debugInfo: LazyClue = () => this.toString()
  }

  case class SuccessCase[E](
    override val iterable: ValidatingIterable[E]
  )(
    override implicit val show: ContravariantShow[Iterable[E]]
  ) extends TestCase[E] {

    override def run: Unit = {
      assert(iterable.allValid, debugInfo)
      assertResult({}, debugInfo)(iterable.validateAll())
      assertResult({}, debugInfo)(iterable.foldLeftValidated({})((_, _) => {}))
    }

    override def toString(): String = s"ValidationTest.SuccessCase(iterable = ${show.show(iterable)}"
  }

  case class FailureCase[E](
    override val iterable: ValidatingIterable[E], 
    val error: String
  )(
    override implicit val show: ContravariantShow[Iterable[E]]
  ) extends TestCase[E] {

    override def run: Unit = {
      assert(!iterable.allValid, debugInfo)
      assertValidationFail(iterable.validateAll())
      assertValidationFail(iterable.foldLeftValidated({})((_, _) => {}))
    }

    override def toString(): String = s"ValidationTest.FailureCase(iterable = ${show.show(iterable)}"

    protected def assertValidationFail(f: => Unit): Unit =
      try {
        f
      } catch {
        case e: ValidationException => 
          assertResult(error, debugInfo)(e.getMessage)
        case _ @ e => 
          fail(s"Expected ${classOf[ValidationException].getName}, but got ${e.getClass.getName}. $debugInfo")
      }
  }
}
