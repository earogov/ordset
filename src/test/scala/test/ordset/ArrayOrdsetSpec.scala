package test.ordset

import org.scalatest.funspec.AnyFunSpec

class ArrayOrdsetSpec extends AnyFunSpec
  with OrdsetCases[Int, Boolean]
  with IntervalFlowBehaviors[Int, Boolean] {

  import ordset._
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._
  import scala.language.postfixOps
  import test.syntax.ArraySyntax._
  import scala.collection.immutable.ArraySeq
  import cats.kernel.instances.int._

  override val emptyCase: Option[IFlow] = Some(
    new ArrayOrdset[Int](ArraySeq.empty, complement = false).intervalFlow
  )

  override val universalCase: Option[IFlow] = Some(
    new ArrayOrdset[Int](ArraySeq.empty, complement = true).intervalFlow
  )

  override val singleBoundedCase: Option[IFlow] = Some(
    new ArrayOrdset[Int](Array(0`](`).toImmutableArraySeq, complement = true)
      .intervalFlow
  )

  override val multiBoundedCase: Option[IFlow] = Some(
    new ArrayOrdset[Int](Array(0`)[`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq, complement = false)
      .intervalFlow
  )

  override val degenerateCase: Option[IFlow] = Some(
    new ArrayOrdset[Int](Array(0`)[`, 0`](`, 10`)[`, 20`)[`, 20`](`, 30`)[`).toImmutableArraySeq, complement = false)
      .intervalFlow
  )

  describe("Interval flow of array based ordered set") {

    it should behave like coverDomainWithoutGapsAndOverlapping("empty set",
      emptyCase.get,
      (false forAll x) #:: LazyList.empty
    )

    it should behave like coverDomainWithoutGapsAndOverlapping("universal set",
      universalCase.get,
      (true forAll x) #:: LazyList.empty
    )

    it should behave like coverDomainWithoutGapsAndOverlapping("single bounded set",
      singleBoundedCase.get,
      (true forAll x <=  0) #:: (false forAll x > 0) #:: LazyList.empty
    )

    it should behave like coverDomainWithoutGapsAndOverlapping("multi bounded set",
      multiBoundedCase.get,
      (false forAll x <  0) #::
      (true  forAll x >= 0  & x < 10) #::
      (false forAll x >= 10 & x < 20) #::
      (true  forAll x >= 20 & x < 30) #::
      (false forAll x >= 30 & x < 40) #::
      (true  forAll x >= 40) #::
      LazyList.empty
    )

    it should behave like coverDomainWithoutGapsAndOverlapping("set with degenerate interval",
      degenerateCase.get,
      (false forAll x <  0) #::
      (true  forAll x >= 0  & x <= 0) #::
      (false forAll x >  0  & x <  10) #::
      (true  forAll x >= 10 & x <  20) #::
      (false forAll x >= 20 & x <= 20) #::
      (true  forAll x >  20 & x <  30) #::
      (false forAll x >= 30) #::
      LazyList.empty
    )

    it should behave like supportMoveToElement("empty set",
      emptyCase.get,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like supportMoveToElement("universal set",
      universalCase.get,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like supportMoveToElement("single bounded set",
      singleBoundedCase.get,
      ( 10`)`, false forAll x >  0) ::
      ( 15`[`, false forAll x >  0) ::
      (-10`)`, true  forAll x <= 0) ::
      (-15`[`, true  forAll x <= 0) ::
      (  0`(`, false forAll x >  0) ::
      (  0`]`, true  forAll x <= 0) ::
      Nil
    )

    it should behave like supportMoveToElement("multi bounded set",
      multiBoundedCase.get,
      (10`)`, true  forAll x >= 0  & x < 10) ::
      (10`)`, true  forAll x >= 0  & x < 10) ::
      (30`[`, false forAll x >= 30 & x < 40) ::
      (40`)`, false forAll x >= 30 & x < 40) ::
      (40`[`, true  forAll x >= 40) ::
      (45`[`, true  forAll x >= 40) ::
      (25`[`, true  forAll x >= 20 & x < 30) ::
      (-5`[`, false forAll x <  0) ::
      Nil
    )

    it should behave like supportMoveToElement("set with degenerate interval",
      degenerateCase.get,
      ( 0`]`, true  forAll x >= 0  & x <= 0) ::
      (20`(`, true  forAll x >  20 & x <  30) ::
      (-5`]`, false forAll x <  0) ::
      (45`[`, false forAll x >= 30) ::
      Nil
    )

    it should behave like supportMoveToElementAndThenMoveNext("empty set",
      emptyCase.get,
      ( 10`)`, None) ::
      ( 15`[`, None) ::
      (-10`)`, None) ::
      (-15`[`, None) ::
      Nil
    )

    it should behave like supportMoveToElementAndThenMoveNext("universal set",
      universalCase.get,
      ( 10`)`, None) ::
      ( 15`[`, None) ::
      (-10`)`, None) ::
      (-15`[`, None) ::
      Nil
    )

    it should behave like supportMoveToElementAndThenMoveNext("single bounded set",
      singleBoundedCase.get,
      ( 10`)`, None) ::
      ( 15`[`, None) ::
      (-10`)`, Some(false forAll x >  0)) ::
      (-15`[`, Some(false forAll x >  0)) ::
      (  0`(`, None) ::
      (  0`]`, Some(false forAll x >  0)) ::
      Nil
    )

    it should behave like supportMoveToElementAndThenMoveNext("multi bounded set",
      multiBoundedCase.get,
      (10`)`, Some(false forAll x >= 10 & x < 20)) ::
      (10`)`, Some(false forAll x >= 10 & x < 20)) ::
      (30`[`, Some(true  forAll x >= 40)) ::
      (40`)`, Some(true  forAll x >= 40)) ::
      (40`[`, None) ::
      (45`[`, None) ::
      (25`[`, Some(false forAll x >= 30 & x < 40)) ::
      (-5`[`, Some(true  forAll x >= 0  & x < 10)) ::
      Nil
    )

    it should behave like supportMoveToElementAndThenMoveNext("set with degenerate interval",
      degenerateCase.get,
      ( 0`]`, Some(false forAll x >  0  & x <  10)) ::
      (20`(`, Some(false forAll x >= 30)) ::
      (-5`]`, Some(true  forAll x >= 0  & x <= 0)) ::
      (45`[`, None) ::
      Nil
    )
  }
}