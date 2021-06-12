package test.ordset.core.samples.zippedSeq.zippedSet

import ordset.core.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchedTest
import test.ordset.core.samples.segmentSeq.ZippedSeqSample
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._

import scala.language.postfixOps
import scala.collection.immutable.ArraySeq

class MultiBoundedSetSampleZ1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
    with OriginalSeqPatchedTest[Int, D, Boolean, Boolean] {

  override val sample: String = "Z1"

  override val labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  val firstSeq: TreapOrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuildAsc(
    ArraySeq(10`](`, 20`](`, 30`)[`),
    complementary = true,
    domainOps
  )()

  val secondSeq: TreapOrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuildAsc(
    ArraySeq(5`)[`, 15`)[`, 25`](`, 35`](`),
    complementary = false,
    domainOps
  )()

  // a:
  // X-----t----](------f-----](-------t-----)[-------f------X
  //            10            20             30
  // b:
  // X--f--)[-----t----)[------f-----](------t-------](---f--X
  //       5           15            25              35
  //
  // a intersection b:
  // X--f--)[-t-](---------f---------](---t--)[------f-------X
  //       5    10                   25      30
  override def sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.intersection(firstSeq, secondSeq)

  override def firstSeqPatchedCases: Seq[OriginalSeqPatchedTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchedTest.TestCase(
      labels = Set(Label("A1")),
      bound = 15`[`,
      patch = UniformOrderedSet.defaultEmpty,
      expected =
        (true  forAll x <= 10) ::
        (false forAll x >  10  & x <= 25) ::
        (true  forAll x >  25  & x <  30) ::
        (false forAll x >= 30) ::
        Nil
    ),
    OriginalSeqPatchedTest.TestCase(
      labels = Set(Label("A2")),
      bound = 0`(`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(0`](`, 15`](`),
        complementary = true
      ),
      expected =
        (true  forAll x <= 0) ::
        (false forAll x >  0  & x <  5) ::
        (true  forAll x >= 5  & x <= 10) ::
        (false forAll x >  10 & x <= 20) ::
        (true  forAll x >  20 & x <  30) ::
        (false forAll x >= 30) ::
        Nil
    ),
    OriginalSeqPatchedTest.TestCase(
      labels = Set(Label("A3")),
      bound = 35`(`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(35`](`, 45`](`),
        complementary = true
      ),
      expected =
        (true  forAll x <= 10) ::
        (false forAll x >  10  & x <= 20) ::
        (true  forAll x >  20  & x <= 35) ::
        (false forAll x >  35  & x <= 45) ::
        (true  forAll x >  45) ::
        Nil
    )
  )

  override def secondSeqPatchedCases: Seq[OriginalSeqPatchedTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchedTest.TestCase(
      labels = Set(Label("B1")),
      bound = 15`[`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(17`)[`),
        complementary = true
      ),
      expected =
        (false forAll x <  5) ::
        (true  forAll x >= 5  & x <  17) ::
        (false forAll x >= 17 & x <= 25) ::
        (true  forAll x >  25 & x <= 35) ::
        (false forAll x >  35) ::
        Nil
    ),
    OriginalSeqPatchedTest.TestCase(
      labels = Set(Label("B2")),
      bound = 0`(`,
      patch = UniformOrderedSet.defaultUniversal,
      expected =
        (true  forAll x <  15) ::
        (false forAll x >= 15 & x <= 25) ::
        (true  forAll x >  25 & x <= 35) ::
        (false forAll x >  35) ::
        Nil
    ),
    OriginalSeqPatchedTest.TestCase(
      labels = Set(Label("B3")),
      bound = 40`(`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(30`)[`),
        complementary = true
      ),
      expected =
        (false forAll x <  5) ::
        (true  forAll x >= 5  & x <  15) ::
        (false forAll x >= 15 & x <= 25) ::
        (true  forAll x >  25 & x <  30) ::
        (false forAll x >= 30) ::
        Nil
    )
  )
}
