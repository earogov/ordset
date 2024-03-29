package ordset.test.core.specs.util

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.*
import ordset.core.segmentSeq.set.{ArrayOrderedSet, OrderedSet}
import ordset.core.segmentSeq.util.SegmentSeqUtil
import ordset.core.{Bound, ExtendedBound}
import ordset.util.IterableUtil
import ordset.{Order, core}
import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class SegmentSeqUtilSpec extends AnyFunSpec {

  import ordset.givens.boolean._
  import ordset.givens.int._
  import ordset.core.syntax.BoundSyntax._
  import ordset.core.syntax.SetBuilderNotation._
  import ordset.givens.list._
  import ordset.givens.tuple2._
  import ordset.test.core.SegmentSeqAssertions._
  import ordset.test.core.IntervalAssertions._
  import ordset.test.core.TestRngUtil.Givens._

  type Dom[X] = Domain.ContinuousUnbounded[X]

  private val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]

  private val seq1: OrderedSet[Int, Dom] = ArrayOrderedSet.unchecked[Int, Dom](
    ArraySeq.empty,
    complementary = false
  )

  private val seq2: OrderedSet[Int, Dom] = ArrayOrderedSet.unchecked[Int, Dom](
    ArraySeq(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
    complementary = true
  )

  private val seq3: OrderedSet[Int, Dom] = ArrayOrderedSet.unchecked[Int, Dom](
    ArraySeq(2 `)[`, 5 `)[`, 25 `)[`),
    complementary = false
  )

  private val listOrd: Order[List[Bound[Int]]] = implicitly[Order[List[Bound[Int]]]]
  private val listWithSizeOrd: Order[(List[Bound[Int]], Int)] = implicitly[Order[(List[Bound[Int]], Int)]]

  it("should get upper bounds iterable from segment") {

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableFromSegment(seq1.getSegmentForBound(20 `)`), including = false)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableFromSegment(seq1.getSegmentForBound(20 `)`), including = true)
      )
    ))

    assert(listOrd.eqv(
      List(30 `)[`, 40 `)[`),
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableFromSegment(seq2.getSegmentForBound(20 `)`), including = false)
      )
    ))

    assert(listOrd.eqv(
      List(20 `)[`, 30 `)[`, 40 `)[`),
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableFromSegment(seq2.getSegmentForBound(20 `)`), including = true)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableFromSegment(seq2.getSegmentForBound(40 `)`), including = false)
      )
    ))

    assert(listOrd.eqv(
      List(40 `)[`),
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableFromSegment(seq2.getSegmentForBound(40 `)`), including = true)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableFromSegment(seq2.getSegmentForBound(50 `)`), including = false)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableFromSegment(seq2.getSegmentForBound(50 `)`), including = true)
      )
    ))
  }

  it("should get upper bounds iterable to segment") {

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableToSegment(seq1.getSegmentForBound(20 `)`), including = false)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableToSegment(seq1.getSegmentForBound(20 `)`), including = true)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableToSegment(seq2.getSegmentForBound(-20 `)`), including = false)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`),
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableToSegment(seq2.getSegmentForBound(-20 `)`), including = true)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`, 10 `)[`),
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableToSegment(seq2.getSegmentForBound(20 `)`), including = false)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`, 10 `)[`, 20 `)[`),
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableToSegment(seq2.getSegmentForBound(20 `)`), including = true)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableToSegment(seq2.getSegmentForBound(50 `)`), including = false)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
      List.from(
        SegmentSeqUtil.getUpperBoundsIterableToSegment(seq2.getSegmentForBound(50 `)`), including = true)
      )
    ))
  }

  it("should get upper bounds list from segment") {

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqUtil.getUpperBoundsListFromSegment(seq1.getSegmentForBound(20 `)`), including = false)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqUtil.getUpperBoundsListFromSegment(seq1.getSegmentForBound(20 `)`), including = true)
    ))

    assert(listWithSizeOrd.eqv(
      (List(30 `)[`, 40 `)[`), 2),
      SegmentSeqUtil.getUpperBoundsListFromSegment(seq2.getSegmentForBound(20 `)`), including = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(20 `)[`, 30 `)[`, 40 `)[`), 3),
      SegmentSeqUtil.getUpperBoundsListFromSegment(seq2.getSegmentForBound(20 `)`), including = true)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqUtil.getUpperBoundsListFromSegment(seq2.getSegmentForBound(40 `)`), including = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(40 `)[`), 1),
      SegmentSeqUtil.getUpperBoundsListFromSegment(seq2.getSegmentForBound(40 `)`), including = true)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqUtil.getUpperBoundsListFromSegment(seq2.getSegmentForBound(50 `)`), including = false)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqUtil.getUpperBoundsListFromSegment(seq2.getSegmentForBound(50 `)`), including = true)
    ))
  }

  it("should get upper bounds list to segment") {

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqUtil.getUpperBoundsListToSegment(seq1.getSegmentForBound(20 `)`), including = false)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqUtil.getUpperBoundsListToSegment(seq1.getSegmentForBound(20 `)`), including = true)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqUtil.getUpperBoundsListToSegment(seq2.getSegmentForBound(-20 `)`), including = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`), 1),
      SegmentSeqUtil.getUpperBoundsListToSegment(seq2.getSegmentForBound(-20 `)`), including = true)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`, 10 `)[`), 2),
      SegmentSeqUtil.getUpperBoundsListToSegment(seq2.getSegmentForBound(20 `)`), including = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`, 10 `)[`, 20 `)[`), 3),
      SegmentSeqUtil.getUpperBoundsListToSegment(seq2.getSegmentForBound(20 `)`), including = true)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`), 5),
      SegmentSeqUtil.getUpperBoundsListToSegment(seq2.getSegmentForBound(50 `)`), including = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`), 5),
      SegmentSeqUtil.getUpperBoundsListToSegment(seq2.getSegmentForBound(50 `)`), including = true)
    ))
  }

  it("should convert segment sequence to iterable of (bound, value) tuples") {

    assertSameBoundValueSeq[Int, Dom, Boolean](
      List((ExtendedBound.AboveAll, false)),
      SegmentSeqUtil.getBoundValueIterableForSeq(seq1)
    )

    assertSameBoundValueSeq[Int, Dom, Boolean](
      List(
        (0 `)[`, true),
        (10 `)[`, false),
        (20 `)[`, true),
        (30 `)[`, false),
        (40 `)[`, true),
        (ExtendedBound.AboveAll, false)
      ),
      SegmentSeqUtil.getBoundValueIterableForSeq(seq2)
    )
  }

  it("should get bound segments") {

    def toList[E, D[X] <: Domain[X], V](tuple: (Segment[E, D, V], Segment[E, D, V])): List[Segment[E, D, V]] =
      List(tuple._1, tuple._2)

    assertSameRelationSeq(
      (true forAll x < 0) :: (false forAll x >= 40) :: Nil,
      toList(SegmentSeqUtil.getBoundSegments(seq1.firstSegment, seq2)).map(_.intervalRelation)
    )

    assertSameRelationSeq(
      (false forAll x >= 0 & x < 10) :: (false forAll x >= 20 & x < 30) :: Nil,
      toList(SegmentSeqUtil.getBoundSegments(seq3.getSegmentForBound(10 `]`), seq2)).map(_.intervalRelation)
    )

    assertSameRelationSeq(
      (false forAll x >= 0 & x < 10) :: (false forAll x >= 0 & x < 10) :: Nil,
      toList(SegmentSeqUtil.getBoundSegments(seq3.getSegmentForBound(3 `]`), seq2)).map(_.intervalRelation)
    )

    assertSameRelationSeq(
      (false forAll x) :: (false forAll x) :: Nil,
      toList(SegmentSeqUtil.getBoundSegments(seq2.getSegmentForBound(15 `[`), seq1)).map(_.intervalRelation)
    )

    assertSameRelationSeq(
      (false forAll x) :: (false forAll x) :: Nil,
      toList(SegmentSeqUtil.getBoundSegments(seq2.getSegmentForBound(50 `[`), seq1)).map(_.intervalRelation)
    )
  }
  
  it("should get lower bound segment") {
    
    assertSameRelationAndSegment(
      true forAll x < 0,
      SegmentSeqUtil.getLowerBoundSegment(seq1.firstSegment, seq2)
    )

    assertSameRelationAndSegment(
      false forAll x >= 0 & x < 10,
      SegmentSeqUtil.getLowerBoundSegment(seq3.getSegmentForBound(10 `]`), seq2)
    )

    assertSameRelationAndSegment(
      false forAll x >= 0 & x < 10,
      SegmentSeqUtil.getLowerBoundSegment(seq3.getSegmentForBound(3 `]`), seq2)
    )

    assertSameRelationAndSegment(
      false forAll x,
      SegmentSeqUtil.getLowerBoundSegment(seq2.getSegmentForBound(15 `[`), seq1)
    )

    assertSameRelationAndSegment(
      false forAll x,
      SegmentSeqUtil.getLowerBoundSegment(seq2.getSegmentForBound(50 `[`), seq1)
    )
  }

  it("should get upper bound segment") {

    assertSameRelationAndSegment(
      false forAll x >= 40,
      SegmentSeqUtil.getUpperBoundSegment(seq1.firstSegment, seq2)
    )

    assertSameRelationAndSegment(
      false forAll x >= 20 & x < 30,
      SegmentSeqUtil.getUpperBoundSegment(seq3.getSegmentForBound(10 `]`), seq2)
    )

    assertSameRelationAndSegment(
      false forAll x >= 0 & x < 10,
      SegmentSeqUtil.getUpperBoundSegment(seq3.getSegmentForBound(3 `]`), seq2)
    )

    assertSameRelationAndSegment(
      false forAll x,
      SegmentSeqUtil.getUpperBoundSegment(seq2.getSegmentForBound(15 `[`), seq1)
    )

    assertSameRelationAndSegment(
      false forAll x,
      SegmentSeqUtil.getUpperBoundSegment(seq2.getSegmentForBound(50 `[`), seq1)
    )
  }
}
