package test.ordset.core.specs.segmentSeq;

import ordset.core.{Bound, Segment, SegmentSeqOps}
import ordset.core.set.ArrayOrderedSet
import ordset.core.set.OrderedSet
import ordset.core.domain.{Domain, DomainOps}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.{Order, core}
import ordset.util.IterableUtil

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class SegmentSeqOpsSpec extends AnyFunSpec {

  import ordset.core.syntax.BoundSyntax._
  import ordset.core.syntax.SetBuilderNotation._
  import ordset.instances.tuple2._
  import ordset.instances.list._
  import ordset.core.instances.int._
  import ordset.core.instances.boolean._
  import test.ordset.core.TestRngUtil.Implicits._
  import test.ordset.core.SegmentSeqAssert._

  type Dom = Domain[Int]

  private val domainOps: DomainOps[Int, Dom] = implicitly[DomainOps[Int, Dom]]
  private val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom](domainOps)

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
        SegmentSeqOps.getUpperBoundsIterableFromSegment(seq1.getSegment(20 `)`), inclusive = false)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqOps.getUpperBoundsIterableFromSegment(seq1.getSegment(20 `)`), inclusive = true)
      )
    ))

    assert(listOrd.eqv(
      List(30 `)[`, 40 `)[`),
      List.from(
        SegmentSeqOps.getUpperBoundsIterableFromSegment(seq2.getSegment(20 `)`), inclusive = false)
      )
    ))

    assert(listOrd.eqv(
      List(20 `)[`, 30 `)[`, 40 `)[`),
      List.from(
        SegmentSeqOps.getUpperBoundsIterableFromSegment(seq2.getSegment(20 `)`), inclusive = true)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqOps.getUpperBoundsIterableFromSegment(seq2.getSegment(40 `)`), inclusive = false)
      )
    ))

    assert(listOrd.eqv(
      List(40 `)[`),
      List.from(
        SegmentSeqOps.getUpperBoundsIterableFromSegment(seq2.getSegment(40 `)`), inclusive = true)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqOps.getUpperBoundsIterableFromSegment(seq2.getSegment(50 `)`), inclusive = false)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqOps.getUpperBoundsIterableFromSegment(seq2.getSegment(50 `)`), inclusive = true)
      )
    ))
  }

  it("should get upper bounds iterable to segment") {

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqOps.getUpperBoundsIterableToSegment(seq1.getSegment(20 `)`), inclusive = false)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqOps.getUpperBoundsIterableToSegment(seq1.getSegment(20 `)`), inclusive = true)
      )
    ))

    assert(listOrd.eqv(
      Nil,
      List.from(
        SegmentSeqOps.getUpperBoundsIterableToSegment(seq2.getSegment(-20 `)`), inclusive = false)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`),
      List.from(
        SegmentSeqOps.getUpperBoundsIterableToSegment(seq2.getSegment(-20 `)`), inclusive = true)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`, 10 `)[`),
      List.from(
        SegmentSeqOps.getUpperBoundsIterableToSegment(seq2.getSegment(20 `)`), inclusive = false)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`, 10 `)[`, 20 `)[`),
      List.from(
        SegmentSeqOps.getUpperBoundsIterableToSegment(seq2.getSegment(20 `)`), inclusive = true)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
      List.from(
        SegmentSeqOps.getUpperBoundsIterableToSegment(seq2.getSegment(50 `)`), inclusive = false)
      )
    ))

    assert(listOrd.eqv(
      List(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
      List.from(
        SegmentSeqOps.getUpperBoundsIterableToSegment(seq2.getSegment(50 `)`), inclusive = true)
      )
    ))
  }

  it("should get upper bounds list from segment") {

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqOps.getUpperBoundsListFromSegment(seq1.getSegment(20 `)`), inclusive = false)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqOps.getUpperBoundsListFromSegment(seq1.getSegment(20 `)`), inclusive = true)
    ))

    assert(listWithSizeOrd.eqv(
      (List(30 `)[`, 40 `)[`), 2),
      SegmentSeqOps.getUpperBoundsListFromSegment(seq2.getSegment(20 `)`), inclusive = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(20 `)[`, 30 `)[`, 40 `)[`), 3),
      SegmentSeqOps.getUpperBoundsListFromSegment(seq2.getSegment(20 `)`), inclusive = true)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqOps.getUpperBoundsListFromSegment(seq2.getSegment(40 `)`), inclusive = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(40 `)[`), 1),
      SegmentSeqOps.getUpperBoundsListFromSegment(seq2.getSegment(40 `)`), inclusive = true)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqOps.getUpperBoundsListFromSegment(seq2.getSegment(50 `)`), inclusive = false)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqOps.getUpperBoundsListFromSegment(seq2.getSegment(50 `)`), inclusive = true)
    ))
  }

  it("should get upper bounds list to segment") {

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqOps.getUpperBoundsListToSegment(seq1.getSegment(20 `)`), inclusive = false)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqOps.getUpperBoundsListToSegment(seq1.getSegment(20 `)`), inclusive = true)
    ))

    assert(listWithSizeOrd.eqv(
      (Nil, 0),
      SegmentSeqOps.getUpperBoundsListToSegment(seq2.getSegment(-20 `)`), inclusive = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`), 1),
      SegmentSeqOps.getUpperBoundsListToSegment(seq2.getSegment(-20 `)`), inclusive = true)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`, 10 `)[`), 2),
      SegmentSeqOps.getUpperBoundsListToSegment(seq2.getSegment(20 `)`), inclusive = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`, 10 `)[`, 20 `)[`), 3),
      SegmentSeqOps.getUpperBoundsListToSegment(seq2.getSegment(20 `)`), inclusive = true)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`), 5),
      SegmentSeqOps.getUpperBoundsListToSegment(seq2.getSegment(50 `)`), inclusive = false)
    ))

    assert(listWithSizeOrd.eqv(
      (List(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`), 5),
      SegmentSeqOps.getUpperBoundsListToSegment(seq2.getSegment(50 `)`), inclusive = true)
    ))
  }

  it("should convert segment sequence to iterable of (bound, value) tuples") {

    assertEqualBoundValueIterables(
      List((null, false)),
      SegmentSeqOps.getBoundValueIterableForSeq(seq1)
    )

    assertEqualBoundValueIterables(
      List((0 `)[`, true), (10 `)[`, false), (20 `)[`, true), (30 `)[`, false), (40 `)[`, true), (null, false)),
      SegmentSeqOps.getBoundValueIterableForSeq(seq2)
    )
  }

  it("should get bound segments") {

    def toList[E, D <: Domain[E], V](tuple: (Segment[E, D, V], Segment[E, D, V])): List[Segment[E, D, V]] =
      List(tuple._1, tuple._2)

    assertEqualRelationSeq(
      (true forAll x < 0) :: (false forAll x >= 40) :: Nil,
      toList(SegmentSeqOps.getBoundSegments(seq1.firstSegment, seq2)).map(_.intervalRelation)
    )

    assertEqualRelationSeq(
      (false forAll x >= 0 & x < 10) :: (false forAll x >= 20 & x < 30) :: Nil,
      toList(SegmentSeqOps.getBoundSegments(seq3.getSegment(10 `]`), seq2)).map(_.intervalRelation)
    )

    assertEqualRelationSeq(
      (false forAll x >= 0 & x < 10) :: (false forAll x >= 0 & x < 10) :: Nil,
      toList(SegmentSeqOps.getBoundSegments(seq3.getSegment(3 `]`), seq2)).map(_.intervalRelation)
    )

    assertEqualRelationSeq(
      (false forAll x) :: (false forAll x) :: Nil,
      toList(SegmentSeqOps.getBoundSegments(seq2.getSegment(15 `[`), seq1)).map(_.intervalRelation)
    )

    assertEqualRelationSeq(
      (false forAll x) :: (false forAll x) :: Nil,
      toList(SegmentSeqOps.getBoundSegments(seq2.getSegment(50 `[`), seq1)).map(_.intervalRelation)
    )
  }
}
