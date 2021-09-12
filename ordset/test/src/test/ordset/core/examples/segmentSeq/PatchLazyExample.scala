package test.ordset.core.examples.segmentSeq

import ordset.core.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{TreapOrderedMap, UniformOrderedMap, OrderedMap}
import ordset.core.set.{TreapOrderedSet, UniformOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.value.ValueOps
import ordset.random.RngManager
import test.ordset.core.implementations.domain.BoundSelector

import scala.language.postfixOps

object PatchLazyExample {

  import ordset.core.instances.boolean.*
  import ordset.core.instances.int.*
  import test.ordset.core.TestRngUtil.Implicits.*

  private val sep = "-----------------"

  private val stringValueOps: ValueOps[String] = implicitly[ValueOps[String]]
  private val domainOps: DomainOps[Int, Domain[Int]] = implicitly[DomainOps[Int, Domain[Int]]]

  @main
  def patchLazyExampleMain(): Unit = {
    example1()
  }

  def example1(): Unit = {
    println()
    println(s"$sep SegmentSeq.patchLazy example $sep")

    def buildPatchSeq(): OrderedMap[Int, Domain[Int], String] = {
      val patchSeq = TreapOrderedMap.getFactory.unsafeBuildAsc(
        List(
          (5`)[`, "X"),
          (25`](`, "Y"),
          (AboveAll, "Z")
        ),
        domainOps,
        stringValueOps
      )()
      println("->>>")
      println("Built patch sequence:")
      println(patchSeq)
      println("<<<-")
      patchSeq
    }

    println("Initial sequence:")
    val seq1 = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (0`)[`, "A"),
        (10`)[`, "B"),
        (20`)[`, "C"),
        (30`)[`, "D"),
        (40`)[`, "E"),
        (AboveAll, "F")
      ),
      domainOps,
      stringValueOps
    )()
    println(seq1)

    println()
    println("Let's patch it with sequence:")
    val lazySeq = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (15`)[`, None),
        (35`](`, Some(() => buildPatchSeq())),
        (AboveAll, None)
      ),
      domainOps,
      OptionalSeqSupplier.ValueOpsImpl.get
    )()
    println(lazySeq)

    println()
    println("Each segment of sequence above optionally contains function that evolves into new sequence.")
    println("Functions are computed lazily only if corresponding segment is requested.")
    println("So patch sequence hadn't been built yet.")

    println()
    println("After lazy patch is applied we will get the following sequence:")
    val seq2 = seq1.patchLazy(lazySeq)
    println(seq2)

    // TODO fix `patchLazy` to get one lazy segment instead of three (because lambda is the same):
    //  ...
    //  {15 <= x < 20} -> (C,LazyValue(test.ordset.core.examples.segmentSeq.PatchLazyExample$$$Lambda$34/0x0000000800c68040@1f1c7bf6)),
    //  {20 <= x < 30} -> (D,LazyValue(test.ordset.core.examples.segmentSeq.PatchLazyExample$$$Lambda$34/0x0000000800c68040@1f1c7bf6)),
    //  {30 <= x <= 35} -> (E,LazyValue(test.ordset.core.examples.segmentSeq.PatchLazyExample$$$Lambda$34/0x0000000800c68040@1f1c7bf6)),
    //  ...

    val bound1 = Bound.Upper.inclusive(20)
    println()
    println(s"No let's request segment at bound $bound1")
    seq2.getSegmentForBound(bound1)

    println()
    println("Patch sequence was built and cached:")
    println(seq2)

    println()
    println("Compute all lazy values:")
    println(TreapOrderedMap.getFactory.convertMap(seq2))
  }
}
