package ordset.test.core.examples.segmentSeq

import ordset.core.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{TreapOrderedMap, UniformOrderedMap, OrderedMap}
import ordset.core.set.{TreapOrderedSet, UniformOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.test.core.implementations.domain.BoundSelector

import scala.language.postfixOps
import ordset.core.map.MappedValueOrderedMap

object PatchLazyExample {

  import ordset.core.instances.boolean.*
  import ordset.core.instances.int.*
  import ordset.test.core.TestRngUtil.Implicits.*

  private val sep = "-----------------"

  private val stringValueOps: ValueOps[String] = implicitly
  private val domainOps: DomainOps[Int, Domain.UnboundedContinuous[Int]] = implicitly

  @main
  def patchLazyExampleMain(): Unit = {
    example1()
    example2()
    example3()
  }

  def example1(): Unit = {
    println()
    println(s"$sep SegmentSeq.patchLazy example 1 $sep")

    println("Initial treap ordered map:")
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
      SeqSupplier.ValueOpsImpl.get
    )()
    println(lazySeq)

    println()
    println("Each segment of sequence above optionally contains function that evolves into new sequence.")
    println("Functions are computed lazily only if corresponding segment is requested.")
    println("So patch sequence hadn't been initialized yet.")

    println()
    println("After lazy patch is applied we will get the following sequence:")
    val seq2 = seq1.patchLazy(lazySeq)
    println(seq2)

    val bound1 = Bound.Upper.including(20)
    println()
    println(s"Now let's request segment at bound $bound1")
    seq2.getSegmentForBound(bound1)

    println()
    println("Patch sequence was built and cached:")
    println(seq2)

    println()
    println("Compute all lazy values:")
    println(TreapOrderedMap.getFactory.convertMap(seq2))
  }

  def example2(): Unit = {
    println()
    println(s"$sep SegmentSeq.patchLazy example 2 $sep")

    println("Initial mapped ordered map:")
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
    val seq2 = MappedValueOrderedMap.identity(seq1)
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
      SeqSupplier.ValueOpsImpl.get
    )()
    println(lazySeq)

    println()
    println("After lazy patch is applied we will get the following sequence:")
    val seq3 = seq2.patchLazy(lazySeq)
    println(seq3)

    println()
    println("The sequence above consists only of lazy segments.")
    println("This optimization allows to avoid immediate conversion of initial mapped sequence into treap based sequence")
    println("(which is used internally by lazy sequence). The conversion will take place only when corresponding segment is requested.")
    println("And even then the minimal possible part of sequence will be converted to create required segment.")

    val bound1 = Bound.Upper.including(-5)
    println()
    println(s"No let's request segment at bound $bound1")
    seq3.getSegmentForBound(bound1)

    println()
    println("Patch sequence was not built yet, because there is no need.")

    println()
    println("Compute all lazy values:")
    println(TreapOrderedMap.getFactory.convertMap(seq3))
  }

  def example3(): Unit = {
    println()
    println(s"$sep SegmentSeq.patchLazy example 3 $sep")

    println("Initial lazy ordered map:")
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
    val seq2 = seq1.flatMap(s => seq1)
    println(seq2)

    println()
    println("Let's patch it with sequence:")
    val lazySeq = TreapOrderedMap.getFactory.unsafeBuildAsc(
      List(
        (15`)[`, None),
        (35`](`, Some(() => buildPatchSeq())),
        (AboveAll, None)
      ),
      domainOps,
      SeqSupplier.ValueOpsImpl.get
    )()
    println(lazySeq)

    println()
    println("After lazy patch is applied we will get the following sequence:")
    val seq3 = seq2.patchLazy(lazySeq)
    println(seq3)
    
    println()
    println("Initial sequence was lazy. Patch operation hasn't create one more lazy level.")
    println("Instead it took internal structure of initial lazy sequence and created new one replacing some regions.")
    println("This optimization makes series of lazy operations much more efficient - resulting sequences don't have")
    println("overhead of multiple nested levels.")

    println()
    println("Compute all lazy values:")
    println(TreapOrderedMap.getFactory.convertMap(seq3))
  }

  private def buildPatchSeq(): OrderedMap[Int, Domain.UnboundedContinuous[Int], String] = {
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
    println("Patch sequence initialized:")
    println(patchSeq)
    println("<<<-")
    patchSeq
  }
}
