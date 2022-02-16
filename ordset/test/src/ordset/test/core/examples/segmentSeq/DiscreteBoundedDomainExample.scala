package ordset.test.core.examples.segmentSeq

import ordset.core.domain.{DomainOps, Domain}
import ordset.core.segmentSeq.map.TreapOrderedMap
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import scala.language.postfixOps

object DiscreteBoundedDomainExample {

  import ordset.givens.int.*
  import ordset.givens.string.*
  import ordset.implementations.int
  import ordset.test.core.TestRngUtil.Givens.*
  
  def main(args: Array[String]): Unit = {

    println("Let's take discrete domain of integers with bounds [0, 100].")
    implicit val domainOps: DomainOps[Int, Domain.DiscreteBounded] = 
      DomainOps.BoundedOps.default(
        Domain.DiscreteBounded.default(
          int.tryNaturalOrderWithBounds(0, 100).get
        )
      )

    println("And build on it ordered map with the following bounds and values: ")
    println("                                         ")
    println("    abc         xyz         zzz          ")
    println("----------]-----------)-----------X      ")
    println("         10          80       AboveAll   ")
    println("                                         ")
    println("                 Pic.1                   ")
    val map1 = TreapOrderedMap.getFactory[Int, Domain.DiscreteBounded, String].unsafeBuildAsc(
      List(
        (10 `]`, "abc"),
        (80 `)`, "xyz"),
        (AboveAll, "zzz")
      )
    )
    println("")
    println("The result map is:")
    println(map1)

    println("")
    println("Note that the domain bounds have been substituted as lower and upper bounds of the map.")
    println("However, map has exactly the same segments as shown on the Pic. 1.")
    println("So we can even request segment for element below lower bound of the domain.")
    println("For instance, for element -1 we will receive:")
    val s1 = map1.getSegmentForElement(-1)

    println("")
    println(s1)

    println("")
    println("And then segment can be converted into interval with respect of domain bounds:")

    println("")
    println(s1.intervalRelation)

    println("")
    println("Now let's get segment for bound `x < 11` or 11`)`.")
    println("Because of our domain is discrete this bound equals to `x <= 10` or 10`]`.")
    println("So we get the same segment:")
    val s2 = map1.getSegmentForBound(11`)`)

    println("")
    println(s2)
  }
}
