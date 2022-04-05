### Overview

Implementation of ordered sets and maps.

Links:
- [project blog](https://ordset.blogspot.com/)
- [usage examples](https://github.com/earogov/ordset/tree/master/ordset/test/src/ordset/test/core/examples/segmentSeq)

### Getting Started

Project is available for [Scala 3.1.1](https://www.scala-lang.org/download/3.1.1.html).

Configure your build file (examples for `build.sbt` of SBT build tool):

1. Add dependencies:

```scala
libraryDependencies += "io.github.earogov" %% "ordset" % "0.1.0"
libraryDependencies += "io.github.earogov" %% "ordset-commonsRandom" % "0.1.0"
```

Modules:

|          name          |                  description                  
| ---------------------- | ----------------------------------------------
| `ordset`               | Core module.
| `ordset-commonsRandom` | Random generator based on [Apache Commons RNG](https://commons.apache.org/proper/commons-rng/). Generator is used internally by ordered sets and maps.

2. If you need a snapshot version, add corresponding [sonatype repository](https://s01.oss.sonatype.org):

```scala
resolvers += Resolver.sonatypeRepo("snapshots")
```

### Example

Assume we have unbounded discrete domain of big integers:

```scala
import ordset.givens.bigInt.*

type Dom[X] = Domain.DiscreteUnbounded[X]

implicit val domainOps: DomainOps[BigInt, Dom] = DomainOps.default

val x: BoundBuilder[BigInt, Dom] = BoundBuilder[BigInt, Dom]
```

Let's create an ordered sets on it:

```scala
val a = OrderedSet.Try(
  x <= 10,
  x >= 20 & x <= 30,
  x >= 40
).get

println("")
println(s"a = $a")

val b = OrderedSet.Try(
  x <= 5,
  x >= 15 & x <= 25,
  x >= 35 
).get

println("")
println(s"b = $b")
```

The output is:

```
a = 
{
  {x <= 10} -> true,
  {10 < x < 20} -> false,
  {20 <= x <= 30} -> true,
  {30 < x < 40} -> false,
  {x >= 40} -> true
}

b = 
{
  {x <= 5} -> true,
  {5 < x < 15} -> false,
  {15 <= x <= 25} -> true,
  {25 < x < 35} -> false,
  {x >= 35} -> true
}
```

Here boolean values indicates whether interval is included in set or not.
Now we can transform and compose sets with each other using familiar operators, for example:

```scala
val c = ~(a | b)

println("")
println(s"c = ~(a | b) = $c")

val d = ~a & ~b

println("")
println(s"d = ~a & ~b = $d")
```

We will get:

```
c = ~(a | b) = 
{
  {x <= 10} -> false,
  {10 < x < 15} -> true,
  {15 <= x <= 30} -> false,
  {30 < x < 35} -> true,
  {x >= 35} -> false
}

d = ~a & ~b = 
{
  {x <= 10} -> false,
  {10 < x < 15} -> true,
  {15 <= x <= 30} -> false,
  {30 < x < 35} -> true,
  {x >= 35} -> false
}
```

Ordered map is a similar concept, but instead of boolean value each interval is associated with some value of
arbitrary type:

```scala
implicit val booleanOps: ValueOps[Boolean] = ValueOps.booleanValueOps

println("Initial ordered map:")
val map1  = OrderedMap.Try(
  0,
  -20 forAll x < 0,
  -5 forAll x >= 0 & x < 10,
  10 forAll x >= 10 & x < 20,
  -7 forAll x >= 20 & x < 30,
  15 forAll x >= 30 & x < 40,
  20 forAll x >= 40
).get
println(map1)

println("Received ordered set:")
val set1 = map1.map(_ >= 0)
println(set1)
```

It will print out:

```
Initial ordered map:
{
  {x < 0} -> -20,
  {0 <= x < 10} -> -5,
  {10 <= x < 20} -> 10,
  {20 <= x < 30} -> -7,
  {30 <= x < 40} -> 15,
  {x >= 40} -> 20
}

Received ordered set:
{
  {x < 10} -> false,
  {10 <= x < 20} -> true,
  {20 <= x < 30} -> false,
  {x >= 30} -> true
}
```

There a lot of other map operators such as zip, flatMap, patch, etc. And of course we can get a value associated with
any element or bound, iterate over intervals and and so forth.