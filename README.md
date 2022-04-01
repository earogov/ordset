### Overview

Implementation of ordered sets and maps.

Links:
- [project blog](https://ordset.blogspot.com/)
- [usage examples](https://github.com/earogov/ordset/tree/master/ordset/test/src/ordset/test/core/examples/segmentSeq)

### Example

Assume we have unbounded discrete domain of big integers:

```scala
type Dom[X] = Domain.DiscreteUnbounded[X]

implicit val domainOps: DomainOps[BigInt, Dom] = DomainOps.UnboundedOps.default[BigInt, Dom]

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

### Installation

1. [Install](https://com-lihaoyi.github.io/mill/#installation) Mill build tool.

2. Clone project:

```dtd
$ cd /Users/{user}/{projects}
$ git clone git://github.com/earogov/ordset
```

3. Run tests (main module):

```dtd
$ cd /Users/{user}/{projects}/ordset
$ mill ordset.test
```

4.a [Optional] Use Visual Studio Code with [Metals](https://scalameta.org/metals/docs/editors/vscode/) plugin.

4.b. [Optional] Configure IntelliJ IDEA project with BSP ([Build Server Protocol](https://build-server-protocol.github.io/)).

- Open IntelliJ IDEA and choose File -> New -> Project from Existing Sources

- Select project directory /Users/{user}/{projects}/ordset

- Choose `Import project from external model` and select `BSP`. New project will be opened with `bsp` toolbar available 
  on the right side.
  
- Configure BSP integration running script:

```dtd
$ cd /Users/{user}/{projects}/ordset
$ mill mill.bsp.BSP/install  
```

- Now you can update IDEA modules definition with button `Reload All BSP Projects` on `bsp` toolbar.
Syntax highlight will be also available in `build.sc` file (Mill build configuration).

Note, project restart may be required for the changes to take effect.