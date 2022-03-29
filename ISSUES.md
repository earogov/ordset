1. Optimize patch with mapped and zipped sequences.

Assume we have some sequence s. We need to patch it with sequence p inside bound b1 and b2, and p is mapped or zipped sequence.

Current implementation does the following:

val tmp = s.appendAboveBound(b1, p)
val result = s.prependBelowBound(b2, tmp)

If s for example a treap sequence then first step converts into treap a part of p that is greater than b1. But actually we don't need part greater than b2 to be converted as it will be dropped at step 2.

2. Consider adding segment (or interval) as input parameter to SeqSupplier function.

It will allows to optimize computation of lazy values when only part of sequence required. For example if lazy segment becomes shorter, we can return smaller part of sequence.

3. [Closed] Fix stack unsafe implementation of takeAboveBound and takeBelowBound in LazyTreapSegmentSeq.

If there is a lazy segment at specified bound then we create new lazy value. New function takes old one and modifies its result. Multiple calls of takeAboveBound/takeAboveBound methods with the same bound can produce function that causes stack overflow.

4. Add `span` method to IntervalAlgebra (see RangeAlgebra).

5. [Closed] Fix nullable caches.

Consider we have some internal state of type `C`, that has not been initialized yet:

var cache: C | Null = null

Next we get some value and put it into cache. Later we do `cache != null` checks to avoid repeated initialization, etc.
But if type `C` is itself nullable, then after initialization cache may contain `null`. This can lead to the wrong
behavior. Such cases are spread all over the project. Consider to replace `null` with some dummy object (available
only inside library) to explicitly define uninitialized state.

6. Override `contravariant` and `reversed` methods of `DefaultOrder` for `Bound` and `ExtendedBound`.

`DefaultOrder` also extends `Hash`. We should return more precise type, that includes `Hash`, for  `contravariant` and `reversed` methods.

7. [Closed] Provide `StrictSegmentSeq` subtype of segment sequence.

It will allow distinguish lazy sequences (which need some additional computations to return segment or value) from
strict ones, for instance, `TreapOrderedSet`, `ArrayOrderedSet`, etc.

8. [Closed] Provide `strict` method for segment sequence.

Method should convert lazy sequence into strict one with subtype `StrictSegmentSeq` (see issue 7).

9. Further optimization of `strict` method of `AbstractLazyTreapSegmentSeq`.

See docs for `ZSegmentSeqBuilder.strictZippedSeq`.

10. Optimize `inverse` method for NonuniformTreapOrderedSet and NonuniformTreapOrderedMap.

Current implementation of `inverse` method just creates mapped segment sequence. Method can be significantly optimized
by integrating `complementary` flag into treap data structure. Each node should be able to store `complementary`
flag and ordinary boolean value. If `complementary` is `true` for some node, then ordinary value of this node and values
of all its children should be inverted. This will allow to get super fast `inverse` method, but will require rather 
complex implementation of merge operation for treaps (for cases when we need to merge nodes with different 
`complementary` flags).
