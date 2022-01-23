package ordset.core.internal

import ordset.core.domain.Domain
import ordset.core.{SegmentT, SegmentTruncationT, TreapSegmentBase, TreapSegmentSeq, UniformSegment}
import ordset.core.{ZippedSegmentBase, ZippedSegmentSeq}

package object lazySeq {
  
  protected[ordset] type ZValue[E, D[X] <: Domain[X], V] = (V, ControlValue[E, D, V])


  protected[ordset] type BaseSegmentBase[E, D[X] <: Domain[X], V] = TreapSegmentBase[E, D, V] | UniformSegment[E, D, V]

  protected[ordset] type BaseSegment[E, D[X] <: Domain[X], V] = SegmentT[E, D, V, BaseSegmentBase[E, D, V]]

  protected[ordset] type BaseSegmentSeq[E, D[X] <: Domain[X], V] = TreapSegmentSeq[E, D, V]


  protected[ordset] type ControlSegmentBase[E, D[X] <: Domain[X], V] =
    TreapSegmentBase[E, D, ControlValue[E, D, V]] | UniformSegment[E, D, ControlValue[E, D, V]]

  protected[ordset] type ControlSegment[E, D[X] <: Domain[X], V] = 
    SegmentT[E, D, ControlValue[E, D, V], ControlSegmentBase[E, D, V]]

  protected[ordset] type ControlSegmentWithNext[E, D[X] <: Domain[X], V] = 
    SegmentT.WithNext[E, D, ControlValue[E, D, V], ControlSegmentBase[E, D, V]]

  protected[ordset] type ControlSegmentWithPrev[E, D[X] <: Domain[X], V] = 
    SegmentT.WithPrev[E, D, ControlValue[E, D, V], ControlSegmentBase[E, D, V]]

  protected[ordset] type ControlTruncation[E, D[X] <: Domain[X], V] =
    SegmentTruncationT[E, D, ControlValue[E, D, V], ControlSegmentBase[E, D, V], ControlSegment[E, D, V]]

  protected[ordset] type ControlSegmentSeq[E, D[X] <: Domain[X], V] = TreapSegmentSeq[E, D, ControlValue[E, D, V]]


  protected[ordset] type ZSegmentBase[E, D[X] <: Domain[X], V] =
    ZippedSegmentBase[
      E,
      D,
      V,
      ControlValue[E, D, V],
      ZValue[E, D, V],
      BaseSegmentBase[E, D, V],
      ControlSegmentBase[E, D, V]
    ]

  protected[ordset] type ZSegment[E, D[X] <: Domain[X], V] = 
    SegmentT[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentWithNext[E, D[X] <: Domain[X], V] = 
    SegmentT.WithNext[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentWithPrev[E, D[X] <: Domain[X], V] = 
    SegmentT.WithPrev[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentInitial[E, D[X] <: Domain[X], V] = 
    SegmentT.Initial[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentTerminal[E, D[X] <: Domain[X], V] = 
    SegmentT.Terminal[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentInner[E, D[X] <: Domain[X], V] = 
    SegmentT.Inner[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentSingle[E, D[X] <: Domain[X], V] = 
    SegmentT.Single[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZTruncation[E, D[X] <: Domain[X], V] =
    SegmentTruncationT[
      E,
      D,
      ZValue[E, D, V],
      ZSegmentBase[E, D, V],
      ZSegment[E, D, V]
    ]

  protected[ordset] type ZTruncationWithPrev[E, D[X] <: Domain[X], V] =
    SegmentTruncationT[
      E,
      D,
      ZValue[E, D, V],
      ZSegmentBase[E, D, V],
      ZSegmentWithPrev[E, D, V]
    ]

  protected[ordset] type ZTruncationWithNext[E, D[X] <: Domain[X], V] =
    SegmentTruncationT[
      E,
      D,
      ZValue[E, D, V],
      ZSegmentBase[E, D, V],
      ZSegmentWithNext[E, D, V]
    ]

  protected[ordset] type ZSegmentSeq[E, D[X] <: Domain[X], V] =
    ZippedSegmentSeq[
      E,
      D,
      V,
      ControlValue[E, D, V],
      ZValue[E, D, V],
      BaseSegmentBase[E, D, V],
      ControlSegmentBase[E, D, V]
    ]
}
