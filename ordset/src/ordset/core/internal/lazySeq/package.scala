package ordset.core.internal

import ordset.core.domain.Domain
import ordset.core.{SegmentT, SegmentTruncationT, TreapSegmentBase, TreapSegmentSeq, UniformSegment}
import ordset.core.{ZippedSegmentBase, ZippedSegmentSeq}

package object lazySeq {
  
  protected[ordset] type ZValue[E, D <: Domain[E], V] = (V, ControlValue[E, D, V])


  protected[ordset] type BaseSegmentBase[E, D <: Domain[E], V] = TreapSegmentBase[E, D, V] | UniformSegment[E, D, V]

  protected[ordset] type BaseSegment[E, D <: Domain[E], V] = SegmentT[E, D, V, BaseSegmentBase[E, D, V]]

  protected[ordset] type BaseSegmentSeq[E, D <: Domain[E], V] = TreapSegmentSeq[E, D, V]


  protected[ordset] type ControlSegmentBase[E, D <: Domain[E], V] =
    TreapSegmentBase[E, D, ControlValue[E, D, V]] | UniformSegment[E, D, ControlValue[E, D, V]]

  protected[ordset] type ControlSegment[E, D <: Domain[E], V] = 
    SegmentT[E, D, ControlValue[E, D, V], ControlSegmentBase[E, D, V]]

  protected[ordset] type ControlSegmentWithNext[E, D <: Domain[E], V] = 
    SegmentT.WithNext[E, D, ControlValue[E, D, V], ControlSegmentBase[E, D, V]]

  protected[ordset] type ControlSegmentWithPrev[E, D <: Domain[E], V] = 
    SegmentT.WithPrev[E, D, ControlValue[E, D, V], ControlSegmentBase[E, D, V]]

  protected[ordset] type ControlTruncation[E, D <: Domain[E], V] =
    SegmentTruncationT[E, D, ControlValue[E, D, V], ControlSegmentBase[E, D, V], ControlSegment[E, D, V]]

  protected[ordset] type ControlSegmentSeq[E, D <: Domain[E], V] = TreapSegmentSeq[E, D, ControlValue[E, D, V]]


  protected[ordset] type ZSegmentBase[E, D <: Domain[E], V] =
    ZippedSegmentBase[
      E,
      D,
      V,
      ControlValue[E, D, V],
      ZValue[E, D, V],
      BaseSegmentBase[E, D, V],
      ControlSegmentBase[E, D, V]
    ]

  protected[ordset] type ZSegment[E, D <: Domain[E], V] = 
    SegmentT[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentWithNext[E, D <: Domain[E], V] = 
    SegmentT.WithNext[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentWithPrev[E, D <: Domain[E], V] = 
    SegmentT.WithPrev[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentInitial[E, D <: Domain[E], V] = 
    SegmentT.Initial[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentTerminal[E, D <: Domain[E], V] = 
    SegmentT.Terminal[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentInner[E, D <: Domain[E], V] = 
    SegmentT.Inner[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZSegmentSingle[E, D <: Domain[E], V] = 
    SegmentT.Single[E, D, ZValue[E, D, V], ZSegmentBase[E, D, V]]

  protected[ordset] type ZTruncation[E, D <: Domain[E], V] =
    SegmentTruncationT[
      E,
      D,
      ZValue[E, D, V],
      ZSegmentBase[E, D, V],
      ZSegment[E, D, V]
    ]

  protected[ordset] type ZTruncationWithPrev[E, D <: Domain[E], V] =
    SegmentTruncationT[
      E,
      D,
      ZValue[E, D, V],
      ZSegmentBase[E, D, V],
      ZSegmentWithPrev[E, D, V]
    ]

  protected[ordset] type ZTruncationWithNext[E, D <: Domain[E], V] =
    SegmentTruncationT[
      E,
      D,
      ZValue[E, D, V],
      ZSegmentBase[E, D, V],
      ZSegmentWithNext[E, D, V]
    ]

  protected[ordset] type ZSegmentSeq[E, D <: Domain[E], V] =
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
