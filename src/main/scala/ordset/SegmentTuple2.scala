package ordset

trait SegmentTuple2[E, +V, +S1 <: Segment[E, V], +S2 <: Segment[E, V]] extends Product2[S1, S2]