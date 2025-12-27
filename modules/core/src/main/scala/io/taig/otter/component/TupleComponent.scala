package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[
    Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
    SelfRead[+_[a] <: BoundRead[a], a],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[+_],
    BoundWrite[-_]
](using F: TupleOperation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite]):
  def TNil: Self[Nothing, Unit] = F.empty
