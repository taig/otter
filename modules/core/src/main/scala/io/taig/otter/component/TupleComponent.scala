package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[
    Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
    SelfRead[+_[a] <: BoundRead[a], a],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Schema[+_[a] <: Bound[a], a] <: Bound[a],
    SchemaRead[+_[a] <: BoundRead[a], a] <: BoundRead[a],
    SchemaWrite[+_[a] <: BoundWrite[a], a] <: BoundWrite[a],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[+_],
    BoundWrite[-_]
](using F: TupleOperation[Self, SelfRead, SelfWrite, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite]):
  def TNil: Self[Nothing, Unit] = F.empty
