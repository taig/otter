package io.taig.otter.operation

trait Operation[
    Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
    SelfRead[+_[a] <: BoundRead[a], _],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
] extends Operation.Read[SelfRead, BoundRead],
      Operation.Write[SelfWrite, BoundWrite]:
  extension [S[a] <: Bound[a], A](self: Self[S, A])
    final inline def asRead: SelfRead[S, A] = self

    final inline def asWrite: SelfWrite[S, A] = self

object Operation:
  trait Read[Self[+_[a] <: Bound[a], _], Bound[_]]

  trait Write[Self[+_[a] <: Bound[a], _], Bound[_]]
