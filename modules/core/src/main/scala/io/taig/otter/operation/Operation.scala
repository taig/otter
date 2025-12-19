package io.taig.otter.operation

trait Operation[
    Self[+s[a] <: Bound[a], _],
    SelfRead[+_[a] <: BoundRead[a], _],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
] extends Operation.Read[SelfRead, BoundRead],
      Operation.Write[SelfWrite, BoundWrite]:
  extension [F[a] <: Bound[a], A](self: Self[F, A])
    def asRead: SelfRead[F, A]

    def asWrite: SelfWrite[F, A]

object Operation:
  trait Read[Self[+_[a] <: Bound[a], _], Bound[_]]

  trait Write[Self[+_[a] <: Bound[a], _], Bound[_]]
