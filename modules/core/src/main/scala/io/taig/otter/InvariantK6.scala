package io.taig.otter

trait InvariantK6[F[
    _[+s[a] <: bound[a], a] <: read[s, a] & write[s, a],
    read[+_[a] <: boundRead[a], _] <: Matchable,
    write[+_[a] <: boundWrite[a], _] <: Matchable,
    bound[a] <: boundRead[a] & boundWrite[a],
    boundRead[_],
    boundWrite[_]
]]:
  extension [
      Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
      SelfRead[+_[a] <: BoundRead[a], _] <: Matchable,
      SelfWrite[+_[a] <: BoundWrite[a], _] <: Matchable,
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[_],
      BoundWrite[_]
  ](fa: F[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite])
    def imapK[
        SelfK[+s[a] <: Bound[a], a] <: SelfReadK[s, a] & SelfWriteK[s, a],
        SelfReadK[+_[a] <: BoundRead[a], _] <: Matchable,
        SelfWriteK[+_[a] <: BoundWrite[a], _] <: Matchable
    ](
        fK: [S[a] <: Bound[a], A] => Self[S, A] => SelfK[S, A],
        gK: [S[a] <: Bound[a], A] => SelfK[S, A] => Self[S, A]
    )(
        fKR: [S[a] <: BoundRead[a], A] => SelfRead[S, A] => SelfReadK[S, A],
        gKR: [S[a] <: BoundRead[a], A] => SelfReadK[S, A] => SelfRead[S, A]
    )(
        fKW: [S[a] <: BoundWrite[a], A] => SelfWrite[S, A] => SelfWriteK[S, A],
        gKW: [S[a] <: BoundWrite[a], A] => SelfWriteK[S, A] => SelfWrite[S, A]
    ): F[SelfK, SelfReadK, SelfWriteK, Bound, BoundRead, BoundWrite]

object InvariantK6:
  inline def apply[F[
      _[+s[a] <: bound[a], a] <: read[s, a] & write[s, a],
      read[+_[a] <: boundRead[a], _],
      write[+_[a] <: boundWrite[a], _],
      bound[a] <: boundRead[a] & boundWrite[a],
      boundRead[_],
      boundWrite[_]
  ]](using self: InvariantK6[F]): InvariantK6[F] = self
