package io.taig.otter

// final case class Wrapper[
//     F[_],
//     D[_[_], _],
//     O[f[_], a] <: Optional[f, a],
//     S[f[_], d[_[_], _], a] <: Schema[f, d, ?, a],
//     A
// ](schema: F[D[O[S[F, D, *], *], A]])

sealed abstract class Wrapper[
  //     F[_],
//     D[_[_], _],
//     O[f[_], a] <: Optional[f, a],
//     S[f[_], d[_[_], _], a] <: Schema[f, d, ?, a],
//     A
]