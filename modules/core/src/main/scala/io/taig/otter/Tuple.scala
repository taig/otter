package io.taig.otter

import cats.data.Chain

trait Tuple[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def empty: F[G, Unit]

  def tuple[H[a] <: G[a], A](schema: Reference[H, A]): F[H, A]

  extension [H[a] <: G[a], A](fha: F[H, A])
    def schemas: Chain[Reference[H, ?]]

    def zip[I[a] <: G[a], B](schema: F[I, B]): F[[a] =>> H[a] | I[a], (A, B)]

  // def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
  //     gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  // ): Tuple[H, G] = ???

// object Tuple:
//   trait Read[F[+_[a] <: G[a], _], G[_]] extends Tuple[F, G]:
//     self =>

//     override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
//         gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
//     ): Tuple.Read[H, G] = ???

//   object Read:
//     inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Tuple.Read[F, G]): Tuple.Read[F, G] = self

//     given InvariantK2[Tuple.Read] with
//       extension [F[+_[a] <: G[a], _], G[_]](fa: Tuple.Read[F, G])
//         override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
//             gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
//         ): Tuple.Read[H, G] = fa.imapK(fK)(gK)

//   trait Write[F[+_[a] <: G[a], _], G[_]] extends Tuple[F, G]:
//     self =>

//     override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
//         gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
//     ): Tuple.Write[H, G] = ???

//   object Write:
//     inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Tuple.Write[F, G]): Tuple.Write[F, G] = self

//     given InvariantK2[Tuple.Write] with
//       extension [F[+_[a] <: G[a], _], G[_]](fa: Tuple.Write[F, G])
//         override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
//             gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
//         ): Tuple.Write[H, G] = fa.imapK(fK)(gK)

//   inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Tuple[F, G]): Tuple[F, G] = self

//   given InvariantK2[Tuple] with
//     extension [F[+_[a] <: G[a], _], G[_]](fa: Tuple[F, G])
//       override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
//           gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
//       ): Tuple[H, G] = fa.imapK(fK)(gK)
