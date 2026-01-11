package io.taig.otter

import cats.InvariantSemigroupal
import cats.syntax.all.*

type Prepend[A, B] = B match
  case _ *: _ =>
    A match
      case Unit => B
      case _    => A *: B
  case Unit => A
  case _    =>
    A match
      case Unit => B
      case _    => A *: B *: EmptyTuple

object Prepend:
  inline def apply[F[_] <: Matchable: InvariantSemigroupal, G[a] <: F[a], A, B](fa: F[A], gb: G[B]): F[Prepend[A, B]] =
    inline gb match
      case gxy: G[x *: y] =>
        inline fa match
          case fa: F[Unit] =>
            fa.product(gxy)
              .imap[x *: y]((_, xy) => xy)(xy => ((), xy))
              .asInstanceOf[F[Prepend[A, B]]]
          case _ =>
            fa
              .product(gxy)
              .imap[A *: x *: y](_ *: _) { case a *: xy => (a, xy) }
              .asInstanceOf[F[Prepend[A, B]]]
      case fb: F[Unit] =>
        fa.product(fb)
          .imap[A]((a, _) => a)(a => (a, ()))
          .asInstanceOf[F[Prepend[A, B]]]
      case _ =>
        inline fa match
          case fa: F[Unit] =>
            fa.product(gb)
              .imap[B]((_, b) => b)(b => ((), b))
              .asInstanceOf[F[Prepend[A, B]]]
          case _ =>
            fa.product(gb).asInstanceOf[F[Prepend[A, B]]]
