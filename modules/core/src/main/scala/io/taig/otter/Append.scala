package io.taig.otter

import scala.Tuple as STuple
import cats.InvariantSemigroupal
import cats.syntax.all.*

type Append[A, B] = A match
  case STuple =>
    B match
      case Unit => A
      case _    => STuple.Append[A, B]
  case Unit => B
  case _    =>
    B match
      case Unit => A
      case _    => A *: B *: EmptyTuple

object Append:
  inline def apply[F[_] <: Matchable: InvariantSemigroupal, G[a] <: F[a], A, B](fa: F[A], gb: G[B]): F[Append[A, B]] =
    inline fa match
      case fxy: F[x *: y] =>
        inline gb match
          case gb: F[Unit] =>
            fxy.product(gb).imap[x *: y]((xy, _) => xy)(xy => (xy, ())).asInstanceOf[F[Append[A, B]]]
          case _ =>
            fxy
              .product(gb)
              .imap[STuple.Append[x *: y, B]](_ :* _)(xyb => (xyb.init, xyb.last).asInstanceOf[(x *: y, B)])
              .asInstanceOf[F[Append[A, B]]]
      case fa: F[Unit] =>
        fa.product(gb).imap[B]((_, b) => b)(b => ((), b)).asInstanceOf[F[Append[A, B]]]
      case _ =>
        inline gb match
          case gb: F[Unit] =>
            fa.product(gb).imap[A]((a, _) => a)(a => (a, ())).asInstanceOf[F[Append[A, B]]]
          case _ =>
            fa.product(gb)
              .imap[A *: B *: EmptyTuple]((a, b) => a *: b *: EmptyTuple) { case a *: b *: EmptyTuple => (a, b) }
              .asInstanceOf[F[Append[A, B]]]
