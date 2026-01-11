package io.taig.otter

import scala.Tuple as STuple
import cats.InvariantSemigroupal
import cats.syntax.all.*
import scala.annotation.nowarn

type Append[A, B] = A match
  case _ *: _ =>
    B match
      case Unit => A
      case _    => STuple.Append[A, B]
  case Unit => B
  case _    =>
    B match
      case Unit => A
      case _    => A *: B *: EmptyTuple

object Append:
  @nowarn("msg=Matchable")
  inline def apply[F[_]: InvariantSemigroupal, A, B](fa: F[A], fb: F[B]): F[Append[A, B]] =
    inline fa match
      case fxy: F[x *: y] =>
        inline fb match
          case fb: F[Unit] =>
            fxy.product(fb).imap[x *: y]((xy, _) => xy)(xy => (xy, ())).asInstanceOf[F[Append[A, B]]]
          case _ =>
            fxy
              .product(fb)
              .imap[STuple.Append[x *: y, B]](_ :* _)(xyb => (xyb.init, xyb.last).asInstanceOf[(x *: y, B)])
              .asInstanceOf[F[Append[A, B]]]
      case fa: F[Unit] =>
        fa.product(fb).imap[B]((_, b) => b)(b => ((), b)).asInstanceOf[F[Append[A, B]]]
      case _ =>
        inline fb match
          case fb: F[Unit] =>
            fa.product(fb).imap[A]((a, _) => a)(a => (a, ())).asInstanceOf[F[Append[A, B]]]
          case _ =>
            fa.product(fb)
              .imap[A *: B *: EmptyTuple]((a, b) => a *: b *: EmptyTuple) { case a *: b *: EmptyTuple => (a, b) }
              .asInstanceOf[F[Append[A, B]]]
