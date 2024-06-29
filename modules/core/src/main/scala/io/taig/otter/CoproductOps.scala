package io.taig.otter

import cats.Invariant
import cats.syntax.all.*

trait CoproductOps[Self[_, _], Value[_, _]]:
  given selfInvariant[A]: Invariant[Self[A, *]]

  extension [A, B](self: Self[A, B])
    infix def orElse[C, D](other: Self[C, D]): Self[A | C, Either[B, D]]
    infix def or[C, D](other: Value[C, D]): Self[A | other.type, Either[B, D]]
    final def :+[C, D](other: Value[C, D]): Self[A | other.type, Either[B, D]] = or(other)

  extension [A, B <: Matchable](self: Self[A, B])
    final inline def ||[C, D <: Matchable](value: Self[C, D]): Self[A | C, B | D] = self
      .orElse(value)
      .imap {
        case Left(b)  => b
        case Right(d) => d
      } {
        case b: B => Left(b)
        case d: D => Right(d)
      }
