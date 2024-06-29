package io.taig.otter

import cats.Invariant
import cats.syntax.all.*

trait CoproductLiftOps[Self[_, _], Value[_, _], Result[_, _]]:
  given resultInvariant[A]: Invariant[Result[A, *]]

  extension [A, B](self: Self[A, B])
    infix def or[C, D](other: Value[C, D]): Result[self.type | other.type, Either[B, D]]
    final def :+[C, D](other: Value[C, D]): Result[self.type | other.type, Either[B, D]] = or(other)
    final def +:[C, D](other: Value[C, D]): Result[self.type | other.type, Either[B, D]] = or(other)

  extension [A, B <: Matchable](self: Self[A, B])
    final inline def |[C, D <: Matchable](value: Value[C, D]): Result[self.type | value.type, B | D] = self
      .or(value)
      .imap {
        case Left(b)  => b
        case Right(d) => d
      } {
        case b: B => Left(b)
        case d: D => Right(d)
      }
