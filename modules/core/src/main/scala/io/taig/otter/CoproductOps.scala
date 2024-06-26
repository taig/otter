package io.taig.otter

import cats.Invariant
import cats.syntax.all.*

trait CoproductOps[Self[_, _], Parent[_], Result[_, _]]:
  given invariant[A]: Invariant[Result[A, *]]

  extension [A, B](self: Self[A, B])
    def orElse[C](schema: Parent[C]): Result[self.type | schema.type, Either[B, C]]

  extension [A, B <: Matchable](self: Self[A, B])
    final inline def |[C <: Matchable](schema: Parent[C]): Result[self.type | schema.type, B | C] = self
      .orElse(schema)
      .imap {
        case Left(b)  => b
        case Right(c) => c
      } {
        case b: B => Left(b)
        case c: C => Right(c)
      }