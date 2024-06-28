package io.taig.otter

trait OrOps[Self[_, _], Value[_, _], Result[_, _]]:
  extension [A, B](self: Self[A, B])
    infix def or[C, D](other: Value[C, D]): Result[self.type | other.type, Either[B, D]]

  extension [A, B <: Matchable](self: Self[A, B])
    inline def |[C, D <: Matchable](value: Value[C, D]): Result[self.type | value.type, B | D]

object OrOps:
  trait Isomorphic[Self[_, _], Value[_, _], Result[_, _]] extends OrOps[Self, Value, Result], IsomorphicOps[Result]:
    extension [A, B <: Matchable](self: Self[A, B])
      override inline def |[C, D <: Matchable](value: Value[C, D]): Result[self.type | value.type, B | D] =
        self
          .or(value)
          .imap {
            case Left(b)  => b
            case Right(d) => d
          } {
            case b: B => Left(b)
            case d: D => Right(d)
          }

  trait Reader[Self[_, _], Value[_, _], Result[_, _]] extends OrOps[Self, Value, Result], ReaderOps[Result]:
    extension [A, B <: Matchable](self: Self[A, B])
      override inline def |[C, D <: Matchable](value: Value[C, D]): Result[self.type | value.type, B | D] = self
        .or(value)
        .map:
          case Left(b)  => b
          case Right(d) => d

  trait Writer[Self[_, _], Value[_, _], Result[_, _]] extends OrOps[Self, Value, Result], WriterOps[Result]:
    extension [A, B <: Matchable](self: Self[A, B])
      override inline def |[C, D <: Matchable](value: Value[C, D]): Result[self.type | value.type, B | D] = self
        .or(value)
        .contramap:
          case b: B => Left(b)
          case d: D => Right(d)
