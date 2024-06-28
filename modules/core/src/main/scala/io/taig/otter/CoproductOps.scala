package io.taig.otter

trait CoproductOps[Self[_, _], Value[_, _]]:
  extension [A, B](self: Self[A, B])
    def orElse[C, D](other: Self[C, D]): Self[A | C, Either[B, D]]
    def or[C, D](other: Value[C, D]): Self[A | other.type, Either[B, D]]

  extension [A, B <: Matchable](self: Self[A, B])
    inline def ||[C, D <: Matchable](value: Self[C, D]): Self[A | C, B | D]
    inline def |[C, D <: Matchable](value: Value[C, D]): Self[A | value.type, B | D]

object CoproductOps:
  trait Isomorphic[Self[_, _], Value[_, _]] extends CoproductOps[Self, Value], IsomorphicOps[Self]:
    extension [A, B <: Matchable](self: Self[A, B])
      final override inline def ||[C, D <: Matchable](value: Self[C, D]): Self[A | C, B | D] = self
        .orElse(value)
        .imap {
          case Left(b)  => b
          case Right(d) => d
        } {
          case b: B => Left(b)
          case d: D => Right(d)
        }

      override inline def |[C, D <: Matchable](value: Value[C, D]): Self[A | value.type, B | D] = self
        .or(value)
        .imap {
          case Left(b)  => b
          case Right(d) => d
        } {
          case b: B => Left(b)
          case d: D => Right(d)
        }

  trait Reader[Self[_, _], Value[_, _]] extends CoproductOps[Self, Value], ReaderOps[Self]:
    extension [A, B <: Matchable](self: Self[A, B])
      final override inline def ||[C, D <: Matchable](value: Self[C, D]): Self[A | C, B | D] = self
        .orElse(value)
        .map:
          case Left(b)  => b
          case Right(d) => d

      final override inline def |[C, D <: Matchable](value: Value[C, D]): Self[A | value.type, B | D] = self
        .or(value)
        .map:
          case Left(b)  => b
          case Right(d) => d

  trait Writer[Self[_, _], Value[_, _]] extends CoproductOps[Self, Value], WriterOps[Self]:
    extension [A, B <: Matchable](self: Self[A, B])
      final override inline def ||[C, D <: Matchable](value: Self[C, D]): Self[A | C, B | D] = self
        .orElse(value)
        .contramap:
          case b: B => Left(b)
          case d: D => Right(d)

      override inline def |[C, D <: Matchable](value: Value[C, D]): Self[A | value.type, B | D] = self
        .or(value)
        .contramap:
          case b: B => Left(b)
          case d: D => Right(d)
