package io.taig.otter

trait OrElseOps[Self[_, _]]:
  extension [A, B](self: Self[A, B]) infix def orElse[C, D](other: Self[C, D]): Self[A | C, Either[B, D]]

  extension [A, B <: Matchable](self: Self[A, B])
    inline def ||[C, D <: Matchable](value: Self[C, D]): Self[A | C, B | D]

object OrElseOps:
  trait Isomorphic[Self[_, _]] extends OrElseOps[Self], IsomorphicOps[Self]:
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

  trait Reader[Self[_, _]] extends OrElseOps[Self], ReaderOps[Self]:
    extension [A, B <: Matchable](self: Self[A, B])
      final override inline def ||[C, D <: Matchable](value: Self[C, D]): Self[A | C, B | D] = self
        .orElse(value)
        .map:
          case Left(b)  => b
          case Right(d) => d

  trait Writer[Self[_, _]] extends OrElseOps[Self], WriterOps[Self]:
    extension [A, B <: Matchable](self: Self[A, B])
      final override inline def ||[C, D <: Matchable](value: Self[C, D]): Self[A | C, B | D] = self
        .orElse(value)
        .contramap:
          case b: B => Left(b)
          case d: D => Right(d)
