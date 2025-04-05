package io.taig.otter

trait CoproductInvariant[Self[_]] extends CodecInvariant[Self]:
  extension [A](self: Self[A])
    def orElse[B](codec: Self[B]): Self[Either[A, B]]
    final def :+[B](codec: Self[B]): Self[Either[A, B]] = orElse(codec)
    final def +:[B](codec: Self[B]): Self[Either[B, A]] = codec.orElse(self)

  extension [A <: Matchable](self: Self[A])
    inline def |[B <: Matchable](codec: Self[B]): Self[A | B] = self
      .orElse(codec)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }
