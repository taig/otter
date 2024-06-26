package io.taig.otter

trait CoproductOps[Self[_, _], Value[_, _], Result[_, _]]:
  extension [A, B](self: Result[A, B]) def imap[C](f: B => C)(g: C => B): Result[A, C]

  extension [A, B](self: Self[A, B]) def orElse[C, D](value: Value[C, D]): Result[self.type | value.type, Either[B, C]]

  extension [A, B <: Matchable](self: Self[A, B])
    final inline def |[C, D <: Matchable](value: Value[C, D]): Result[self.type | value.type, B | D] = ???
    // self
    //   .orElse(value)
    //   .imap {
    //     case Left(b)  => b
    //     case Right(c) => c
    //   } {
    //     case b: B => Left(b)
    //     case d: D => Right(d)
    //   }
