package io.taig.otter

trait CoproductOps[Self[_, _], Value[_, _], Result[_, _]]:
  // extension [A, B](self: Result[A, B]) def imap[C](f: B => C)(g: C => B): Result[A, C]

  extension [A, B](self: Self[A, B])
    def orElse[C, D](value: Self[C, D]): Result[A | C, Either[B, D]]
    def or[C, D](value: Value[C, D]): Result[A | value.type, Either[B, D]]

  // extension [A, B <: Matchable](self: Self[A, B])
  //   def |[C, D <: Matchable](value: Value[C, D]): Result[self.type | value.type, B | D]
  // self
  //   .orElse(value)
  //   .imap {
  //     case Left(b)  => b
  //     case Right(d) => d
  //   } {
  //     case b: B => Left(b)
  //     case d: D => Right(d)
  //   }
