package io.taig.otter.schema

final class CoproductOps[A](self: Coproduct[A]) extends AnyVal:
  inline def :+[B](branch: Branch[B]): Coproduct[A + B] = ???
  inline def +:[B](branch: Branch[B]): Coproduct[A + B] = ???

final class CoproductOpsMatchable[A <: Matchable](self: Coproduct[A]) extends AnyVal:
  inline def |[B <: Matchable](branch: Branch[B]): Coproduct[A | B] = (self :+ branch).imap[A | B] {
    case Left(a)  => a
    case Right(b) => b
  } {
    case a: A => Left(a)
    case b: B => Right(b)
  }

trait ToCoproductOps:
  implicit def toCoproductOps[A](self: Coproduct[A]): CoproductOps[A] = CoproductOps(self)
  implicit def toCoproductOpsMatchable[A <: Matchable](self: Coproduct[A]): CoproductOpsMatchable[A] =
    CoproductOpsMatchable(self)
