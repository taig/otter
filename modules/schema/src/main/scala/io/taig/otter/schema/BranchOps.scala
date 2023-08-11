package io.taig.otter.schema

final class BranchOps[A](self: Branch[A]) extends AnyVal:
  inline def :+[B](branch: Branch[B]): Coproduct[A + B] = ???
  inline def +:[B](branch: Branch[B]): Coproduct[A + B] = ???

final class BranchOpsMatchable[A <: Matchable](self: Branch[A]) extends AnyVal:
  inline def |[B <: Matchable](branch: Branch[B]): Coproduct[A | B] = (self :+ branch).imap[A | B] {
    case Left(a)  => a
    case Right(b) => b
  } {
    case a: A => Left(a)
    case b: B => Right(b)
  }

trait ToBranchOps:
  implicit def toBranchOps[A](self: Branch[A]): BranchOps[A] = BranchOps(self)
  implicit def toBranchOpsMatchable[A <: Matchable](self: Branch[A]): BranchOpsMatchable[A] = BranchOpsMatchable(self)
