package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.schema.SumSchema

import scala.annotation.targetName

trait SumComponent[Self[_]: Invariant, -Branch[_]](self: SumSchema[Self, Branch]):
  extension [A](self: Self[A])
    @targetName("sumOrElse")
    final def orElse[B](schema: Self[B]): Self[Either[A, B]] = this.self.orElse(self)(schema)

    @targetName("sumAppend")
    final def :+[B](branch: Branch[B]): Self[Either[A, B]] = orElse(this.self.lift(branch))

  extension [A](branch: Branch[A])
    @targetName("sumPrepend")
    def +:[B](schema: Self[B]): Self[Either[A, B]] = this.self.lift(branch).orElse(schema)

  extension [A <: Matchable](self: Self[A])
    final inline def or[B <: Matchable](schema: Self[B]): Self[A | B] =
      self
        .orElse(schema)
        .imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

    final inline def |[B <: Matchable](branch: Branch[B]): Self[A | B] =
      or(this.self.lift(branch))
