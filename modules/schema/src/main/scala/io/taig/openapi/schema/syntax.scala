package io.taig.openapi.schema

import cats.syntax.all.*

object syntax:
  extension [A, B](self: Sum[A, B])
    def +[C](branch: Branch[A, C]): Sum[A, Either[B, C]] = self :+ branch

    def setDiscriminator(discriminator: Option[Discriminator]): Sum[A, B] =
      self.modifyDiscriminator(_ => discriminator)
    def withDiscriminator(discriminator: Discriminator): Sum[A, B] = setDiscriminator(discriminator.some)
    def withoutDiscriminator: Sum[A, B] = setDiscriminator(none)

  extension [A, B <: Matchable](self: Sum[A, B])
    inline infix def or[C <: Matchable](sum: Sum[A, C]): Sum[A, B | C] = self
      .orElse(sum)
      .imap {
        case Left(b)  => b
        case Right(c) => c
      } {
        case b: B => Left(b)
        case c: C => Right(c)
      }

    inline def |[C <: Matchable](branch: Branch[A, C]): Sum[A, B | C] = or(branch.toSum)

  extension [A, B](self: Branch[A, B]) def +[C](branch: Branch[A, C]): Sum[A, Either[B, C]] = self :+ branch

  extension [A, B <: Matchable](self: Branch[A, B])
    inline def |[C <: Matchable](branch: Branch[A, C]): Sum[A, B | C] = self.toSum | branch
