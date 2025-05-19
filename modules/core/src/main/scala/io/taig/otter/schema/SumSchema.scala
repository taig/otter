package io.taig.otter.schema

import cats.syntax.all.*
import io.taig.otter.Discriminator
import io.taig.otter.Metadata

import scala.annotation.targetName

trait SumSchema[Self[_], -Branch[_]] extends Schema[Self]:
  self =>

  def lift[A](branch: => Branch[A]): Self[A]

  extension [A](self: Self[A])
    def orElse[B](schema: Self[B]): Self[Either[A, B]]

    @targetName("sum :+ branch")
    final def :+[B](branch: Branch[B]): Self[Either[A, B]] = orElse(branch.toSum)

    @targetName("sum +: branch")
    final def +:[B](branch: Branch[B]): Self[Either[B, A]] = branch.toSum.orElse(self)

    def discriminator: Discriminator
    def modifyDiscriminator(f: Discriminator => Discriminator): Self[A]

    final def explicit: Self[A] = modifyDiscriminator(_ => Discriminator.Explicit.Default)
    final def keyed: Self[A] = modifyDiscriminator(_ => Discriminator.Keyed)
    final def merged: Self[A] = modifyDiscriminator(_ => Discriminator.Merged.Default)

  extension [A <: Matchable](self: Self[A])
    final inline def or[B <: Matchable](schema: Self[B]): Self[A | B] = self
      .orElse(schema)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

    @targetName("sum | branch")
    final inline def |[B <: Matchable](branch: Branch[B]): Self[A | B] = self.or(branch.toSum)

  extension [A](self: Branch[A])
    @targetName("branch :+ branch")
    final def :+[B](branch: Branch[B]): Self[Either[A, B]] = self.toSum :+ branch

    @targetName("branch +: branch")
    final def +:[B](branch: Branch[B]): Self[Either[B, A]] = branch.toSum.orElse(self.toSum)

    final def toSum: Self[A] = lift(self)

  extension [A <: Matchable](self: Branch[A])
    @targetName("branch | branch")
    final inline def |[B <: Matchable](branch: Branch[B]): Self[A | B] = self.toSum.or(branch.toSum)

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): SumSchema[T, Branch] =
    new SumSchema[T, Branch]:
      override def lift[A](branch: => Branch[A]): T[A] = fK(self.lift(branch))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A])
        override def discriminator: Discriminator = self.discriminator(gK(ta))
        override def modifyDiscriminator(f: Discriminator => Discriminator): T[A] =
          fK(self.modifyDiscriminator(gK(ta))(f))
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def orElse[B](schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))

object SumSchema:
  inline def apply[Self[_], Branch[_]](using self: SumSchema[Self, Branch]): SumSchema[Self, Branch] = self
