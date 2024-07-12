package io.taig.otter.json

import io.taig.otter.*
import io.circe.JsonObject
import cats.syntax.all.*
import io.circe.Json

object SumJsonEncoder:
  def apply[A](schema: Sum.Writer.Via[Json, A], a: A): Option[JsonObject] =
    SumJsonEncoder(schema, Discriminator.Default, a)

  def apply[A](schema: Sum.Writer.Via[Json, A], discriminator: Discriminator, a: A): Option[JsonObject] =
    schema match
      case Sum.Combine(_, left, right)        => combine(left, right, discriminator, a)
      case Sum.Optional(self)                 => optional(self, discriminator, a)
      case Sum.Root(_, branch)                => BranchJsonEncoder(branch, discriminator, a).some
      case Sum.Transform(self, _, f)          => transform(self, discriminator, f, a)
      case Sum.Writer.Combine(_, left, right) => combine(left, right, discriminator, a)
      case Sum.Writer.Optional(self)          => optional(self, discriminator, a)
      case Sum.Writer.Root(_, branch)         => BranchJsonEncoder(branch, discriminator, a).some
      case Sum.Writer.Transform(self, f)      => transform(self, discriminator, f, a)

  def combine[A, B](
      left: Sum.Writer.Via[Json, A],
      right: Sum.Writer.Via[Json, B],
      discriminator: Discriminator,
      ab: Either[A, B]
  ): Option[JsonObject] = ab.fold(SumJsonEncoder(left, discriminator, _), SumJsonEncoder(right, discriminator, _))

  def optional[A](self: Sum.Writer.Via[Json, A], discriminator: Discriminator, a: Option[A]): Option[JsonObject] =
    a.flatMap(SumJsonEncoder(self, discriminator, _))

  def transform[A, B](
      self: Sum.Writer.Via[Json, A],
      discriminator: Discriminator,
      f: B => A,
      b: B
  ): Option[JsonObject] = SumJsonEncoder(self, discriminator, f(b))
