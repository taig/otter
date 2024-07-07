package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.JsonObject
import cats.syntax.all.*

object SumJsonEncoder:
  def apply[A](schema: Sum.Writer[A], a: A): Option[JsonObject] = SumJsonEncoder(schema, schema.discriminator, a)

  def apply[A](schema: Sum.Writer[A], discriminator: Sum.Discriminator, a: A): Option[JsonObject] = schema match
    case Base.Sum.Combine(left, right)                       => combine(left, right, discriminator, a)
    case Base.Sum.Discriminators(self, discriminator)        => SumJsonEncoder(self, discriminator, a)
    case Base.Sum.Optional(self)                             => optional(self, discriminator, a)
    case Base.Sum.Root(branch)                               => BranchJsonEncoder(branch, discriminator, a).some
    case Base.Sum.Transform(self, _, f)                      => transform(self, discriminator, f, a)
    case Base.Sum.Writer.Combine(left, right)                => combine(left, right, discriminator, a)
    case Base.Sum.Writer.Discriminators(self, discriminator) => SumJsonEncoder(self, discriminator, a)
    case Base.Sum.Writer.Optional(self)                      => optional(self, discriminator, a)
    case Base.Sum.Writer.Root(branch)                        => BranchJsonEncoder(branch, discriminator, a).some
    case Base.Sum.Writer.Transform(self, f)                  => transform(self, discriminator, f, a)

  def combine[A, B](
      left: Sum.Writer[A],
      right: Sum.Writer[B],
      discriminator: Sum.Discriminator,
      ab: Either[A, B]
  ): Option[JsonObject] = ab.fold(SumJsonEncoder(left, discriminator, _), SumJsonEncoder(right, discriminator, _))

  def optional[A](self: Sum.Writer[A], discriminator: Sum.Discriminator, a: Option[A]): Option[JsonObject] =
    a.flatMap(SumJsonEncoder(self, discriminator, _))

  def transform[A, B](self: Sum.Writer[A], discriminator: Sum.Discriminator, f: B => A, b: B): Option[JsonObject] =
    SumJsonEncoder(self, discriminator, f(b))
