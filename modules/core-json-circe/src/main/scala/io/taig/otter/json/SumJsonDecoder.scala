package io.taig.otter.json

import io.circe.JsonObject
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

object SumJsonDecoder:
  def apply[A](schema: Sum.Reader.Via[Json, A], json: Option[JsonObject]): Decoder.Result[Json, A] =
    SumJsonDecoder(schema, Discriminator.Default, json).andThen(
      _.toValid(
        Violations.rootNec(
          Violation(Constraint.OneOf(schema.branches.map(_.name.asJson).toList), actual = "null".asJson)
        )
      )
    )

    // TODO discriminator
  def apply[A](
      schema: Sum.Reader.Via[Json, A],
      discriminator: Discriminator,
      json: Option[JsonObject]
  ): Decoder.Result[Json, Option[A]] = schema match
    case Sum.Combine(_, left, right)        => combine(left, right, discriminator, json)
    case Sum.Optional(self)                 => optional(self, discriminator, json)
    case Sum.Root(_, branch)                => root(branch, discriminator, json)
    case Sum.Transform(self, f, _)          => transform(self, discriminator, f, json)
    case Sum.Reader.Combine(_, left, right) => combine(left, right, discriminator, json)
    case Sum.Reader.Optional(self)          => optional(self, discriminator, json)
    case Sum.Reader.Root(_, branch)         => root(branch, discriminator, json)
    case Sum.Reader.Transform(self, f)      => transform(self, discriminator, f, json)

  def combine[A, B](
      left: Sum.Reader.Via[Json, A],
      right: Sum.Reader.Via[Json, B],
      discriminator: Discriminator,
      json: Option[JsonObject]
  ): Decoder.Result[Json, Option[Either[A, B]]] = SumJsonDecoder(left, discriminator, json).andThen:
    case Some(a) => a.asLeft.some.valid
    case None    => SumJsonDecoder(right, discriminator, json).map(_.map(_.asRight))

  def optional[A](
      self: Sum.Reader.Via[Json, A],
      discriminator: Discriminator,
      json: Option[JsonObject]
  ): Decoder.Result[Json, Option[Option[A]]] =
    json.fold(none.valid)(json => SumJsonDecoder(self, discriminator, json.some).map(_.some))

  def root[A](
      branch: Branch.Reader.Via[Json, A],
      discriminator: Discriminator,
      json: Option[JsonObject]
  ): Decoder.Result[Json, Option[A]] = json
    .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = "null".asJson)))
    .andThen(BranchJsonDecoder(branch, discriminator, _))

  def transform[A, B](
      self: Sum.Reader.Via[Json, A],
      discriminator: Discriminator,
      f: A => B,
      json: Option[JsonObject]
  ): Decoder.Result[Json, Option[B]] = SumJsonDecoder(self, discriminator, json).map(_.map(f))
