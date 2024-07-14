package io.taig.otter.json

import io.circe.JsonObject
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.*
import io.taig.otter.Keys.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

object SumJsonDecoder:
  def apply[A](schema: Sum[?, A], json: Option[JsonObject]): Decoder.Result[Json, A] =
    SumJsonDecoder(schema, schema.metadata(discriminator).getOrElse(Discriminator.Default), json).andThen(
      _.toValid(
        Violations.rootNec(
          Violation(Constraint.OneOf(schema.branches.map(_.name.asJson).toList), actual = "null".asJson)
        )
      )
    )

  def apply[A](
      schema: Sum[?, A],
      discriminator: Discriminator,
      json: Option[JsonObject]
  ): Decoder.Result[Json, Option[A]] = schema match
    case Sum.Combine(_, left, right) => combine(left, right, discriminator, json)
    case Sum.Optional(self) => json.fold(none.valid)(json => SumJsonDecoder(self, discriminator, json.some).map(_.some))
    case Sum.Root(_, branch) =>
      json
        .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = "null".asJson)))
        .andThen(BranchJsonDecoder(branch, discriminator, _))
    case Sum.Transform(self, f, _) => SumJsonDecoder(self, discriminator, json).map(_.map(f))

  def combine[A, B](
      left: Sum[?, A],
      right: Sum[?, B],
      discriminator: Discriminator,
      json: Option[JsonObject]
  ): Decoder.Result[Json, Option[Either[A, B]]] = SumJsonDecoder(left, discriminator, json).andThen:
    case Some(a) => a.asLeft.some.valid
    case None    => SumJsonDecoder(right, discriminator, json).map(_.map(_.asRight))
