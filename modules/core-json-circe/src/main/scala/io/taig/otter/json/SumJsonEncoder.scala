package io.taig.otter.json

import io.taig.otter.*
import io.taig.otter.Keys.*
import io.circe.JsonObject
import cats.syntax.all.*
import io.circe.Json

object SumJsonEncoder:
  def apply[A](schema: Sum.Via[Json, A], a: A): Option[JsonObject] =
    SumJsonEncoder(schema, schema.metadata(discriminator).getOrElse(Discriminator.Default), a)

  def apply[A](schema: Sum.Via[Json, A], discriminator: Discriminator, a: A): Option[JsonObject] =
    schema match
      case Sum.Combine(_, left, right) =>
        a.fold(SumJsonEncoder(left, discriminator, _), SumJsonEncoder(right, discriminator, _))
      case Sum.Optional(self)        => a.flatMap(SumJsonEncoder(self, discriminator, _))
      case Sum.Root(_, branch)       => BranchJsonEncoder(branch, discriminator, a).some
      case Sum.Transform(self, _, f) => SumJsonEncoder(self, discriminator, f(a))
