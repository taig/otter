package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Effect
import cats.syntax.all.*
import cats.Applicative
import cats.Order
import io.taig.otter.Key

final class JsonEffectRenderer[S[_]: Applicative, A: Order](value: Renderer[Json, S[A]])
    extends Renderer[Json, S[Effect[A]]]:
  val reference = ReferenceConstantRenderer(encoder = KeyPrinter.Quoted)
  val union = UnionEffectRenderer[Json, S, A](renderer = value)

  override def render[B](schema: Json[B]): S[Effect[A]] = schema match
    case Json.Collection(self) =>
      value.render(self.value.schema.value).map(Effect.Array.apply)
    case Json.Constant(self) => ???
    case Json.Dictionary(self) =>
      (
        value.render(schema = self.value.key.value.translate[Json]),
        value.render(schema = self.value.value.value)
      ).mapN(Effect.Record.apply)
    case Json.Enumeration(self) => ???
    case Json.Nullable(self) =>
      self.value.schema.fold(Effect.Void.pure): schema =>
        value.render(schema = schema.value).map(Effect.Nullable.apply)
    case Json.Primitive(self) => PrimitiveEffectRenderer.render(schema = self).pure
    case Json.Record(self) =>
      self.value.fields
        .map(_.value)
        .traverse(field => value.render(field.value.value).tupleLeft(reference.render(field.key)))
        .map(Effect.Struct.apply)
    case Json.Tuple(self) =>
      self.value.schemas.traverse(schema => value.render(schema.value)).map(Effect.Tuple.apply)
    case Json.Union(self) => union.render(self)
