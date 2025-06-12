package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Effect
import cats.syntax.all.*
import cats.Applicative

final class JsonEffectRenderer[S[_]: Applicative, A](renderer: Renderer[Json, S[A]])
    extends Renderer[Json, S[Effect[A]]]:
  val key = ReferenceConstantRenderer(encoder = KeyPrinter.Quoted)

  override def render[B](schema: Json[B]): S[Effect[A]] = schema match
    case Json.Collection(self) =>
      renderer.render(self.value.schema.value).map(Effect.Collection.apply)
    case Json.Constant(self)    => ???
    case Json.Dictionary(self)  => ???
    case Json.Enumeration(self) => ???
    case Json.Nullable(self) =>
      self.value.schema.fold(Effect.Void.pure): schema =>
        renderer.render(schema = schema.value).map(Effect.Nullable.apply)
    case Json.Primitive(self) => PrimitiveEffectRenderer.render(schema = self).pure
    case Json.Record(self) =>
      self.value.fields
        .map(_.value)
        .traverse(field => renderer.render(field.value.value).tupleLeft(key.render(field.key)))
        .map(Effect.Object.apply)
    case Json.Tuple(self) =>
      self.value.schemas.traverse(schema => renderer.render(schema.value)).map(Effect.Tuple.apply)
    case Json.Union(self) =>
      self.value.schemas.traverse(schema => renderer.render(schema.value)).map(Effect.Union.apply)
