package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Typescript
import io.taig.otter.TypescriptState
import io.taig.otter.Typescript.Value
import io.taig.otter.Json.Dictionary
import io.taig.otter.Json.Nullable
import io.taig.otter.Json.Primitive
import io.taig.otter.Json.Union
import io.taig.otter.Key

object JsonTypescriptRenderer extends Renderer[Json, TypescriptState[Typescript.Value, Typescript.Value]]:
  val value: Encoder[Json, Option[String]] = Encoder {
    [A] =>
      (schema: Json[A], a: A) =>
        schema match
          case schema: Json.Primitive[A] => JsonPrimitivePrinter.encode(schema, a).some
          case _                         => none
  }

  val field = FieldTypescriptRenderer[
    Key,
    Json,
    TypescriptState[Typescript.Value, *],
    Typescript.Value
  ](key = KeyPrinter.Unquoted, value = this).mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)

  val renderer: Renderer[Json, TypescriptState[Typescript.Value, Typescript.Value]] = TypescriptRenderer[
    Json,
    Json.Primitive,
    Json.Field,
    TypescriptState[Typescript.Value, *],
    Typescript.Value
  ](
    renderer = TypescriptStateRenderer(renderer = this),
    printer = JsonPrimitivePrinter,
    value,
    key = JsonKeyTypescriptRenderer,
    field
  ).mapK[Json](
    [A] =>
      (self: Json[A]) =>
        self match
          case Json.Collection(self)  => self
          case Json.Constant(self)    => self
          case Json.Dictionary(self)  => self
          case Json.Enumeration(self) => self
          case Json.Nullable(self)    => self
          case Json.Primitive(self)   => self
          case Json.Record(self)      => self
          case Json.Tuple(self)       => self
          case Json.Union(self)       => self
  ).map(_.map(Typescript.Value.apply))

  override def render[A](schema: Json[A]): TypescriptState[Typescript.Value, Typescript.Value] =
    renderer.render(schema)
