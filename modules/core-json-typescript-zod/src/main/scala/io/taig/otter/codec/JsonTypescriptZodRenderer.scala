package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.TypescriptZod
import io.taig.otter.TypescriptZodState
import io.taig.otter.Key
import io.taig.otter.Typescript

object JsonTypescriptZodRenderer extends Renderer[Json, TypescriptZodState[TypescriptZod]]:
  val field = FieldTypescriptRenderer[
    Key,
    Json,
    TypescriptZodState,
    TypescriptZod
  ](key = KeyPrinter.Unquoted, value = this).mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)

  val renderer: Renderer[Json, TypescriptZodState[TypescriptZod]] = TypescriptRenderer[
    Json,
    Json.Primitive,
    Json.Field,
    TypescriptZodState,
    TypescriptZod
  ](
    renderer =
      TypescriptStateRenderer(renderer = JsonTypescriptMetadataZodRenderer(this))(lift = TypescriptZod.Shared.apply),
    printer = JsonPrimitivePrinter,
    key = JsonKeyTypescriptZodRenderer,
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
  ).map(_.map(TypescriptZod.Shared.apply))

  override def render[A](schema: Json[A]): TypescriptZodState[TypescriptZod] = renderer.render(schema)
