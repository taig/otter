package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.TypescriptZod
import io.taig.otter.TypescriptZodState
import io.taig.otter.Typescript

object JsonTypescriptZodRenderer extends Renderer[Json, TypescriptZodState[TypescriptZod]]:
  val value: Encoder[Json, Option[String]] = Encoder {
    [A] =>
      (schema: Json[A], a: A) =>
        schema match
          case schema: Json.Primitive[A] => JsonPrimitivePrinter.encode(schema, a).some
          case _                         => none
  }

  val field: Renderer[Json.Field, TypescriptZodState[(String, Typescript.Value)]] = ???
  // FieldTypescriptRenderer(key = KeyPrinter.Unquoted, value = this)
  // .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)

  val renderer = TypescriptRenderer[Json, Json.Primitive, Json.Field, TypescriptZodState](
    renderer = this, // TypescriptReferenceRenderer(renderer = this),
    printer = JsonPrimitivePrinter,
    value,
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
  ).map(_.map(TypescriptZod.apply))

  override def render[A](schema: Json[A]): TypescriptZodState[TypescriptZod] = renderer.render(schema)
