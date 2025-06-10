package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.TypescriptZod
import io.taig.otter.TypescriptZodState
import io.taig.otter.Typescript

// TODO reference handling / catching
object JsonTypescriptZodRenderer extends Renderer[Json, TypescriptZodState[Typescript[TypescriptZod]]]:
  val renderer: Renderer[Json, TypescriptZodState[Typescript[TypescriptZod]]] = TypescriptRenderer[
    Json,
    Json.Primitive,
    Json.Field,
    TypescriptZodState,
    TypescriptZod
  ](
    renderer =
      ???, // ReferenceTypescriptRenderer(MetadataTypescriptZodRenderer(???)).map(_.map(TypescriptZod.Shared.apply)),
    printer = JsonPrimitivePrinter,
    key = JsonKeyTypescriptZodRenderer,
    field = JsonFieldRenderer(this).map(_.map(_.map(TypescriptZod.Shared.apply)))
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
  )

  override def render[A](schema: Json[A]): TypescriptZodState[Typescript[TypescriptZod]] =
    renderer.render(schema)
