package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Typescript
import cats.Id
import cats.arrow.FunctionK
import io.taig.otter.Key

object JsonTypescriptRenderer extends Renderer[Json, Typescript.Value]:
  val field: Renderer[Json.Field, (String, Typescript.Value)] =
    FieldRenderer[Key, Json, Id, Typescript.Value](printer = KeyPrinter.Unquoted, renderer = this)
      .mapK[Json.Field](FunctionK.liftFunction(_.self))

  val fromJson = [A] =>
    (json: Json[A]) =>
      json match
        case Json.Collection(self)  => self
        case Json.Constant(self)    => self
        case Json.Dictionary(self)  => self.leftMapK(FunctionK.liftFunction[Key, Json](_.translate[Json]))
        case Json.Enumeration(self) => self
        case Json.Nullable(self)    => self
        case Json.Primitive(self)   => self
        case Json.Record(self)      => self
        case Json.Tuple(self)       => self
        case Json.Union(self)       => self

  val renderer =  TypescriptRenderer[Json, Json.Primitive, Json.Field, Id, Typescript.Value](
      renderer = ReferenceTypescriptRenderer(this)(lift = Typescript.Value.apply),
      printer = JsonPrimitivePrinter.Quoted,
      field
    )(lift = Typescript.Value.apply).mapK[Json](FunctionK.lift(fromJson)).map(Typescript.Value.apply)

  override def render[B](schema: Json[B]): Typescript.Value = renderer.render(schema)
