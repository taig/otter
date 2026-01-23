package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript
import cats.Applicative
import cats.syntax.all.*
import cats.data.NonEmptyList

final class JsonTypescriptTypeRenderer[F[_]: Applicative](renderer: Renderer[Json.Write, F[Typescript.Type]])
    extends Renderer[Json.Write, F[Typescript.Type]]:
  override def render[A](json: Json.Write[A]): F[Typescript.Type] = json match
    case json: Json.Constant.Write[A]   => json.encode(JsonPrimitiveTypescriptTypeLiteralEncoder).pure
    case json: Json.Collection.Write[A] =>
      renderer.render(json.schema.value).map(tpe => Typescript.Type.Symbol(name = "Array", parameters = List(tpe)))
    case json: Json.Dictionary.Write[A] =>
      renderer
        .render(json.schema.value)
        .map(List(Typescript.Type.Symbol(name = "string", parameters = Nil), _))
        .map(Typescript.Type.Symbol(name = "Record", _))
    case json: Json.Enumeration.Write[A] =>
      Typescript.Type
        .Union(types = json.encode(JsonPrimitiveTypescriptTypeLiteralEncoder).toNonEmptyList)
        .pure
    case json: Json.Optional.Write[A] =>
      renderer
        .render(json.schema.value)
        .map(NonEmptyList.of(_, Typescript.Type.Null))
        .map(Typescript.Type.Union.apply)
    case _: Json.Primitive.Boolean.Write[A]        => Typescript.Type.Symbol(name = "boolean", parameters = Nil).pure
    case _: Json.Primitive.Coerce.Boolean.Write[A] =>
      Typescript.Type
        .Union(
          types = NonEmptyList.of(
            Typescript.Type.Symbol(name = "boolean", parameters = Nil),
            Typescript.Type.Symbol(name = "string", parameters = Nil)
          )
        )
        .pure
    case _: Json.Primitive.Coerce.Number.Write[A] =>
      Typescript.Type
        .Union(
          types = NonEmptyList.of(
            Typescript.Type.Symbol(name = "number", parameters = Nil),
            Typescript.Type.Symbol(name = "string", parameters = Nil)
          )
        )
        .pure
    case _: Json.Primitive.Coerce.Text.Write[A] =>
      Typescript.Type
        .Union(
          types = NonEmptyList.of(
            Typescript.Type.Symbol(name = "boolean", parameters = Nil),
            Typescript.Type.Symbol(name = "number", parameters = Nil),
            Typescript.Type.Symbol(name = "string", parameters = Nil)
          )
        )
        .pure
    case _: Json.Primitive.Number.Write[A] => Typescript.Type.Symbol(name = "number", parameters = Nil).pure
    case _: Json.Primitive.Text.Write[A]   => Typescript.Type.Symbol(name = "string", parameters = Nil).pure
    case json: Json.Record.Write[A]        =>
      json.fields
        .map(_.value)
        .toList
        .traverse: field =>
          renderer
            .render(field.schema.value)
            .map(Typescript.Type.Field(name = field.name, _, optional = field.isOptional))
        .map(Typescript.Type.Object.apply)
    case json: Json.Tuple.Write[A] =>
      json.schemas.map(_.value).toList.traverse(renderer.render).map(Typescript.Type.Tuple.apply)
    case json: Json.Union.Write[A] =>
      json.branches
        .map(_.value.schema.value)
        .traverse(renderer.render)
        .map(types => Typescript.Type.Union(types = types.toNonEmptyList))
