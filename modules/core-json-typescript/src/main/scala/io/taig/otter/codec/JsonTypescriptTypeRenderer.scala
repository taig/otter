package io.taig.otter.codec

import cats.Applicative
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Typescript

final class JsonTypescriptTypeRenderer[F[_]: Applicative](
    renderer: Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Type]]
) extends Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Type]]:
  override def render[A](json: Json.Read[A] | Json.Write[A]): F[Typescript.Type] = json match
    case json: Json.Constant.Read[?]     => json.encode(JsonPrimitiveTypescriptTypeLiteralEncoder).pure
    case json: Json.Constant.Write[?]    => json.encode(JsonPrimitiveTypescriptTypeLiteralEncoder).pure
    case json: Json.Collection.Read[?]   => collection(json.schema.value)
    case json: Json.Collection.Write[?]  => collection(json.schema.value)
    case json: Json.Dictionary.Read[?]   => dictionary(json.schema.value)
    case json: Json.Dictionary.Write[?]  => dictionary(json.schema.value)
    case json: Json.Enumeration.Read[?]  => enumeration(json.encode(JsonPrimitiveTypescriptTypeLiteralEncoder)).pure
    case json: Json.Enumeration.Write[?] => enumeration(json.encode(JsonPrimitiveTypescriptTypeLiteralEncoder)).pure
    case json: Json.Optional.Read[?]     => optional(json.schema.value)
    case json: Json.Optional.Write[?]    => optional(json.schema.value)
    case _: (Json.Primitive.Boolean.Read[?] | Json.Primitive.Boolean.Write[?]) =>
      Typescript.Type.Symbol(name = "boolean", parameters = Nil).pure
    case _: (Json.Primitive.Coerce.Boolean.Read[?] | Json.Primitive.Coerce.Boolean.Write[?]) =>
      Typescript.Type
        .Union(
          types = NonEmptyList.of(
            Typescript.Type.Symbol(name = "boolean", parameters = Nil),
            Typescript.Type.Symbol(name = "string", parameters = Nil)
          )
        )
        .pure
    case _: (Json.Primitive.Coerce.Number.Read[?] | Json.Primitive.Coerce.Number.Write[?]) =>
      Typescript.Type
        .Union(
          types = NonEmptyList.of(
            Typescript.Type.Symbol(name = "number", parameters = Nil),
            Typescript.Type.Symbol(name = "string", parameters = Nil)
          )
        )
        .pure
    case _: (Json.Primitive.Coerce.Text.Read[?] | Json.Primitive.Coerce.Text.Write[?]) =>
      Typescript.Type
        .Union(
          types = NonEmptyList.of(
            Typescript.Type.Symbol(name = "boolean", parameters = Nil),
            Typescript.Type.Symbol(name = "number", parameters = Nil),
            Typescript.Type.Symbol(name = "string", parameters = Nil)
          )
        )
        .pure
    case _: (Json.Primitive.Number.Read[?] | Json.Primitive.Number.Write[?]) =>
      Typescript.Type.Symbol(name = "number", parameters = Nil).pure
    case _: (Json.Primitive.Text.Read[?] | Json.Primitive.Text.Write[?]) =>
      Typescript.Type.Symbol(name = "string", parameters = Nil).pure
    case json: Json.Record.Read[?]  => record(json.fields.map(_.value))
    case json: Json.Record.Write[?] => record(json.fields.map(_.value))
    case json: Json.Tuple.Read[?]   => tuple(json.schemas.map(_.value))
    case json: Json.Tuple.Write[?]  => tuple(json.schemas.map(_.value))
    case json: Json.Union.Read[?]   => union(json.branches.map(_.value.schema.value))
    case json: Json.Union.Write[?]  => union(json.branches.map(_.value.schema.value))

  def collection[A](json: Json.Read[A] | Json.Write[A]): F[Typescript.Type] =
    renderer.render(json).map(_.pure[List]).map(Typescript.Type.Symbol(name = "ReadonlyArray", _))

  def dictionary[A](json: Json.Read[A] | Json.Write[A]): F[Typescript.Type] = renderer
    .render(json)
    .map(List(Typescript.Type.Symbol(name = "string", parameters = Nil), _))
    .map(Typescript.Type.Symbol(name = "Record", _))

  def enumeration(json: NonEmptyChain[Typescript.Type]): Typescript.Type =
    Typescript.Type.Union(types = json.toNonEmptyList)

  def optional[A](json: Json.Read[A] | Json.Write[A]): F[Typescript.Type] = renderer
    .render(json)
    .map(NonEmptyList.of(_, Typescript.Type.Null))
    .map(Typescript.Type.Union.apply)

  def record[A](json: Chain[Json.Field.Read[A] | Json.Field.Write[A]]): F[Typescript.Type] = json.toList
    .traverse:
      case field: Json.Field.Read[?] =>
        renderer
          .render(field.schema.value)
          .map(Typescript.Type.Field(name = field.name, _, optional = field.isOptional))
      case field: Json.Field.Write[?] =>
        renderer
          .render(field.schema.value)
          .map(Typescript.Type.Field(name = field.name, _, optional = field.isOptional))
    .map(Typescript.Type.Object.apply)

  def tuple[A](json: Chain[Json.Read[A] | Json.Write[A]]): F[Typescript.Type] =
    json.toList.traverse(renderer.render).map(Typescript.Type.Tuple.apply)

  def union[A](json: NonEmptyChain[Json.Read[A] | Json.Write[A]]): F[Typescript.Type] =
    json.toNonEmptyList.traverse(renderer.render).map(Typescript.Type.Union.apply)
