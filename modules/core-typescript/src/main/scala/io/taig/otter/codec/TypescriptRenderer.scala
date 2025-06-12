package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.codec.TypescriptRenderer.Schema
import cats.Applicative
import cats.Order

final class TypescriptRenderer[S[_], T[_], U[_], V[_]: Applicative, A: Order](
    renderer: Renderer[S, V[A]],
    printer: Encoder[T, String],
    field: Renderer[U, V[(String, A)]]
)(lift: Typescript[A] => A)
    extends Renderer[Schema[S, T, U, *], V[Typescript[A]]]:
  override def render[B](schema: Schema[S, T, U, B]): V[Typescript[A]] = schema match
    case schema: Collection[S, B] =>
      renderer.render(schema.value.schema.value).map(Typescript.Array.apply)
    case schema: Constant[T, B] =>
      ReferenceConstantRenderer(encoder = printer)
        .render(reference = schema.value.schema)
        .pure[V]
        .map(Typescript.Literal.apply)
    case schema: Dictionary[S, S, B] =>
      (renderer.render(schema.value.key.value), renderer.render(schema.value.value.value))
        .mapN(Typescript.Record.apply)
    case schema: Enumeration[T, B] =>
      schema.value.constants
        .map(ReferenceConstantRenderer(encoder = printer).render)
        .distinct
        .map(Typescript.Literal.apply)
        .map(lift)
        .pure[V]
        .map(Typescript.Union.apply)
    case schema: Nullable[S, B] =>
      schema.value.schema.fold(Typescript.Void.pure[V]): schema =>
        renderer.render(schema.value).map(Typescript.Nullable.apply)
    case schema: Primitive[T, B] => PrimitiveTypescriptRenderer.render(schema).pure
    case schema: Record[U, B] =>
      schema.value.fields.traverse(schema => field.render(schema.value)).map(Typescript.Object.apply)
    case schema: Tuple[S, B] =>
      schema.value.schemas
        .traverse(schema => renderer.render(schema.value))
        .map(Typescript.Tuple.apply)
    case schema: Union[S, B] =>
      schema.value.schemas
        .traverse(schema => renderer.render(schema.value))
        .map(_.distinct)
        .map(Typescript.Union.apply)

object TypescriptRenderer:
  type Schema[S[_], T[_], U[_], A] = Collection[S, A] | Constant[T, A] | Dictionary[S, S, A] | Enumeration[T, A] |
    Nullable[S, A] | Primitive[T, A] | Record[U, A] | Tuple[S, A] | Union[S, A]
