package io.taig.otter.codec

import cats.Applicative
import cats.Order
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.codec.EffectRenderer.Schema

final class EffectRenderer[S[_], T[_], U[_], V[_]: Applicative, A: Order](
    renderer: Renderer[S, V[A]],
    printer: Encoder[T, String],
    field: Renderer[U, V[(String, A)]]
)(lift: Effect[A] => A)
    extends Renderer[Schema[S, T, U, *], V[Effect[A]]]:
  override def render[B](schema: Schema[S, T, U, B]): V[Effect[A]] = schema match
    case schema: Collection[S, B] => renderer.render(schema.value.schema.value).map(Effect.Array.apply)
    case schema: Constant[T, B]   =>
      ReferenceConstantRenderer(encoder = printer)
        .render(reference = schema.value.schema)
        .pure[V]
        .map(Effect.Literal.apply)
    case schema: Dictionary[S, S, B] =>
      (renderer.render(schema.value.key.value), renderer.render(schema.value.value.value))
        .mapN(Effect.Record.apply)
    case schema: Enumeration[T, B] =>
      schema.value.constants
        .map(ReferenceConstantRenderer(encoder = printer).render)
        .distinct
        .map(Effect.Literal.apply)
        .map(lift)
        .pure[V]
        .map(Effect.Union.apply)
    case schema: Nullable[S, B] =>
      schema.value.schema.fold(Effect.Void.pure[V]): schema =>
        renderer.render(schema.value).map(Effect.Nullable.apply)
    case schema: Primitive[T, B] => PrimitiveEffectRenderer.render(schema).pure
    case schema: Record[U, B]    =>
      schema.value.fields.traverse(schema => field.render(schema.value)).map(Effect.Struct.apply)
    case schema: Tuple[S, B] =>
      schema.value.schemas
        .traverse(schema => renderer.render(schema.value))
        .map(Effect.Tuple.apply)
    case schema: Union[S, B] =>
      schema.value.schemas
        .traverse(schema => renderer.render(schema.value))
        .map(_.distinct)
        .map(Effect.Union.apply)

object EffectRenderer:
  type Schema[S[_], T[_], U[_], A] = Collection[S, A] | Constant[T, A] | Dictionary[S, S, A] | Enumeration[T, A] |
    Nullable[S, A] | Primitive[T, A] | Record[U, A] | Tuple[S, A] | Union[S, A]
