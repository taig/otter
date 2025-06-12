package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Typescript
import cats.Applicative
import cats.Order

final class JsonTypescriptRenderer[S[_]: Applicative, A: Order](renderer: Renderer[Json, S[A]])
    extends Renderer[Json, S[Typescript[A]]]:
  val key = ReferenceConstantRenderer(encoder = KeyPrinter.Quoted)
  val constant = ConstantTypescriptRenderer(printer = JsonPrimitivePrinter)
  val enumeration = EnumerationTypescriptRenderer[Json.Primitive](printer = JsonPrimitivePrinter)
  val union = UnionTypescriptRenderer[Json, S, A](renderer)

  override def render[B](schema: Json[B]): S[Typescript[A]] = schema match
    case Json.Collection(self) =>
      renderer.render(self.value.schema.value).map(Typescript.Array.apply)
    case Json.Constant(self)   => constant.render(schema = self).pure
    case Json.Dictionary(self) =>
      // (
      //   KeyTypescriptRenderer.render(schema = self.value.key.value).self.pure[S],
      //   renderer.render(schema = self.value.value.value)
      // ).mapN(Typescript.Record.apply)
      ???
    case Json.Enumeration(self) => ??? // enumeration.render(schema = self).pure
    case Json.Nullable(self) =>
      self.value.schema.fold(Typescript.Void.pure): schema =>
        renderer.render(schema = schema.value).map(Typescript.Nullable.apply)
    case Json.Primitive(self) => PrimitiveTypescriptRenderer.render(schema = self).pure
    case Json.Record(self) =>
      self.value.fields
        .map(_.value)
        .traverse(field => renderer.render(field.value.value).tupleLeft(key.render(field.key)))
        .map(Typescript.Object.apply)
    case Json.Tuple(self) =>
      self.value.schemas.traverse(schema => renderer.render(schema.value)).map(Typescript.Tuple.apply)
    case Json.Union(self) => union.render(schema = self)
