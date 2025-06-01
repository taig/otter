package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Typescript
import io.taig.otter.TypescriptState

object JsonTypescriptRenderer extends Renderer[Json, TypescriptState[Typescript]]:
  val renderer = ReferenceTypescriptRenderer(renderer = Expression)

  val collection = CollectionTypescriptRenderer(renderer = this)
  val constant = ConstantTypescriptRenderer[Json, Option](
    printer = Encoder:
      [A] =>
        (schema: Json[A], a: A) =>
          schema match
            case schema: Json.Primitive[A] => JsonPrimitivePrinter.encode(schema, a).some
            case _                    => none
  ).map(_.getOrElse(Typescript.Any)).map[TypescriptState[Typescript]](State.pure)
  val dictionary = DictionaryTypescriptRenderer(key = KeyTypescriptRenderer.map(State.pure), value = this)
  val enumeration = EnumerationTypescriptRenderer(printer = JsonPrimitivePrinter)
  val nullable = NullableTypescriptRenderer(renderer = this)
  val record = RecordTypescriptRenderer(
    renderer = FieldTypescriptRenderer(key = KeyPrinter.Quoted, value = this)
      .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)
  )
  val tuple = TupleTypescriptRenderer(renderer = this)
  val union = UnionTypescriptRenderer(renderer = this)

  override def render[A](schema: Json[A]): TypescriptState[Typescript] = renderer.render(schema)

  object Expression extends Renderer[Json, TypescriptState[Typescript]]:
    override def render[A](schema: Json[A]): TypescriptState[Typescript] = schema match
      case Json.Collection(self)  => collection.render(schema = self.self)
      case Json.Constant(self)    => constant.render(schema = self.self)
      case Json.Dictionary(self)  => dictionary.render(schema = self.self)
      case Json.Enumeration(self) => State.pure(enumeration.render(schema = self.self))
      case Json.Nullable(self)    => nullable.render(schema = self.self)
      case Json.Primitive(self)   => State.pure(PrimitiveTypescriptRenderer.render(schema = self.self))
      case Json.Record(self)      => record.render(schema = self.self)
      case Json.Tuple(self)       => tuple.render(schema = self.self)
      case Json.Union(self)       => union.render(schema = self.self)
