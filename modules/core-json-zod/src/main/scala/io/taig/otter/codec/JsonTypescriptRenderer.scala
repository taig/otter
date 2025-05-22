package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript
import io.taig.otter.TypescriptState
import cats.data.State
import cats.syntax.all.*

object JsonTypescriptRenderer extends Renderer[Json, TypescriptState[Typescript]]:
  val renderer = ReferenceTypescriptRenderer(renderer = Expression)

  val collection = CollectionTypescriptRenderer(renderer = this)
  val constant = ConstantTypescriptRenderer[Json, Option](
    printer = Encoder:
      [A] =>
        (schema: Json[A], a: A) =>
          schema match
            case Json.Primitive(self) => PrimitivePrinter.Quoted.encode(schema = self.self, a).some
            case _                    => none
  ).map(_.getOrElse(Typescript.Any)).map[TypescriptState[Typescript]](State.pure)

  val dictionary = DictionaryTypescriptRenderer(key = KeyTypescriptRenderer.map(State.pure), value = this)

  val record = RecordTypescriptRenderer(
    renderer = FieldTypescriptRenderer(key = KeyPrinter.Quoted, value = this)
      .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)
  )

  val union = UnionTypescriptRenderer(renderer = this)

  override def render[A](schema: Json[A]): TypescriptState[Typescript] = renderer.render(schema)

  object Expression extends Renderer[Json, TypescriptState[Typescript]]:
    override def render[A](schema: Json[A]): TypescriptState[Typescript] = schema match
      case Json.Collection(self)  => collection.render(schema = self.self)
      case Json.Constant(self)    => constant.render(schema = self.self)
      case Json.Dictionary(self)  => dictionary.render(schema = self.self)
      case Json.Enumeration(self) => ???
      case Json.Nullable(self)    => ???
      case Json.Primitive(self)   => State.pure(PrimitiveTypescriptRenderer.render(schema = self.self))
      case Json.Record(self)      => record.render(schema = self.self)
      case Json.Tuple(self)       => ???
      case Json.Union(self)       => union.render(schema = self.self)
