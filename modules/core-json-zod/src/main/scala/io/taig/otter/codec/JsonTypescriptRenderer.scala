package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript
import io.taig.otter.TypescriptState
import io.taig.otter.Constraint.Type
import cats.data.State

object JsonTypescriptRenderer extends Renderer[Json, TypescriptState[Typescript]]:
  val renderer = ReferenceTypescriptRenderer(renderer = Expression)

  val collection = CollectionTypescriptRenderer(renderer = this)

  val record = RecordTypescriptRenderer(
    renderer = FieldTypescriptRenderer(key = KeyPrinter.Quoted, value = this)
      .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)
  )

  val union = UnionTypescriptRenderer(renderer = this)

  override def render[A](schema: Json[A]): TypescriptState[Typescript] =
    renderer.render(schema)

  object Expression extends Renderer[Json, TypescriptState[Typescript]]:
    override def render[A](schema: Json[A]): TypescriptState[Typescript] = schema match
      case Json.Collection(self)  => collection.render(schema = self.self)
      case Json.Constant(self)    => ???
      case Json.Dictionary(self)  => ???
      case Json.Enumeration(self) => ???
      case Json.Nullable(self)    => ???
      case Json.Primitive(self)   => State.pure(PrimitiveTypescriptRenderer.render(schema = self.self))
      case Json.Record(self)      => record.render(schema = self.self)
      case Json.Tuple(self)       => ???
      case Json.Union(self)       => union.render(schema = self.self)
