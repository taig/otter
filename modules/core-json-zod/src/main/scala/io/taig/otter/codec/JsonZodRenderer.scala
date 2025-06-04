package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Zod
import io.taig.otter.ZodState

object JsonZodRenderer extends Renderer[Json, ZodState[Zod]]:
  val renderer = ZodRenderer(renderer = Expression)

  val collection = CollectionZodRenderer(renderer = this)
  val constant = ConstantZodRenderer[Json, Option](
    printer = Encoder:
      [A] =>
        (schema: Json[A], a: A) =>
          schema match
            case schema: Json.Primitive[A] => JsonPrimitivePrinter.encode(schema, a).some
            case _                         => none
  ).map(_.getOrElse(Zod.Expression("z.any()"))).map[ZodState[Zod]](State.pure)
  val dictionary = DictionaryZodRenderer(key = KeyZodRenderer.map(State.pure), value = this)
  val enumeration = EnumerationZodRenderer(printer = JsonPrimitivePrinter)
  val nullable = NullableZodRenderer(renderer = this)
  val record = RecordZodRenderer(
    renderer = FieldZodRenderer(key = KeyPrinter.Unquoted, value = this)
      .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)
  )
  val tuple = TupleZodRenderer(renderer = this)
  val union = UnionZodRenderer(renderer = this)

  override def render[A](schema: Json[A]): ZodState[Zod] = renderer.render(schema)

  object Expression extends Renderer[Json, ZodState[Zod]]:
    override def render[A](schema: Json[A]): ZodState[Zod] = schema match
      case Json.Collection(self)  => collection.render(schema = self.self)
      case Json.Constant(self)    => constant.render(schema = self.self)
      case Json.Dictionary(self)  => dictionary.render(schema = self.self)
      case Json.Enumeration(self) => State.pure(enumeration.render(schema = self.self))
      case Json.Nullable(self)    => nullable.render(schema = self.self)
      case Json.Primitive(self)   => State.pure(PrimitiveZodRenderer.render(schema = self.self))
      case Json.Record(self)      => record.render(schema = self.self)
      case Json.Tuple(self)       => tuple.render(schema = self.self)
      case Json.Union(self)       => union.render(schema = self.self)
