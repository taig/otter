package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Json.Primitive
import io.taig.otter.ZodExpression
import io.taig.otter.ZodState

object JsonZodRenderer extends Renderer[Json, ZodState[ZodExpression]]:
  // val renderer = NamespaceZodRenderer(renderer = ZodRenderer(renderer = Expression))

  // val collection = CollectionZodRenderer(renderer = this)
  // val constant = ConstantZodRenderer[Json, Option](
  //   printer = Encoder:
  //     [A] =>
  //       (schema: Json[A], a: A) =>
  //         schema match
  //           case Json.Primitive(self) => PrimitivePrinter.Quoted.encode(schema = self.self, a).some
  //           case _                    => none
  // ).map(_.getOrElse("z.any()")).map[ZodState[String]](State.pure)
  // val dictionary = DictionaryZodRenderer(
  //   key = KeyZodRenderer.map((expression) => State.pure(ZodExpression.Inline(expression))),
  //   value = this
  // )
  // val enumeration = EnumerationZodRenderer(printer =
  //   PrimitivePrinter.Quoted.mapK[Json.Primitive]([A] => (json: Json.Primitive[A]) => json.self.self)
  // )
  // val nullable = NullableZodRenderer(renderer = this)
  // val record = RecordZodRenderer(
  //   renderer = FieldZodRenderer(key = KeyPrinter.Quoted, value = this)
  //     .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)
  // )
  // val tuple = TupleZodRenderer(renderer = this)
  // val union = UnionZodRenderer(renderer = this)

  override def render[A](schema: Json[A]): ZodState[ZodExpression] = ???
  // renderer.render(schema)

  // object Expression extends Renderer[Json, ZodState[String]]:
  //   override def render[A](schema: Json[A]): ZodState[String] = schema match
  //     case Json.Collection(self)  => collection.render(schema = self.self)
  //     case Json.Constant(self)    => constant.render(schema = self.self)
  //     case Json.Dictionary(self)  => dictionary.render(schema = self.self)
  //     case Json.Enumeration(self) => State.pure(enumeration.render(schema = self.self))
  //     case Json.Nullable(self)    => nullable.render(schema = self.self)
  //     case Json.Primitive(self)   => State.pure(PrimitiveZodRenderer.render(schema = self.self))
  //     case Json.Record(self)      => record.render(schema = self.self)
  //     case Json.Tuple(self)       => tuple.render(schema = self.self)
  //     case Json.Union(self)       => union.render(schema = self.self)
