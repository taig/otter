// package io.taig.otter.schema

// import cats.syntax.all.*
// import cats.data.Validated
// import io.taig.otter.OpenApi
// import io.taig.otter.syntax.*

// import scala.collection.immutable.VectorMap

// abstract class Field[A]:
//   self =>
//   def key: String
//   def schema: Schema[?]

//   def properties: Field.Properties[A]
//   final def copy(update: Field.Properties[A]): Field[A] = new Field[A]:
//     export self.{decodeWithRemainders, encodeWithNull, key, schema}
//     override def properties: Field.Properties[A] = update

//   final class Nulls:
//     def value: Option[Null] = properties.nulls
//     def modify(f: Option[Null] => Option[Null]): Field[A] = copy(properties.copy(nulls = f(properties.nulls)))
//     def apply(value: Option[Null]): Field[A] = modify(_ => value)
//     def inherit: Field[A] = apply(None)
//     def hide: Field[A] = apply(Some(Null.Hide))
//     def show: Field[A] = apply(Some(Null.Show))

//   def nulls: Nulls = new Nulls

//   def decodeWithRemainders(openapi: VectorMap[String, OpenApi]): Validated[Violations, (VectorMap[String, OpenApi], A)]
//   final def encode(b: A, nulls: Null): OpenApi.Object = encodeWithNull(b, properties.nulls.getOrElse(nulls))
//   protected def encodeWithNull(b: A, nulls: Null): OpenApi.Object

//   def toRecord: Record[A] = Record(this)
//   def to[B](using Evidence.Product.Aux[B, A]): Record[B] = toRecord.to[B]

// object Field extends ToFieldOps:
//   final case class Properties[+A](default: Option[A], nulls: Option[Null])

//   object Properties:
//     val Default: Field.Properties[Nothing] = Properties(None, None)

//   def apply[A, B](name: A, a: => Schema.Value[A], b: => Schema[B]): Field[B] = new Field[B]:
//     override val key: String = a.print(name).getOrElse("")
//     override def schema: Schema[B] = b
//     override def properties: Properties[B] = Properties.Default
//     override def decodeWithRemainders(
//         openapi: VectorMap[String, OpenApi]
//     ): Validated[Violations, (VectorMap[String, OpenApi], B)] =
//       schema.decode(openapi.get(key).flatMap(_.asValue)).tupleLeft(openapi.removed(key))
//     override def encodeWithNull(b: B, nulls: Null): OpenApi.Object = (schema.encode(b), nulls) match
//       case (Some(value), _)  => OpenApi.obj(key := value)
//       case (None, Null.Show) => OpenApi.obj(key := OpenApi.Null)
//       case (None, Null.Hide) => OpenApi.Object.Empty
