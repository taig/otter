//package io.taig.otter.schema
//
//import cats.data.Chain
//import cats.syntax.all.*
//import io.taig.otter.OpenApi
//
//object Encoder:
//  def apply[A](schema: Schema[A], a: A): OpenApi = Encoder.schema(a)(schema)
//
//  def schema[A](a: A): Schema[A] => OpenApi =
//    case schema: Value[A]    => Encoder.value(a)(schema).getOrElse(OpenApi.Null)
//    case schema: AnyValue[A] => Encoder.anyValue(a)(schema)
//
//  def value[A](a: A): Value[A] => Option[OpenApi] =
//    case schema: Primitive[A] => Encoder.primitive(a)(schema)
//
//  def primitive[A](a: A): Primitive[A] => Option[OpenApi.Primitive] =
//    case Schema.Primitive.Root(_, tpe)         => Encoder.tpe(a)(tpe).some
//    case Schema.Primitive.Validate(self, _, g) => primitive(g(a))(self)
//    case Schema.Primitive.Optional(self)       => a.flatMap(primitive(_)(self))
//
//  def collection[A](a: A): Collection[A] => Option[Vector[OpenApi]] =
//    case Schema.Collection.Root(schema, _)      => a.map(Encoder(schema.value, _)).toVector.some
//    case Schema.Collection.Validate(self, _, g) => collection(g(a))(self)
//    case Schema.Collection.Optional(self)       => a.flatMap(collection(_)(self))
//
//  def anyValue[A](a: A): AnyValue[A] => OpenApi =
//    case Schema.AnyValue.Root() => a
//
//  def tpe[A](a: A): Type[A] => OpenApi.Primitive =
//    case Type.BigDecimal => OpenApi.Decimal(a)
//    case Type.BigInt     => OpenApi.Integer(a)
//    case Type.Boolean    => OpenApi.Bool(a)
//    case Type.Double     => OpenApi.Decimal(a)
//    case Type.Float      => OpenApi.Decimal(a)
//    case Type.Int        => OpenApi.Integer(a)
//    case Type.Long       => OpenApi.Integer(a)
//    case Type.String     => OpenApi.Text(a)
