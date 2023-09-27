//package io.taig.otter
//
//import cats.data.Chain
//import cats.syntax.all.*
//
//object StringEncoder:
//  val value: Encoder[Schema.Of[Schema.Value[?], *], Option[String]] = new Encoder:
//    override def encode[A](schema: Schema.Value[A], a: A): Option[String] = schema match
//      case schema: Schema.Enumeration[A] => ???
//      case schema: Schema.Primitive[A]   => primitive.encode(schema, a)
//
//  val collection: Encoder[Schema.Collection[Schema.Value, *], Option[Chain[String]]] = new Encoder:
//    // Unsafe because mapFilter throws nulls away
//    override def encode[A](schema: Schema.Collection[Schema.Value, A], a: A): Option[Chain[String]] = schema match
//      case Schema.Collection.Root(schema, _)   => a.mapFilter(value.encode(schema, _)).some
//      case Schema.Collection.Optional(self)       => a.flatMap(encode(self, _))
//      case Schema.Collection.Validate(self, _, g) => encode(self, g(a))
//
//  val primitive: Encoder[Schema.Primitive, Option[String]] = new Encoder:
//    override def encode[A](schema: Schema.Primitive[A], a: A): Option[String] = schema match
//      case Schema.Primitive.Root(tpe, _, _)   => encode(tpe, a).some
//      case Schema.Primitive.Optional(self)       => a.flatMap(encode(self, _))
//      case Schema.Primitive.Validate(self, _, g) => encode(self, g(a))
//
//    def encode[A](tpe: Type[A], a: A): String = tpe match
//      case Type.BigDecimal => a.toString
//      case Type.BigInt     => a.toString
//      case Type.Boolean    => String.valueOf(a)
//      case Type.Double     => String.valueOf(a)
//      case Type.Float      => String.valueOf(a)
//      case Type.Int        => String.valueOf(a)
//      case Type.Long       => String.valueOf(a)
//      case Type.String     => String.valueOf(a)
