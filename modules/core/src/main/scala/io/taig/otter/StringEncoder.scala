package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*

object StringEncoder:
  val value: Encoder[Schema.Value, Option[String]] = new Encoder:
    override def encode[B](schema: Schema.Value[B], b: B): Option[String] = schema match
      case schema: Schema.Enumeration[B] => ???
      case schema: Schema.Primitive[B]   => primitive.encode(schema, b)

  val collection: Encoder[Schema.Collection[Schema.Value, *], Option[Chain[String]]] = new Encoder:
    // Unsafe because mapFilter throws nulls away
    override def encode[B](schema: Schema.Collection[Schema.Value, B], b: B): Option[Chain[String]] = schema match
      case Schema.Collection.Root(schema, _, _)   => b.mapFilter(value.encode(schema, _)).some
      case Schema.Collection.Optional(self)       => b.flatMap(encode(self, _))
      case Schema.Collection.Validate(self, _, g) => encode(self, g(b))

  val primitive: Encoder[Schema.Primitive, Option[String]] = new Encoder:
    override def encode[B](schema: Schema.Primitive[B], b: B): Option[String] = schema match
      case Schema.Primitive.Root(tpe, _, _, _)   => encode(tpe, b).some
      case Schema.Primitive.Optional(self)       => b.flatMap(encode(self, _))
      case Schema.Primitive.Validate(self, _, g) => encode(self, g(b))

    def encode[A](tpe: Type[A], a: A): String = tpe match
      case Type.BigDecimal => a.toString
      case Type.BigInt     => a.toString
      case Type.Boolean    => String.valueOf(a)
      case Type.Double     => String.valueOf(a)
      case Type.Float      => String.valueOf(a)
      case Type.Int        => String.valueOf(a)
      case Type.Long       => String.valueOf(a)
      case Type.String     => String.valueOf(a)
