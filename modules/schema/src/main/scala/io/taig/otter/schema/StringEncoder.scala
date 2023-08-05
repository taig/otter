package io.taig.otter.schema

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.schema.Collection

object StringEncoder:
  val value: Encoder[Value, Option[String]] = new Encoder:
    override def encode[A](value: Value[A], a: A): Option[String] = value match
      case schema: Primitive[?]   => primitive.encode(schema, a)
      case schema: Enumeration[?] => enumeration.encode(schema, a)

  val primitive: Encoder[Primitive, Option[String]] = new Encoder:
    def encode[A](a: A): Type[A] => String =
      case Type.BigDecimal => a.bigDecimal.toPlainString
      case Type.BigInt     => a.toString
      case Type.Boolean    => String.valueOf(a)
      case Type.Double     => String.valueOf(a)
      case Type.Float      => String.valueOf(a)
      case Type.Int        => String.valueOf(a)
      case Type.Long       => String.valueOf(a)
      case Type.String     => a

    override def encode[A](primitive: Primitive[A], a: A): Option[String] = primitive match
      case Schema.Primitive.Root(_, tpe)         => encode(a)(tpe).some
      case Schema.Primitive.Validate(self, _, g) => encode(self, g(a))
      case Schema.Primitive.Optional(self)       => a.flatMap(encode(self, _))

  val enumeration: Encoder[Enumeration, Option[String]] = new Encoder:
    override def encode[A](enumeration: Enumeration[A], a: A): Option[String] = enumeration match
      case Schema.Enumeration.Root(mapping, schema, _) => value.encode(schema.value, mapping.inj(a))
      case Schema.Enumeration.Validate(self, _, g)     => encode(self, g(a))
      case Schema.Enumeration.Optional(self)           => a.flatMap(encode(self, _))

  val collection: Encoder[Collection.Of[Value, *], Option[Chain[String]]] = new Encoder:
    override def encode[A](collection: Collection.Of[Value, A], a: A): Option[Chain[String]] = collection match
      case Schema.Collection.Root(schema, _)      => a.mapFilter(value.encode(schema.value, _)).some
      case Schema.Collection.Validate(self, _, g) => encode(self, g(a))
      case Schema.Collection.Optional(self)       => a.flatMap(encode(self, _)).filter(_.nonEmpty)
