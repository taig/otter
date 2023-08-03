package io.taig.otter.schema

import cats.syntax.all.*

object StringEncoder:
  val value: Encoder[Schema.Value, Option[String]] = new Encoder[Schema.Value, Option[String]]:
    override def encode[B](schema: Schema.Value[B], b: B): Option[String] = schema match
      case schema: Primitive[?]   => primitive.encode(schema, b)
      case schema: Enumeration[?] => enumeration.encode(schema, b)

  val primitive: Encoder[Primitive, Option[String]] = new Encoder[Primitive, Option[String]]:
    def encode[B](b: B): Type[B] => String =
      case Type.BigDecimal => b.bigDecimal.toPlainString
      case Type.BigInt     => b.toString
      case Type.Boolean    => String.valueOf(b)
      case Type.Double     => String.valueOf(b)
      case Type.Float      => String.valueOf(b)
      case Type.Int        => String.valueOf(b)
      case Type.Long       => String.valueOf(b)
      case Type.String     => b

    override def encode[B](schema: Primitive[B], b: B): Option[String] = schema match
      case Schema.Primitive.Root(_, tpe)         => encode(b)(tpe).some
      case Schema.Primitive.Validate(self, _, g) => encode(self, g(b))
      case Schema.Primitive.Optional(self)       => b.flatMap(encode(self, _))

  val enumeration: Encoder[Enumeration, Option[String]] = new Encoder[Enumeration, Option[String]]:
    override def encode[B](schema: Enumeration[B], b: B): Option[String] = schema match
      case Schema.Enumeration.Root(mapping, schema, _) => value.encode(schema.value, mapping.inj(b))
      case Schema.Enumeration.Validate(self, _, g)     => encode(self, g(b))
      case Schema.Enumeration.Optional(self)           => b.flatMap(encode(self, _))
