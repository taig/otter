package io.taig.crock.schema

import scala.annotation.tailrec

object StringEncoder:
  val value: Encoder[Schema.Value, String] = new Encoder[Schema.Value, String]:
    override def encode[B](schema: Schema.Value[B], b: B): String = schema match
      case schema: Primitive[?]   => primitive.encode(schema, b)
      case schema: Enumeration[?] => enumeration.encode(schema, b)

  val primitive: Encoder[Primitive, String] = new Encoder[Primitive, String]:
    def encode[B](b: B): Type[B] => String =
      case Type.BigDecimal => b.bigDecimal.toPlainString
      case Type.BigInt     => b.toString
      case Type.Boolean    => String.valueOf(b)
      case Type.Double     => String.valueOf(b)
      case Type.Float      => String.valueOf(b)
      case Type.Int        => String.valueOf(b)
      case Type.Long       => String.valueOf(b)
      case Type.String     => b

    @tailrec
    override def encode[B](schema: Primitive[B], b: B): String = schema match
      case Primitive.Root(_, tpe)              => encode(b)(tpe)
      case Primitive.Validate(primitive, _, g) => encode(primitive, g(b))

  val enumeration: Encoder[Enumeration, String] = new Encoder[Enumeration, String]:
    @tailrec
    override def encode[B](schema: Enumeration[B], b: B): String = schema match
      case Enumeration.Root(mapping, schema, _)    => value.encode(schema.value, mapping.inj(b))
      case Enumeration.Validate(enumeration, _, g) => encode(enumeration, g(b))
