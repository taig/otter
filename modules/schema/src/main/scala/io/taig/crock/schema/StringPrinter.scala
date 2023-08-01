package io.taig.crock.schema

import scala.annotation.tailrec

object StringPrinter:
  val value: Printer[Schema.Value, String] = new Printer[Schema.Value, String]:
    override def print[B](schema: Schema.Value[B], b: B): String = schema match
      case schema: Primitive[?]   => primitive.print(schema, b)
      case schema: Enumeration[?] => enumeration.print(schema, b)

  val primitive: Printer[Primitive, String] = new Printer[Primitive, String]:
    def print[B](b: B): Type[B] => String =
      case Type.BigDecimal => b.bigDecimal.toPlainString
      case Type.BigInt     => b.toString
      case Type.Boolean    => String.valueOf(b)
      case Type.Double     => String.valueOf(b)
      case Type.Float      => String.valueOf(b)
      case Type.Int        => String.valueOf(b)
      case Type.Long       => String.valueOf(b)
      case Type.String     => b

    @tailrec
    override def print[B](schema: Primitive[B], b: B): String = schema match
      case Primitive.Root(_, tpe)              => print(b)(tpe)
      case Primitive.Validate(primitive, _, g) => print(primitive, g(b))

  val enumeration: Printer[Enumeration, String] = new Printer[Enumeration, String]:
    @tailrec
    override def print[B](schema: Enumeration[B], b: B): String = schema match
      case Enumeration.Root(mapping, schema, _)    => value.print(schema.value, mapping.inj(b))
      case Enumeration.Validate(enumeration, _, g) => print(enumeration, g(b))
