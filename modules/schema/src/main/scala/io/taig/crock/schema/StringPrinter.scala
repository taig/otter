package io.taig.crock.schema

import scala.annotation.tailrec

object StringPrinter extends Printer[Primitive, String]:
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
    case Primitive.Root(_, tpe)           => print(b)(tpe)
    case Primitive.Validate(schema, _, g) => print(schema, g(b))
