package io.taig.otter.codec
import io.taig.otter.Primitive
import io.taig.otter.escape

final class PrimitivePrinter(quotes: Boolean) extends Encoder[Primitive.Value, String]:
  override def encode[A](schema: Primitive.Value[A], a: A): String = schema match
    case Primitive.Value.Boolean.Modify(self, _, g)  => encode(schema = self, g(a))
    case Primitive.Value.Boolean.Root                => String.valueOf(a)
    case Primitive.Value.Number.BigDecimal(_, _, _)  => a.toPlainString
    case Primitive.Value.Number.BigInteger(_, _, _)  => String.valueOf(a)
    case Primitive.Value.Number.Double(_, _, _)      => String.valueOf(a)
    case Primitive.Value.Number.Float(_, _, _)       => String.valueOf(a)
    case Primitive.Value.Number.Int(_, _, _)         => String.valueOf(a)
    case Primitive.Value.Number.Long(_, _, _)        => String.valueOf(a)
    case Primitive.Value.Number.Modify(self, _, g)   => encode(schema = self, g(a))
    case Primitive.Value.String.Modify(self, _, g)   => encode(schema = self, g(a))
    case Primitive.Value.String.Parsed(self)    => encode(schema = self, a)
    case Primitive.Value.String.Parser(_, _, encode) => apply(encode(a))
    case Primitive.Value.String.Text(_, _, _)        => apply(a)

  def apply(value: String): String = if quotes then s""""${{ escape(value, "\"") }}"""" else value

object PrimitivePrinter:
  val Quoted: Encoder[Primitive.Value, String] = PrimitivePrinter(quotes = true)
  val Unquoted: Encoder[Primitive.Value, String] = PrimitivePrinter(quotes = false)
