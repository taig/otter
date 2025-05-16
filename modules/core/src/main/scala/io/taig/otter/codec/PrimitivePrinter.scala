package io.taig.otter.codec
import io.taig.otter.Primitive
import io.taig.otter.escape

final class PrimitivePrinter(quotes: Boolean) extends Encoder[Primitive, String]:
  override def encode[A](codec: Primitive[A], a: A): String = codec match
    case Primitive.Boolean.Modify(self, _, g)              => encode(codec = self, g(a))
    case Primitive.Boolean.Root(_)                         => String.valueOf(a)
    case Primitive.Number.BigDecimal(_, _, _, _)           => a.toPlainString
    case Primitive.Number.BigInteger(_, _, _, _)           => String.valueOf(a)
    case Primitive.Number.Double(_, _, _, _)               => String.valueOf(a)
    case Primitive.Number.Float(_, _, _, _)                => String.valueOf(a)
    case Primitive.Number.Int(_, _, _, _)                  => String.valueOf(a)
    case Primitive.Number.Long(_, _, _, _)                 => String.valueOf(a)
    case Primitive.Number.Modify(self, _, g)               => encode(codec = self, g(a))
    case Primitive.String.Modify(self, _, g)               => encode(codec = self, g(a))
    case Primitive.String.Parser(_, _, encode, _, _, _, _) => apply(encode(a))
    case Primitive.String.Text(_, _, _, _)                 => apply(a)

  def apply(value: String): String = if quotes then s""""${{ escape(value, "\"") }}"""" else value

object PrimitivePrinter:
  val Quoted: Encoder[Primitive, String] = PrimitivePrinter(quotes = true)
  val Unquoted: Encoder[Primitive, String] = PrimitivePrinter(quotes = false)
