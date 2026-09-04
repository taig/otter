package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Primitive

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object JsonPrimitiveBorerEncoder extends Encoder[Json.Primitive.Node, BorerWrite]:
  override def encode[W](json: Json.Primitive.Node[W, Any], w: W): BorerWrite = json match
    case Json.Primitive.Boolean.Schema(annotation) => encode(annotation.self, w)
    case Json.Primitive.Number.Schema(annotation)  => encode(annotation.self, w)
    case Json.Primitive.Text.Schema(annotation)    => encode(annotation.self, w)

  def encode[W](schema: Primitive[W, Any], w: W): BorerWrite = schema match
    case Primitive.Modify(self, _, g)         => encode(self, g(w))
    case Primitive.Boolean.Modify(self, _, g) => encode(self, g(w))
    case Primitive.Boolean.Root               => BorerWrite(_.writeBoolean(w))
    case Primitive.Number.BigDecimal(_)       => BorerWrite(_.writeNumberString((w: JBigDecimal).toString))
    case Primitive.Number.BigInteger(_)       => BorerWrite(_.writeNumberString((w: JBigInteger).toString))
    case Primitive.Number.Double(_)           => JsonPrimitiveBorerEncoder.double(w)
    case Primitive.Number.Float(_)            => JsonPrimitiveBorerEncoder.float(w)
    case Primitive.Number.Int(_)              => BorerWrite(_.writeInt(w))
    case Primitive.Number.Long(_)             => BorerWrite(_.writeLong(w))
    case Primitive.Number.Modify(self, _, g)  => encode(self, g(w))
    case Primitive.Text.Format(_, _, print)   => BorerWrite(_.writeString(print(w)))
    case Primitive.Text.Modify(self, _, g)    => encode(self, g(w))
    case Primitive.Text.Root(_)               => BorerWrite(_.writeString(w))

  /** `Json.fromDoubleOrString`, which is what the circe interpreter writes.
    *
    * JSON has no number for a NaN or an infinity, and borer does not paper over that: its JSON renderer *fails* on one
    * rather than writing something no parser would take back. So they go out as text, which is what circe does too, and
    * what keeps the two modules writing the same document.
    */
  private def double(value: Double): BorerWrite =
    if value.isNaN || value.isInfinite then BorerWrite(_.writeString(String.valueOf(value)))
    else BorerWrite(_.writeDouble(value))

  /** As [[double]], but kept a `Float` all the way to the output: widening `0.1f` to a `Double` first would write
    * `0.10000000149011612`, which reads back as the same `Float` but is not the document circe writes.
    *
    * On Scala.js that is what gets written anyway, because the platform has no float formatting of its own and
    * `0.1f.toString` is `"0.10000000149011612"` there. circe's `fromFloatOrString` writes the same thing on the same
    * platform, so the two modules still agree and it is the platform that differs; the value round trips either way.
    */
  private def float(value: Float): BorerWrite =
    if value.isNaN || value.isInfinite then BorerWrite(_.writeString(String.valueOf(value)))
    else BorerWrite(_.writeFloat(value))
