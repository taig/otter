package io.taig.otter.codec

import io.bullet.borer.Dom
import io.taig.otter.Json
import io.taig.otter.Primitive

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

/** Writes a primitive as a `Dom.Element`, which is not how this module writes a document.
  *
  * [[ConstantDecoder]] and [[EnumerationDecoder]] take an `Encoder[F, T]` at the same `T` as their decoder, because
  * what they report is the value they *expected*, rendered as `io.taig.data.Data`: a constant says `*.equals "fiction"`
  * and an enumeration lists every value it takes. [[JsonBorerEncoder]] writes into a `Writer`, which is an effect on an
  * output rather than a value, so it cannot answer that question and this can. Nothing else uses it, and no document is
  * ever built out of it.
  *
  * Which element each case picks is chosen so that `JsonBorer.toData` renders it exactly as `data-circe` renders what
  * `JsonPrimitiveCirceEncoder` writes -- that agreement is what makes a violation from this module and a violation from
  * the circe one the same value. A `Float` therefore goes in as a `FloatElem` rather than as its `toString`: nothing
  * should turn on `Float.toString`, which the JVM and Scala.js do not agree about.
  */
object JsonPrimitiveBorerDomEncoder extends Encoder[Json.Primitive.Node, Dom.Element]:
  override def encode[W](json: Json.Primitive.Node[W, Any], w: W): Dom.Element = json match
    case Json.Primitive.Boolean.Schema(annotation) => encode(annotation.self, w)
    case Json.Primitive.Number.Schema(annotation)  => encode(annotation.self, w)
    case Json.Primitive.Text.Schema(annotation)    => encode(annotation.self, w)

  def encode[W](schema: Primitive[W, Any], w: W): Dom.Element = schema match
    case Primitive.Modify(self, _, g)         => encode(self, g(w))
    case Primitive.Boolean.Modify(self, _, g) => encode(self, g(w))
    case Primitive.Boolean.Root               => Dom.BooleanElem(w)
    case Primitive.Number.BigDecimal(_)       => Dom.NumberStringElem((w: JBigDecimal).toString)
    case Primitive.Number.BigInteger(_)       => Dom.NumberStringElem((w: JBigInteger).toString)
    case Primitive.Number.Double(_)           => JsonPrimitiveBorerDomEncoder.double(w)
    case Primitive.Number.Float(_)            => JsonPrimitiveBorerDomEncoder.float(w)
    case Primitive.Number.Int(_)              => Dom.IntElem(w)
    case Primitive.Number.Long(_)             => Dom.LongElem(w)
    case Primitive.Number.Modify(self, _, g)  => encode(self, g(w))
    case Primitive.Text.Format(_, _, print)   => Dom.StringElem(print(w))
    case Primitive.Text.Modify(self, _, g)    => encode(self, g(w))
    case Primitive.Text.Root(_)               => Dom.StringElem(w)

  /** `Json.fromDoubleOrString`, as [[JsonPrimitiveBorerEncoder.double]]. */
  private def double(value: Double): Dom.Element =
    if value.isNaN || value.isInfinite then Dom.StringElem(String.valueOf(value)) else Dom.DoubleElem(value)

  private def float(value: Float): Dom.Element =
    if value.isNaN || value.isInfinite then Dom.StringElem(String.valueOf(value)) else Dom.FloatElem(value)
