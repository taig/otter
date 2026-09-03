package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Coerce
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Parameter

/** Normalises a laxer parameter before handing it to the primitive decoder.
  *
  * A URL is typed by nobody. What reaches a query string is whatever a form, a shell or another service put there, so
  * the leniencies worth naming are the ones those actually produce: surrounding whitespace, the several spellings of a
  * boolean, and a leading `+` on a number that percent decoding has already turned into a space.
  *
  * The one leniency that is HTTP's own is the bare flag. `?verbose` arrives as a name with no value at all, which
  * [[QueriesDecoder]] hands down as empty text, and a boolean reads that as `true` -- a name given without a value is
  * the request asserting it. Only a boolean does: empty text is a perfectly good `String` and a number that is not
  * there is not a number.
  */
object ParameterCoerceDecoder extends Decoder[[w, r] =>> Coerce[Parameter.Primitive.Node, w, r], String]:
  override def decode[R](
      schema: Coerce[Parameter.Primitive.Node, Nothing, R],
      value: String
  ): Validated[Violations, R] = schema match
    case Coerce.Modify(self, f, _) => decode(self, value).map(f)
    case Coerce.Root(reference)    =>
      val primitive = reference.value
      ParameterPrimitiveDecoder.decode(primitive, coerce(primitive, value))

  private def coerce[R](schema: Parameter.Primitive.Node[Nothing, R], value: String): String = schema match
    case Parameter.Primitive.Boolean.Schema(_) =>
      val trimmed = value.trim
      if trimmed.isEmpty then "true" else ParameterCoerceDecoder.booleans.getOrElse(trimmed.toLowerCase, trimmed)
    case Parameter.Primitive.Number.Schema(_) => value.trim.stripPrefix("+")
    case Parameter.Primitive.Text.Schema(_)   => value.trim

  /** What a form or a shell writes a boolean as. `true` and `false` in any casing are already accepted by the strict
    * decoder, so only the alternate spellings are listed here.
    */
  private val booleans: Map[String, String] = Map(
    "y" -> "true",
    "yes" -> "true",
    "on" -> "true",
    "1" -> "true",
    "n" -> "false",
    "no" -> "false",
    "off" -> "false",
    "0" -> "false"
  )
