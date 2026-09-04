package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Dom
import io.taig.otter.Coerce
import io.taig.otter.Json
import io.taig.otter.JsonBorerNumber
import io.taig.otter.Violations

import scala.util.matching.Regex

/** Normalises a laxer wire representation before handing it to the primitive decoder: a quoted boolean or number is
  * accepted where one is expected, and a boolean or number is accepted where a string is expected.
  *
  * Coercing a number to text is the one place this cannot answer as circe does. circe keeps a number's lexeme, so
  * `1.50` coerces to `"1.50"`; borer's parser has already turned it into a `DoubleElem`, so it coerces to `"1.5"`. The
  * value is the same and the text is not.
  */
object JsonCoerceBorerDecoder extends Decoder[[w, r] =>> Coerce[Json.Primitive.Node, w, r], Dom.Element]:
  /** JSON's own number grammar, which is what `io.circe.JsonNumber.fromString` accepts. Deliberately stricter than
    * `java.math.BigDecimal`, which would take a leading `+` and other things no document can carry.
    */
  private val Number: Regex = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r

  override def decode[R](
      schema: Coerce[Json.Primitive.Node, Nothing, R],
      element: Dom.Element
  ): Validated[Violations, R] = schema match
    case Coerce.Modify(self, f, _) => decode(self, element).map(f)
    case Coerce.Root(reference)    =>
      val primitive = reference.value
      JsonPrimitiveBorerDecoder.decode(primitive, JsonCoerceBorerDecoder.coerce(primitive, element))

  private def coerce[R](schema: Json.Primitive.Node[Nothing, R], element: Dom.Element): Dom.Element = schema match
    case Json.Primitive.Boolean.Schema(_) =>
      JsonCoerceBorerDecoder.text(element).flatMap(_.toBooleanOption).fold(element)(Dom.BooleanElem.apply)
    case Json.Primitive.Number.Schema(_) =>
      JsonCoerceBorerDecoder.text(element).flatMap(JsonCoerceBorerDecoder.number).getOrElse(element)
    case Json.Primitive.Text.Schema(_) =>
      element match
        case JsonBorerNumber(number) => Dom.StringElem(number.lexeme)
        case Dom.BooleanElem(value)  => Dom.StringElem(String.valueOf(value))
        case element                 => element

  private def text(element: Dom.Element): Option[String] = element match
    case element: Dom.AbstractTextElem => element.compact.some
    case _                             => none

  private def number(value: String): Option[Dom.Element] =
    Option.when(JsonCoerceBorerDecoder.Number.matches(value))(Dom.NumberStringElem(value))
