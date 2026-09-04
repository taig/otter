package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Dom
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.JsonBorer
import io.taig.otter.JsonBorerNumber
import io.taig.otter.Primitive
import io.taig.otter.Violations
import io.taig.validation.Validation
import io.taig.validation.Violation

object JsonPrimitiveBorerDecoder extends Decoder[Json.Primitive.Node, Dom.Element]:
  override def decode[R](json: Json.Primitive.Node[Nothing, R], element: Dom.Element): Validated[Violations, R] =
    json match
      case Json.Primitive.Boolean.Schema(annotation) => decode(annotation.self, element)
      case Json.Primitive.Number.Schema(annotation)  => decode(annotation.self, element)
      case Json.Primitive.Text.Schema(annotation)    => decode(annotation.self, element)

  def decode[R](schema: Primitive[Nothing, R], element: Dom.Element): Validated[Violations, R] = schema match
    case Primitive.Modify(self, f, _)         => decode(self, element).map(f)
    case Primitive.Boolean.Modify(self, f, _) => decode(self, element).map(f)
    case Primitive.Boolean.Root               =>
      JsonPrimitiveBorerDecoder
        .boolean(element)
        .toValid(JsonPrimitiveBorerDecoder.mismatch("boolean", element))
        .leftMap(Violations.apply)
    case Primitive.Number.BigDecimal(validation) => number("bigDecimal", element, _.toBigDecimal, validation)
    case Primitive.Number.BigInteger(validation) => number("bigInteger", element, _.toBigInteger, validation)
    case Primitive.Number.Double(validation)     => number("double", element, _.toDouble.some, validation)
    case Primitive.Number.Float(validation)      => number("float", element, _.toFloat.some, validation)
    case Primitive.Number.Int(validation)        => number("int", element, _.toInt, validation)
    case Primitive.Number.Long(validation)       => number("long", element, _.toLong, validation)
    case Primitive.Number.Modify(self, f, _)     => decode(self, element).map(f)
    case Primitive.Text.Format(name, parse, _)   =>
      JsonPrimitiveBorerDecoder
        .text(element)
        .andThen: input =>
          parse(input).toValidated
            .leftMap: error =>
              Violation(Constraint.Generic.Type(name), actual = JsonBorer.toData(element), hint = error.some)
            .leftMap(Violations.apply)
    case Primitive.Text.Modify(self, f, _) => decode(self, element).map(f)
    case Primitive.Text.Root(validation)   =>
      JsonPrimitiveBorerDecoder
        .text(element)
        .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))

  private def mismatch(name: String, element: Dom.Element): Violation[Constraint] =
    Violation(constraint = Constraint.Generic.Type(name), actual = JsonBorer.typeOf(element).asData, hint = none)

  private def boolean(element: Dom.Element): Option[Boolean] = element match
    case Dom.BooleanElem(value) => value.some
    case _                      => none

  /** Both text elements, because a `Dom` reached from CBOR can hold a text stream where a document holds a string. */
  private def text(element: Dom.Element): Validated[Violations, String] = element match
    case element: Dom.AbstractTextElem => element.compact.valid
    case _                             => Violations(JsonPrimitiveBorerDecoder.mismatch("string", element)).invalid

  /** The target is asked of the *value* rather than of the element, which is what [[JsonBorerNumber]] is for: a
    * document writing `412.0` means the same `Int` as one writing `412`, and borer's parser has already chosen a
    * different element for each.
    */
  private def number[A](
      name: String,
      element: Dom.Element,
      extract: JsonBorerNumber => Option[A],
      validation: Validation[Constraint.Primitive.Number, A]
  ): Validated[Violations, A] =
    JsonBorerNumber
      .unapply(element)
      .flatMap(extract)
      .toValid(JsonPrimitiveBorerDecoder.mismatch(name, element))
      .leftMap(Violations.apply)
      .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
