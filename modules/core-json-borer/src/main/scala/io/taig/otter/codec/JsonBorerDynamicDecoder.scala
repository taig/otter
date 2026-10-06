package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Dom
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.JsonBorer
import io.taig.otter.JsonBorerNumber
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.validation.Violation

object JsonBorerDynamicDecoder:
  def decode[W, R](schema: Json.Dynamic.Node[W, R], element: Dom.Element): Validated[Violations, R] =
    schema match
      case Json.Dynamic.Node.AnyValue()              => document(element)
      case Json.Dynamic.Node.ObjectValue(validation) =>
        element match
          case obj: Dom.MapElem =>
            val entries = obj.stringKeyedMembers.toList
            if entries.length != obj.size then keyMismatch(obj).invalid
            else
              entries
                .traverse { (key, value) =>
                  document(value).leftMap(violations => Step.Field(key) /: violations).map(key -> _)
                }
                .map(Data.Object(_))
                .andThen(value => validation.validate(value).toInvalid(value).leftMap(Violations.apply))
          case _ => mismatch("object", element).invalid
      case Json.Dynamic.Node.ArrayValue =>
        element match
          case array: Dom.ArrayElem =>
            array.elems.zipWithIndex.toList
              .traverse { (value, index) =>
                document(value).leftMap(violations => Step.Index(index) /: violations)
              }
              .map(values => Data.Array(values))
          case _ => mismatch("array", element).invalid
      case Json.Dynamic.Node.Modify(self, f, _) => decode(self, element).map(f)

  private def document(element: Dom.Element): Validated[Violations, Data] = element match
    case Dom.NullElem               => Data.Null.valid
    case Dom.BooleanElem(value)     => value.valid
    case JsonBorerNumber(number)    => exactNumber(number)
    case text: Dom.AbstractTextElem => text.compact.valid
    case array: Dom.ArrayElem       =>
      array.elems.zipWithIndex.toList
        .traverse { (value, index) =>
          document(value).leftMap(violations => Step.Index(index) /: violations)
        }
        .map(values => Data.Array(values))
    case obj: Dom.MapElem =>
      val entries = obj.stringKeyedMembers.toList
      if entries.length != obj.size then keyMismatch(obj).invalid
      else
        entries
          .traverse { (key, value) =>
            document(value).leftMap(violations => Step.Field(key) /: violations).map(key -> _)
          }
          .map(values => Data.Object(values))
    case _ => mismatch("JSON value", element).invalid

  private def exactNumber(number: JsonBorerNumber): Validated[Violations, Data] =
    val exact = number.toInt
      .map(value => value: Data)
      .orElse(number.toLong.map(value => value: Data))
      .orElse(number.toBigInteger.map(value => value: Data))
      .orElse(number.toBigDecimal.map(value => value: Data))

    exact.toValid(
      Violations(
        Violation(
          Constraint.Generic.Type("representable JSON number"),
          actual = "number",
          hint = None
        )
      )
    )

  private def mismatch(name: String, element: Dom.Element): Violations =
    Violations(
      Violation(Constraint.Generic.Type(name), actual = JsonBorer.toData(element), hint = None)
    )

  private def keyMismatch(obj: Dom.MapElem): Violations =
    val offending = obj.keys.find:
      case _: Dom.AbstractTextElem => false
      case _                       => true

    mismatch("string", offending.getOrElse(obj))
