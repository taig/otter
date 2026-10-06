package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.Data
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.JsonCirce
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.validation.Violation

import scala.annotation.tailrec
import scala.util.Try

object JsonCirceDynamicDecoder:
  def decode[W, R](schema: Json.Dynamic.Node[W, R], json: CirceJson): Validated[Violations, R] =
    decode(schema, json, numberLexemes(json.noSpaces).iterator)

  private def decode[W, R](
      schema: Json.Dynamic.Node[W, R],
      json: CirceJson,
      numbers: Iterator[String]
  ): Validated[Violations, R] = schema match
    case Json.Dynamic.Node.AnyValue()              => document(json, numbers)
    case Json.Dynamic.Node.ObjectValue(validation) =>
      json.asObject match
        case None      => mismatch("object", json).invalid
        case Some(obj) =>
          obj.toList
            .traverse { (key, value) =>
              document(value, numbers).leftMap(violations => Step.Field(key) /: violations).map(key -> _)
            }
            .map(values => Data.Object(values))
            .andThen(value => validation.validate(value).toInvalid(value).leftMap(Violations.apply))
    case Json.Dynamic.Node.ArrayValue =>
      json.asArray match
        case None         => mismatch("array", json).invalid
        case Some(values) =>
          values.zipWithIndex.toList
            .traverse { (value, index) =>
              document(value, numbers).leftMap(violations => Step.Index(index) /: violations)
            }
            .map(values => Data.Array(values))
    case Json.Dynamic.Node.Modify(self, f, _) => decode(self, json, numbers).map(f)

  private def document(json: CirceJson, numbers: Iterator[String]): Validated[Violations, Data] = json.fold(
    jsonNull = Data.Null.valid,
    jsonBoolean = _.valid,
    jsonNumber = value => number(numbers.nextOption().getOrElse(value.toString)),
    jsonString = _.valid,
    jsonArray = values =>
      values.zipWithIndex.toList
        .traverse { (value, index) =>
          document(value, numbers).leftMap(violations => Step.Index(index) /: violations)
        }
        .map(values => Data.Array(values)),
    jsonObject = obj =>
      obj.toList
        .traverse { (key, value) =>
          document(value, numbers).leftMap(violations => Step.Field(key) /: violations).map(key -> _)
        }
        .map(values => Data.Object(values))
  )

  private def number(lexeme: String): Validated[Violations, Data] =
    val decimal = Try(new java.math.BigDecimal(lexeme)).toOption
    val integer = decimal.flatMap(value => Try(value.toBigIntegerExact).toOption)
    val exact = integer
      .filter(_.bitLength < 32)
      .map(value => value.intValue: Data)
      .orElse(integer.filter(_.bitLength < 64).map(value => value.longValue: Data))
      .orElse(integer.map(value => value: Data))
      .orElse(decimal.map(value => value.stripTrailingZeros: Data))

    exact.toValid(
      Violations(
        Violation(
          Constraint.Generic.Type("representable JSON number"),
          actual = "number",
          hint = None
        )
      )
    )

  private def numberLexemes(document: String): List[String] =
    @tailrec
    def numberEnd(index: Int): Int =
      if index < document.length && isNumberChar(document.charAt(index)) then numberEnd(index + 1)
      else index

    @tailrec
    def loop(index: Int, quoted: Boolean, escaped: Boolean, values: List[String]): List[String] =
      if index >= document.length then values.reverse
      else
        val char = document.charAt(index)
        if quoted then
          if escaped then loop(index + 1, quoted = true, escaped = false, values)
          else if char == '\\' then loop(index + 1, quoted = true, escaped = true, values)
          else if char == '"' then loop(index + 1, quoted = false, escaped = false, values)
          else loop(index + 1, quoted = true, escaped = false, values)
        else if char == '"' then loop(index + 1, quoted = true, escaped = false, values)
        else if char == '-' || (char >= '0' && char <= '9') then
          val end = numberEnd(index + 1)
          loop(end, quoted = false, escaped = false, document.substring(index, end) :: values)
        else loop(index + 1, quoted = false, escaped = false, values)

    loop(0, quoted = false, escaped = false, values = Nil)

  private def isNumberChar(char: Char): Boolean =
    (char >= '0' && char <= '9') || char == '.' || char == 'e' || char == 'E' || char == '+' || char == '-'

  private def mismatch(name: String, json: CirceJson): Violations =
    Violations(Violation(Constraint.Generic.Type(name), actual = JsonCirce.typeOf(json).asData, hint = None))
