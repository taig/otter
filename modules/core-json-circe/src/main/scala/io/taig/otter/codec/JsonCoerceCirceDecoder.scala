package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.Primitive
import io.taig.otter.Coerce

object JsonCoerceCirceDecoder extends Decoder[Json.Coerce.Read, CirceJson]:
  override def decode[A](schema: Json.Coerce.Read[A], json: CirceJson): Validated[Violations, A] =
    decode(schema = schema.self.self, json)

  def decode[A](schema: Coerce.Read[Json.Primitive.Read, A], json: CirceJson): Validated[Violations, A] = schema match
    case Coerce.Modify(self, f, _)   => decode(schema = self, json).map(f)
    case Coerce.Read.Modify(self, f) => decode(schema = self, json).map(f)
    case Coerce.Root(schema)         =>
      val value = schema.value match
        case _: Json.Primitive.Boolean.Read[?] =>
          json.withString:
            case "true"  => CirceJson.fromBoolean(true)
            case "false" => CirceJson.fromBoolean(false)
            case value   => CirceJson.fromString(value)
        case _: Json.Primitive.Number.Read[?] =>
          json.withString: value =>
            schema.value.self.self match
              case Primitive.Number.Int(_) =>
                value.toIntOption.map(CirceJson.fromInt).getOrElse(CirceJson.fromString(value))
              case Primitive.Number.Long(_) =>
                value.toLongOption.map(CirceJson.fromLong).getOrElse(CirceJson.fromString(value))
              case Primitive.Number.Float(_) =>
                value.toFloatOption.flatMap(CirceJson.fromFloat).getOrElse(CirceJson.fromString(value))
              case Primitive.Number.Double(_) =>
                value.toDoubleOption.flatMap(CirceJson.fromDouble).getOrElse(CirceJson.fromString(value))
              case Primitive.Number.BigInteger(_) =>
                Either
                  .catchOnly[NumberFormatException](CirceJson.fromBigInt(BigInt(value)))
                  .getOrElse(CirceJson.fromString(value))
              case Primitive.Number.BigDecimal(_) =>
                Either
                  .catchOnly[NumberFormatException](CirceJson.fromBigDecimal(BigDecimal(value)))
                  .getOrElse(CirceJson.fromString(value))
              case _ => CirceJson.fromString(value)
        case _: Json.Primitive.Text.Read[?] =>
          json
            .withBoolean(value => CirceJson.fromString(String.valueOf(value)))
            .withNumber(value => CirceJson.fromString(value.toString))

      JsonPrimitiveCirceDecoder.decode(schema = schema.value, json = value)
