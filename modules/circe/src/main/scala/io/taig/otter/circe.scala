package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import io.taig.otter.schema.Dynamic
import io.taig.otter.schema.schemas

import scala.collection.immutable.VectorMap

object circe:
  def toOpenApi(json: Json): OpenApi = json.fold(
    jsonNull = OpenApi.Null,
    jsonBoolean = OpenApi.Bool.apply,
    jsonNumber = number =>
      number.toInt.map(OpenApi.Integer.apply) orElse
        number.toLong.map(OpenApi.Integer.apply) orElse
        number.toBigInt.map(OpenApi.Integer.apply) orElse
        Some(number.toFloat)
          .filter(number => number != Float.NegativeInfinity && number != Float.PositiveInfinity)
          .map(OpenApi.Decimal.apply) orElse
        Some(number.toDouble)
          .filter(number => number != Double.NegativeInfinity && number != Double.PositiveInfinity)
          .map(OpenApi.Decimal.apply) orElse
        number.toBigDecimal.map(OpenApi.Decimal.apply) getOrElse
        OpenApi.Decimal(number.toDouble),
    jsonString = OpenApi.Text.apply,
    jsonArray = toOpenApiArray,
    jsonObject = toOpenApiObject
  )

  def toOpenApiArray(json: Vector[Json]): OpenApi.Array = OpenApi.Array(Chain.fromSeq(json).map(toOpenApi))

  def toOpenApiObject(json: JsonObject): OpenApi.Object =
    OpenApi.Object(VectorMap.from(json.toIterable).map { case (key, value) => (key, toOpenApi(value)) })

  def toJson(openapi: OpenApi): Json = openapi match
    case OpenApi.Array(values)              => Json.fromValues(values.map(toJson).toVector)
    case OpenApi.Decimal(value: BigDecimal) => Json.fromBigDecimal(value)
    case OpenApi.Decimal(value: Double)     => Json.fromDoubleOrString(value)
    case OpenApi.Decimal(value: Float)      => Json.fromFloatOrString(value)
    case OpenApi.Integer(value: BigInt)     => Json.fromBigInt(value)
    case OpenApi.Integer(value: Int)        => Json.fromInt(value)
    case OpenApi.Integer(value: Long)       => Json.fromLong(value)
    case OpenApi.Bool(value)                => Json.fromBoolean(value)
    case OpenApi.Null                       => Json.Null
    case OpenApi.Object(values)             => Json.fromFields(values.map { case (key, value) => (key, toJson(value)) })
    case OpenApi.Text(value)                => Json.fromString(value)

  object syntax:
    object dynamic:
      val json: Dynamic[Json] = schemas.dynamic.any.imap(toJson)(toOpenApi)

//  object validations:
//    val json: Validation[Array[Byte], Json] =
//      val parser: JawnParser = new JawnParser(maxValueSize = None, allowDuplicateKeys = true)
//      Validation(Constraint.Type("json"))(parser.parseByteArray(_).leftMap(_ => none).toValidatedNec)
//
//  object request:
//    import io.taig.otter.http.syntax.request.binary
//
//    def json(printer: Printer): Request.Body.Singlepart.Strict[Json] = binary.withHeaders.ivalidate {
//      validations.json.lmap { case (_, bytes) => bytes }
//    } { json =>
//      (Chain.one(ci"Content-Type", "application/json"), printer.print(json).getBytes(StandardCharsets.UTF_8))
//    }
//    val json: Request.Body.Singlepart.Strict[Json] = json(Printer.noSpaces)
//    def json[A](schema: => Schema.Of[Json, A]): Request.Body.Singlepart.Strict[A] =
//      json.andThen(CirceDecoder.schema.decode(schema, _))(CirceEncoder.schema.encode(schema, _).getOrElse(Json.Null))
//
//  object response:
//    import io.taig.otter.http.syntax.response.binary
//
//    def json(printer: Printer): Response.Body.Strict[Json] =
//      binary.withHeaders.ivalidate(validations.json.lmap { case (_, bytes) => bytes }) { json =>
//        (
//          Chain.one(ci"Content-Type" -> "application/json; charset=UTF-8"),
//          printer.print(json).getBytes(StandardCharsets.UTF_8)
//        )
//      }
//
//    val json: Response.Body.Strict[Json] = json(Printer.noSpaces)
//    def json[A](schema: => Schema.Of[Json, A]): Response.Body.Strict[A] =
//      json.andThen(CirceDecoder.schema.decode(schema, _))(CirceEncoder.schema.encode(schema, _).getOrElse(Json.Null))
