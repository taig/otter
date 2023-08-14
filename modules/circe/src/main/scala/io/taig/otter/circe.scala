package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.circe.jawn.JawnParser
import io.circe.{Json, JsonObject, Printer}
import io.taig.otter.http.headers.{ContentType, MediaType}
import io.taig.otter.http.{Request, headers}
import io.taig.otter.schema.schemas.*
import io.taig.otter.schema.{Dynamic, Schema, Violations, schemas}
import io.taig.otter.validation.Violation

import java.nio.charset.StandardCharsets
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

  object request:
    import io.taig.otter.http.syntax.request.binary

    private val parser: JawnParser = new JawnParser(maxValueSize = None, allowDuplicateKeys = true)

    def json(printer: Printer): Request.Body.Singlepart.Strict[Json] =
      (binary :* headers.contentType.optional).andThen { case (bytes, _) =>
        parser.parseByteArray(bytes).toValidated.leftMap(_ => Violations.rootNec(Violation.tpe("json")))
      } { json =>
        val utf8 = StandardCharsets.UTF_8
        (printer.print(json).getBytes(utf8), ContentType(MediaType.text.plain, utf8.name.some).some)
      }
    val json: Request.Body.Singlepart.Strict[Json] = json(Printer.noSpaces)
    def json[A](schema: => Schema[A]): Request.Body.Singlepart.Strict[A] =
      json.andThen(json => schema.decode(toOpenApi(json)))(schema.encode(_).map(toJson).getOrElse(Json.Null))

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
