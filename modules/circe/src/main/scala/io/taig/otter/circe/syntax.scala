package io.taig.otter.circe

import cats.data.Chain
import cats.syntax.all.*
import io.circe.jawn.JawnParser
import io.circe.{Json, Printer}
import io.taig.otter.http.{Request, Response}
import io.taig.otter.schema.Schema
import io.taig.otter.validation.Constraint
import io.taig.otter.validation.Validation
import org.typelevel.ci.CIStringSyntax

import java.nio.charset.StandardCharsets

object syntax:

  object validations:
    val json: Validation[Array[Byte], Json] =
      val parser: JawnParser = new JawnParser(maxValueSize = None, allowDuplicateKeys = true)
      Validation(Constraint.Type("json"))(parser.parseByteArray(_).leftMap(_ => none).toValidatedNec)

  object request:
    import io.taig.otter.http.syntax.request.binary

    def json(printer: Printer): Request.Body.Singlepart.Strict[Json] = binary.withHeaders.ivalidate {
      validations.json.lmap { case (_, bytes) => bytes }
    } { json =>
      (Chain.one(ci"Content-Type", "application/json"), printer.print(json).getBytes(StandardCharsets.UTF_8))
    }
    val json: Request.Body.Singlepart.Strict[Json] = json(Printer.noSpaces)
    def json[A](schema: => Schema[A]): Request.Body.Singlepart.Strict[A] =
      json.andThen(CirceDecoder.schema.decode(schema, _))(CirceEncoder.schema.encode(schema, _).getOrElse(Json.Null))

  object response:
    import io.taig.otter.http.syntax.response.binary

    def json(printer: Printer): Response.Body.Strict[Json] =
      binary.ivalidate(validations.json)(printer.print(_).getBytes(StandardCharsets.UTF_8))
    val json: Response.Body.Strict[Json] = json(Printer.noSpaces)
    def json[A](schema: => Schema[A]): Response.Body.Strict[A] =
      json.andThen(CirceDecoder.schema.decode(schema, _))(CirceEncoder.schema.encode(schema, _).getOrElse(Json.Null))
