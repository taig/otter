package io.taig.openapi.http

import cats.Eval
import cats.data.Chain
import cats.syntax.all.*
import io.circe.{parser, Json}
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.schema.syntax.*
import io.taig.openapi.schema.{Discriminator, Schema, Sum}
import io.taig.screening.*
import io.taig.screening.syntax.*
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import io.taig.openapi.circe.*

import java.nio.charset.StandardCharsets
import scala.annotation.targetName

object syntax:
  val __ : Path[Unit] = Path.Root

  object header:
    @targetName("required")
    def apply[A](name: CIString, schema: => Schema.Of[A, OpenApi.Primitive]): Header[A] =
      Header(name, Eval.later(Header.Value.Required(schema)))

    @targetName("optional")
    def apply[A](name: CIString, schema: => Schema.Of[A, OpenApi.Null.type | OpenApi.Primitive]): Header[A] =
      Header(name, Eval.later(Header.Value.Optional(schema)))

  def parameter[A](name: String, schema: => Schema.Of[A, OpenApi.Primitive]): Segment.Parameter[A] =
    Segment.Parameter(name, Eval.later(schema))

  object query:
    @targetName("required")
    def apply[A](name: String, schema: => Schema.Of[A, OpenApi.Primitive]): Query[A] =
      Query(name, Eval.later(Query.Value.Required(schema)))

    @targetName("optional")
    def apply[A](name: String, schema: => Schema.Of[A, OpenApi.Null.type | OpenApi.Primitive]): Query[A] =
      Query(name, Eval.later(Query.Value.Optional(schema)))

  object body:
    val streaming: Input.Body.Singlepart[Streaming[Byte]] = Input.Body.Singlepart.streaming
    val strict: Input.Body.Singlepart[ByteVector] = Input.Body.Singlepart.strict
    val empty: Input.Body.Singlepart[Unit] = Input.Body.Singlepart.empty
    val text: Input.Body.Singlepart[String] = strict.imap { bytes =>
      // TODO infer charset properly
      new String(bytes.toArray, StandardCharsets.UTF_8)
    }(value => ByteVector(value.getBytes))
    def circe(empty: => Json): Input.Body.Singlepart[Json] =
      def parse(value: String, empty: => Json) = if value.isEmpty then empty.asRight else parser.parse(value)
      text
        .ivalidate(validations.parser("JSON")(parse(_, empty).toOption).mapReference(OpenApi.fromString))(_.spaces2)
    def openapi(empty: => OpenApi): Input.Body.Singlepart[OpenApi] = circe(toJson(empty)).imap(toOpenApi)(toJson)
    val openapi: Input.Body.Singlepart[OpenApi] = openapi(OpenApi.Null)
    def json[A](schema: => Schema[A], empty: => OpenApi): Input.Body.Singlepart[A] =
      openapi(empty).andThen(schema.decode)(schema.encode)
    def json[A](schema: => Schema[A]): Input.Body.Singlepart[A] = json(schema, OpenApi.Null)

    def multipart[A](part: Input.Body.Multipart.Part[A]): Input.Body.Multipart[A] = Input.Body.Multipart(part)
    def multipart[A](name: String, body: Input.Body.Singlepart[A]): Input.Body.Multipart[A] =
      multipart(part(name, body))
    def part[A](name: String, body: Input.Body.Singlepart[A]): Input.Body.Multipart.Part[A] =
      Input.Body.Multipart.Part(name, body)

  def input[A](method: Method, url: Url[A]): Input[A] = Input(method, url)

  def input[A](method: Method, path: Path[A]): Input[A] = Input(method, path.toUrl)

  transparent inline def input[A, B](method: Method, url: Url[A], body: Input.Body[B]): Input[?] =
    inline (url, body) match
      case (_, _: Input.Body[Unit]) => Input(method, url, body).imap { case (a, _) => a }(a => (a, ()))
      case (_: Url[Unit], _)        => Input(method, url, body).imap { case (_, b) => b }(b => ((), b))
      case (_, _)                   => Input(method, url, body)

  transparent inline def input[A, B](method: Method, path: Path[A], body: Input.Body[B]): Input[?] =
    input(method, path.toUrl, body)

  def result(code: Code): Output.Result[Unit] = Output.Result(code)

  def result[A](code: Code, body: => Schema[A]): Output.Result[A] = Output.Result(code, Eval.later(body))

  def output[A](results: Output.Results[A]): Output[A] = Output(
    results,
    result(code.unprocessableEntity, schemas.error.product("validation", violations))
  )

  def output[A](success: Output.Result[A]): Output[A] = output(success.toResults)

  inline def output[E, O](errors: Output.Results[E], success: Output.Result[O]): Output[Either[E, O]] =
    output(errors :+ success)

  object method:
    val delete: Method = Method("DELETE")
    val get: Method = Method("GET")
    val head: Method = Method("HEAD")
    val patch: Method = Method("PATCH")
    val post: Method = Method("POST")
    val put: Method = Method("PUT")

  object code:
    val ok: Code = Code(200)
    val created: Code = Code(201)
    val accepted: Code = Code(202)
    val noContent: Code = Code(204)
    val movedPermanently: Code = Code(301)
    val found: Code = Code(302)
    val seeOther: Code = Code(303)
    val temporaryRedirect: Code = Code(307)
    val permanentRedirect: Code = Code(308)
    val badRequest: Code = Code(400)
    val unauthorized: Code = Code(401)
    val forbidden: Code = Code(403)
    val notFound: Code = Code(404)
    val conflict: Code = Code(409)
    val payloadTooLarge: Code = Code(413)
    val unprocessableEntity: Code = Code(422)
    val internalServerError: Code = Code(500)
    val serviceUnavailable: Code = Code(503)

  extension [A](self: Headers[A])
    transparent inline def *[B](header: Header[B]): Headers[?] = inline (self, header) match
      case (_, _: Header[Unit])  => (self :* header).imap { case (a, _) => a }(a => (a, ()))
      case (_: Headers[Unit], _) => (self :* header).imap { case (_, b) => b }(b => ((), b))
      case (_, _)                => self :* header

  extension [A](self: Header[A]) transparent inline def *[B](header: Header[B]): Headers[?] = self.toHeaders * header

  extension [A](self: Queries[A])
    transparent inline def &[B](query: Query[B]): Queries[?] = inline (self, query) match
      case (_, _: Query[Unit])   => (self :& query).imap { case (a, _) => a }(a => (a, ()))
      case (_: Queries[Unit], _) => (self :& query).imap { case (_, b) => b }(b => ((), b))
      case (_, _)                => self :& query

  extension [A](self: Query[A]) transparent inline def &[B](query: Query[B]): Queries[?] = self.toQueries & query

  extension [A](self: Path[A])
    transparent inline def /[B](segment: Segment[B]): Path[?] = inline (self, segment) match
      case (_, _: Segment[Unit]) => (self :/ segment).imap { case (a, _) => a }(a => (a, ()))
      case (_: Path[Unit], _)    => (self :/ segment).imap { case (_, b) => b }(b => ((), b))
      case (_, _)                => self :/ segment

    transparent inline def /(segment: String): Path[?] = /(Segment.Value(segment))
    transparent inline def /[B](parameter: Segment.Parameter[B]): Path[?] = /(parameter: Segment[B])
    transparent inline def ?[B](query: Query[B]): Url[?] = self.toUrl & query

  extension [A](self: Url[A])
    transparent inline def &[B](query: Query[B]): Url[?] = inline (self, query) match
      case (_, _: Query[Unit]) => (self :& query).imap { case (a, _) => a }(a => (a, ()))
      case (_: Url[Unit], _)   => (self :& query).imap { case (_, b) => b }(b => ((), b))
      case (_, _)              => self :& query

  extension [A](self: Output.Result[A])
    def +[B](result: Output.Result[B]): Output.Results[Either[A, B]] = self :+ result

  extension [A <: Matchable](self: Output.Result[A])
    inline def |[B <: Matchable](result: Output.Result[B]): Output.Results[A | B] = self.toResults | result

  extension [A](self: Output.Results[A])
    def +[B](result: Output.Result[B]): Output.Results[Either[A, B]] = self :+ result

  extension [A <: Matchable](self: Output.Results[A])
    inline infix def or[B <: Matchable](results: Output.Results[B]): Output.Results[A | B] = self
      .orElse(results)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

    inline def |[B <: Matchable](result: Output.Result[B]): Output.Results[A | B] = or(result.toResults)
