package io.taig.otter.http

import org.typelevel.ci.CIString
import io.taig.otter as Base
import cats.syntax.all.*
import org.typelevel.ci.*
import java.util.regex.Pattern
import cats.data.Validated

trait Codecs extends Base.Codecs:
  def cistring(
      minLength: Option[Int] = none,
      maxLength: Option[Int] = none,
      matches: Option[Pattern] = none
  ): Primitive.Required[CIString] = string(minLength, maxLength, matches).imap(CIString.apply)(_.toString)
  def cistring(matches: CIString): Primitive.Required[CIString] =
    cistring(matches = Pattern.compile(Pattern.quote(matches.toString), Pattern.CASE_INSENSITIVE).some)
  val cistring: Primitive.Required[CIString] = cistring()

  val __ : Url[Unit] = Url.Empty

  object method:
    inline def apply(value: String): Method = Method(value)

    val delete: Method = method("DELETE")
    val get: Method = method("GET")
    val head: Method = method("HEAD")
    val patch: Method = method("PATCH")
    val post: Method = method("POST")
    val put: Method = method("PUT")

  object code:
    inline def apply(value: Int): Code = Code(value)

    val ok: Code = code(200)
    val created: Code = code(201)
    val accepted: Code = code(202)
    val noContent: Code = code(204)
    val partialContent: Code = code(206)
    val movedPermanently: Code = code(301)
    val found: Code = code(302)
    val seeOther: Code = code(303)
    val notModified: Code = code(304)
    val temporaryRedirect: Code = code(307)
    val permanentRedirect: Code = code(308)
    val badRequest: Code = code(400)
    val unauthorized: Code = code(401)
    val forbidden: Code = code(403)
    val notFound: Code = code(404)
    val methodNotAllowed: Code = code(405)
    val conflict: Code = code(409)
    val gone: Code = code(410)
    val payloadTooLarge: Code = code(413)
    val unsupportedMediaTypes: Code = code(415)
    val unprocessableEntity: Code = code(422)
    val tooManyRequests: Code = code(429)
    val internalServerError: Code = code(500)
    val serviceUnavailable: Code = code(503)

  object header:
    inline def apply[A](
        name: CIString,
        codec: Codec.Of[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Optional[Data.Primitive]], A]
    ): Header[A] = inline codec match
      case codec: Codec.Of[Data.Primitive, A]                             => Header.Default(name, codec, Metadata.Empty)
      case codec: Codec.Of[Data.Array[Data.Primitive], A]                 => Header.Array(name, codec, Metadata.Empty)
      case codec: Codec.Of[Data.Object[Data.Optional[Data.Primitive]], A] => Header.Object(name, codec, Metadata.Empty)

    inline def authorization[A](
        codec: Codec.Of[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Optional[Data.Primitive]], A]
    ): Header[A] =
      header(ci"Authorization", codec)

  final inline def segment[A](
      name: String,
      codec: Codec.Required.Of[Data.Primitive | Data.Array[Data.Primitive], A] |
        Codec.Of[Data.Object[Data.Primitive], A]
  ): Segment.Parameter[A] = inline codec match
    case codec: Codec.Required.Of[Data.Primitive, A] => Segment.Parameter.Default(name, codec, Metadata.Empty)
    case codec: Codec.Required.Of[Data.Array[Data.Primitive], A] => Segment.Parameter.Array(name, codec, Metadata.Empty)
    case codec: Codec.Of[Data.Object[Data.Optional[Data.Primitive]], A] =>
      Segment.Parameter.Object(name, codec, Metadata.Empty)

  final inline def query[A](
      name: String,
      codec: Codec.Of[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Optional[Data.Primitive]], A]
  ): Query[A] = inline codec match
    case codec: Codec.Of[Data.Primitive, A]                             => Query.Default(name, codec, Metadata.Empty)
    case codec: Codec.Of[Data.Array[Data.Primitive], A]                 => Query.Array(name, codec, Metadata.Empty)
    case codec: Codec.Of[Data.Object[Data.Optional[Data.Primitive]], A] => Query.Object(name, codec, Metadata.Empty)

  object input:
    def apply[A](
        f: Array[Byte] => Validated[Violations[Violation[Constraint, Data]], Data],
        g: Data => Array[Byte],
        codec: Codec[A]
    ): Request.Body.Singlepart.Strict[A] =
      Request.Body.Singlepart.Strict(f, g, codec)

  final def endpoint[I, O](input: Request[I], output: Response[O]): Endpoint[I, O] = Endpoint(input, output)

  def result[A](code: Code, body: Response.Body.Strict[A]): Result[A] = Result(code, body)
  def result(code: Code): Result[Unit] = Result(code, Response.Body.Strict.Empty)

  // Scala.js won't compile if this is included here (for reason unknown)
  export ViolationsCodecs.*

  def error[F[+a] <: Data.Optional[a], O <: Data, A](
      name: String,
      codec: Base.Codec[F, O, A]
  ): Sum.Nested.Required.Of[F[O], A] = branch(name, codec).toBranches.toSumNested

object Codecs extends Codecs
