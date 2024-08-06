package io.taig.otter.http

import org.typelevel.ci.CIString
import io.taig.otter as Base
import cats.syntax.all.*
import org.typelevel.ci.*
import java.util.regex.Pattern
import cats.data.Validated
import Base.Evidence
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import Base.http.ViolationsCodecs.violations

trait Codecs extends Base.Codecs:
  self =>

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

  def body[F[+a] <: Data.Optional[a], O <: Data, A](
      mediaType: MediaType,
      codec: Base.Codec[F, O, A],
      f: (Option[Charset], Array[Byte]) => Codec.Result[Data],
      g: (Option[Charset], F[O]) => Array[Byte]
  ): Body[A] = Body(mediaType, codec, f, g)

  def binary(mediaType: MediaType): Body[Array[Byte]] = Body.binary(mediaType)
  val binary: Body[Array[Byte]] = binary(MediaType.application.octetStream)

  def text[A](codec: Codec.Required.Of[Data.Primitive, A]): Body[A] = body(
    mediaType = MediaType.text.plain,
    codec,
    (charset, bytes) =>
      val value = new String(bytes, charset.getOrElse(StandardCharsets.UTF_8))
      Data.String(value).valid
    ,
    (charset, data) => data.print(quoted = false).getBytes(charset.getOrElse(StandardCharsets.UTF_8))
  )

  def formData[A](codec: Codec.Required.Of[Data.Object[Data.Optional[Data.Primitive]], A]): Body[A] = body(
    mediaType = MediaType.application.wwwFormUrlencoded,
    codec,
    (charset, bytes) =>
      val value = new String(bytes, charset.getOrElse(StandardCharsets.UTF_8))
      val formData = FormData.parse(value).toVector
      Data.Object(formData.map { case (key, value) => (key, value.fold(Data.Null)(Data.String.apply)) }).valid
    ,
    (charset, data) =>
      val charsetOrUtf8 = charset.getOrElse(StandardCharsets.UTF_8)
      val values = data.values.map:
        case (key, Data.Null)            => (key, none)
        case (key, data: Data.Primitive) => (key, data.print(quoted = false).some)
      FormData(values).print(charsetOrUtf8).getBytes(charsetOrUtf8),
  )

  def endpoint[I, O](input: Request[I], output: Response[O]): Endpoint[I, O] = Endpoint(input, output)

  final def result[A, B](code: Code, headers: Headers[A], bodies: Bodies[B])(using
      merge: Evidence.Merge[A, B]
  ): Result[merge.Out] = Result(code, headers, bodies).imap(merge.apply)(merge.unapply)

  final def result[A, B](code: Code, headers: Headers[A], body: Body[B])(using
      merge: Evidence.Merge[A, B]
  ): Result[merge.Out] = result(code, headers, body.toBodies)

  final def result[A](code: Code, bodies: Bodies[A]): Result[A] =
    Result(code, Headers.Empty, bodies).imap { case (_, a) => a }(a => ((), a))

  final def result[A](code: Code, body: Body[A]): Result[A] = result(code, body.toBodies)

  final def result[A](code: Code, headers: Headers[A]): Result[A] = Result(code, headers)

  final def result(code: Code): Result[Unit] = result(code, Headers.Empty)

  final def request[A, B, C](method: Method, url: Url[A], headers: Headers[B], bodies: Bodies[C])(using
      merge1: Evidence.Merge[A, B],
      merge2: Evidence.Merge[merge1.Out, C]
  ): Request[merge2.Out] = Request(method, url, headers, bodies).imap { case (a, b, c) =>
    merge2(merge1(a, b), c)
  } { out =>
    val (ab, c) = merge2.unapply(out)
    merge1.unapply(ab) :* c
  }

  final def request[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[C])(using
      merge1: Evidence.Merge[A, B],
      merge2: Evidence.Merge[merge1.Out, C]
  ): Request[merge2.Out] = request(method, url, headers, body.toBodies)

  final def request[A, B](method: Method, url: Url[A], bodies: Bodies[B])(using
      merge: Evidence.Merge[A, B]
  ): Request[merge.Out] = Request(method, url, Headers.Empty, bodies)
    .imap { case (a, _, b) => merge((a, b)) } { out =>
      val (a, b) = merge.unapply(out)
      (a, (), b)
    }

  final def request[A, B](method: Method, url: Url[A], body: Body[B])(using
      merge: Evidence.Merge[A, B]
  ): Request[merge.Out] = request(method, url, body.toBodies)

  final def request[A, B](method: Method, url: Url[A], headers: Headers[B])(using
      merge: Evidence.Merge[A, B]
  ): Request[merge.Out] = Request(method, url, headers).imap(merge.apply)(merge.unapply)

  final def request[A](method: Method, url: Url[A]): Request[A] =
    Request(method, url, Headers.Empty).imap { case (a, _) => a }(a => (a, ()))

  final def response[A](results: Results[A]): Response[A] =
    // result(code.unsupportedMediaTypes, violations)

    Response(results, mediaTypesUnsupported = ???, validationViolations = ???)

  // Scala.js won't compile if this is included here (for reason unknown)
  export ViolationsCodecs.*

  def error[F[+a] <: Data.Optional[a], O <: Data, A](
      name: String,
      codec: Base.Codec[F, O, A]
  ): Sum.Nested.Required.Of[F[O], A] = branch(name, codec).toBranches.toSumNested

object Codecs extends Codecs
