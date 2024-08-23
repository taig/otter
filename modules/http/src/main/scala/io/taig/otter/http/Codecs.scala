package io.taig.otter.http

import org.typelevel.ci.CIString
import io.taig.otter as Base
import io.taig.otter.http.header.MediaType
import cats.syntax.all.*
import org.typelevel.ci.*
import java.util.regex.Pattern
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import io.taig.otter.http.header.Parameters
import io.taig.otter.http.header.Accept

trait Codecs extends Base.Codecs, Types:
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
    val notAcceptable: Code = code(406)
    val conflict: Code = code(409)
    val gone: Code = code(410)
    val payloadTooLarge: Code = code(413)
    val unsupportedMediaTypes: Code = code(415)
    val unprocessableEntity: Code = code(422)
    val tooManyRequests: Code = code(429)
    val internalServerError: Code = code(500)
    val serviceUnavailable: Code = code(503)

  object mediaType:
    def apply(primary: String, secondary: String): MediaType =
      MediaType(tpe = MediaType.Type(primary, secondary), parameters = Parameters.Empty)

    object application:
      def apply(secondary: String): MediaType = mediaType(primary = "application", secondary)

      val json: MediaType = application(secondary = "json")
      val octetStream: MediaType = application(secondary = "octet-stream")
      val wwwFormUrlencoded: MediaType = application(secondary = "x-www-form-urlencoded")

    object multipart:
      def apply(secondary: String): MediaType = mediaType(primary = "multipart", secondary)

      val fromData: MediaType = application(secondary = "form-data")

    object text:
      def apply(secondary: String): MediaType = mediaType(primary = "text", secondary)

      val csv: MediaType = text(secondary = "csv")
      val plain: MediaType = text(secondary = "plain")
      val html: MediaType = text(secondary = "html")

  object header:
    private type Of = Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Optional[Data.Primitive]]

    inline def apply[A](
        name: CIString,
        codec: Codec.Of[Of, A]
    ): Header[A] = inline codec match
      case codec: Codec.Of[Data.Primitive, A]                             => Header.Default(name, codec, Metadata.Empty)
      case codec: Codec.Of[Data.Array[Data.Primitive], A]                 => Header.Array(name, codec, Metadata.Empty)
      case codec: Codec.Of[Data.Object[Data.Optional[Data.Primitive]], A] => Header.Object(name, codec, Metadata.Empty)

    inline def accept[A](codec: Codec.Of[Of, A]): Header[A] = header(ci"Accept", codec)
    val accept: Header[Accept] = accept(Accept.codec)

    inline def authorization[A](codec: Codec.Of[Of, A]): Header[A] = header(ci"Authorization", codec)

    inline def contentType[A](codec: Codec.Of[Of, A]): Header[A] = header(ci"Content-Type", codec)
    val contentType: Header[MediaType] = contentType(MediaType.codec)

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

  object body:
    def apply[A](
        mediaType: MediaType,
        f: (Option[Charset], Array[Byte]) => Codec.Result[A],
        g: (Option[Charset], A) => Array[Byte]
    ): Body.Strict[A] = Body.Strict(mediaType, of = none, f, g)

    def apply[F[+a] <: Data.Optional[a], O <: Data, A](
        mediaType: MediaType,
        codec: Base.Codec[F, O, A],
        f: (Option[Charset], Array[Byte]) => Codec.Result[Data],
        g: (Option[Charset], F[O]) => Array[Byte]
    ): Body.Strict[A] = Body.Strict(
      mediaType,
      of = codec.some,
      f(_, _).andThen(codec.decode),
      (charset, fo) => g(charset, codec.encode(fo))
    )

  def binary(mediaType: MediaType): Body.Strict[Array[Byte]] =
    body(mediaType, (_, bytes) => bytes.valid, (_, bytes) => bytes)
  val binary: Body.Strict[Array[Byte]] = binary(mediaType.application.octetStream)

  def text(fallback: => Charset): Body.Strict[String] = body(
    mediaType = mediaType.text.plain,
    (charset, bytes) => new String(bytes, charset.getOrElse(fallback)).valid,
    (charset, text) => text.getBytes(charset.getOrElse(fallback))
  )

  val text: Body.Strict[String] = text(fallback = StandardCharsets.UTF_8)

  def text[A](
      codec: Codec.Required.Of[Data.Primitive, A],
      fallback: => Charset = StandardCharsets.UTF_8
  ): Body.Strict[A] = body(
    mediaType = mediaType.text.plain,
    codec,
    (charset, bytes) =>
      val value = new String(bytes, charset.getOrElse(fallback))
      Data.String(value).valid
    ,
    (charset, data) => data.plain.getBytes(charset.getOrElse(fallback))
  )

  object formData:
    private type Of = Data.Object[Data.Optional[Data.Primitive]]

    def apply[A](codec: Codec.Required.Of[Of, A], fallback: Charset = StandardCharsets.UTF_8): Body[A] = body(
      mediaType = mediaType.application.wwwFormUrlencoded,
      codec,
      (charset, bytes) =>
        val value = new String(bytes, charset.getOrElse(fallback))
        val formData = FormData.parse(value).toVector
        Data.Object(formData.map { case (key, value) => (key, value.fold(Data.Null)(Data.String.apply)) }).valid
      ,
      (charset, data) =>
        val values = data.values.map:
          case (key, Data.Null)            => (key, none)
          case (key, data: Data.Primitive) => (key, data.plain.some)

        FormData(values).show.getBytes(charset.getOrElse(fallback)),
    )

  def endpoint[I, O](input: Request[I], output: Response[O]): Endpoint[I, O] = Endpoint(input, output)

  def app[F[_]](routes: Routes[F]): App[F] = App(
    routes,
    error = result(code.notFound, text(error.text.routeNotFound)).toResults.to[App.Error]
  )

  final def result[A, B](code: Code, headers: Headers[A], bodies: Bodies[B])(using
      merge: Merge[A, B]
  ): Result[merge.Out] = Result(code, headers, bodies).imap(merge.apply)(merge.unapply)

  final def result[A, B](code: Code, headers: Headers[A], body: Body[B])(using
      merge: Merge[A, B]
  ): Result[merge.Out] = result(code, headers, body.toBodies)

  final def result[A](code: Code, bodies: Bodies[A]): Result[A] =
    Result(code, Headers.Empty, bodies).imap { case (_, a) => a }(a => ((), a))

  final def result[A](code: Code, body: Body[A]): Result[A] = result(code, body.toBodies)

  final def result[A](code: Code, headers: Headers[A]): Result[A] = Result(code, headers)

  final def result(code: Code): Result[Unit] = result(code, Headers.Empty)

  final def request[A, B, C](method: Method, url: Url[A], headers: Headers[B], bodies: Bodies[C])(using
      merge1: Merge[A, B],
      merge2: Merge[merge1.Out, C]
  ): Request[merge2.Out] = Request(method, url, headers, bodies).imap { case (a, b, c) =>
    merge2(merge1(a, b), c)
  } { out =>
    val (ab, c) = merge2.unapply(out)
    merge1.unapply(ab) :* c
  }

  final def request[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[C])(using
      merge1: Merge[A, B],
      merge2: Merge[merge1.Out, C]
  ): Request[merge2.Out] = request(method, url, headers, body.toBodies)

  final def request[A, B](method: Method, url: Url[A], bodies: Bodies[B])(using
      merge: Merge[A, B]
  ): Request[merge.Out] = Request(method, url, Headers.Empty, bodies)
    .imap { case (a, _, b) => merge((a, b)) } { out =>
      val (a, b) = merge.unapply(out)
      (a, (), b)
    }

  final def request[A, B](method: Method, url: Url[A], body: Body[B])(using
      merge: Merge[A, B]
  ): Request[merge.Out] = request(method, url, body.toBodies)

  final def request[A, B](method: Method, url: Url[A], headers: Headers[B])(using
      merge: Merge[A, B]
  ): Request[merge.Out] = Request(method, url, headers).imap(merge.apply)(merge.unapply)

  final def request[A](method: Method, url: Url[A]): Request[A] =
    Request(method, url, Headers.Empty).imap { case (a, _) => a }(a => (a, ()))

  object error:
    def apply[F[+a] <: Data.Optional[a], O <: Data, A](
        tpe: String,
        codec: Base.Codec[F, O, A]
    ): Record.Required.Of[Data.Primitive | F[O], A] = record {
      field("error", constant(string, tpe)) :*
        field("value", codec)
    }.nulls(Null.Hide)

    def apply(tpe: String): Record.Required[Unit] = error(tpe, void)

    val routeNotFound = error(tpe = "routeNotFound").as(App.Error.RouteNotFound)

    val contentNegotiationFailed = error(
      tpe = "contentNegotiationFailed",
      codec = violations.structured.to[Route.Error.ContentNegotiationFailed]
    )

    val mediaTypesUnsupported = error(
      tpe = "mediaTypesUnsupported",
      codec = violations.structured.to[Route.Error.MediaTypesUnsupported]
    )

    val validationViolations = error(
      tpe = "validationViolations",
      codec = violations.structured.to[Route.Error.ValidationViolations]
    )

    object text:
      val routeNotFound: Primitive.Required[App.Error.RouteNotFound.type] =
        parser(name = "routeNotFound")(App.Error.parse)(_.show)
      val contentNegotiationFailed: Primitive.Required[Route.Error.ContentNegotiationFailed] =
        parser(name = "contentNegotiationFailed")(Route.Error.ContentNegotiationFailed.parse)(_.show)
      val mediaTypesUnsupported: Primitive.Required[Route.Error.MediaTypesUnsupported] =
        parser(name = "mediaTypesUnsupported")(Route.Error.MediaTypesUnsupported.parse)(_.show)
      val validationViolations: Primitive.Required[Route.Error.ValidationViolations] =
        parser(name = "validationViolations")(Route.Error.ValidationViolations.parse)(_.show)

  final def response[A](results: Results[A], error: Results[Route.Error], failure: Result[Unit]): Response[A] =
    Response(results, error, failure)

  def response[A](results: Results[A]): Response[A] = response(
    results,
    error = (
      result(code.notAcceptable, text(error.text.contentNegotiationFailed)) :+
        result(code.unsupportedMediaTypes, text(error.text.mediaTypesUnsupported)) :+
        result(code.unprocessableEntity, text(error.text.validationViolations))
    ).to,
    failure = result(code.internalServerError)
  )

  final def response[A](result: Result[A]): Response[A] = response(result.toResults)

  final def response[A, B](errors: Results[A], results: Results[B]): Response[Either[A, B]] =
    response(errors.orElse(results))

  final def response[A, B](errors: Results[A], result: Result[B]): Response[Either[A, B]] =
    response(errors :+ result)

  // Scala.js won't compile if this is included here (for reasons unknown)
  export ViolationsCodecs.*

object Codecs extends Codecs
