package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.*
import org.typelevel.ci.CIString

import java.nio.charset.StandardCharsets

object syntax:
  val __ : Url[Unit] = Url.Root

  def header[A](name: CIString, codec: Value[A]): Header[A] = Header(name, codec)
  def header[A](name: CIString, codec: Collection.Of[Value[?], A]): Header[A] = Header(name, codec)
  def parameter[A](name: String, codec: Value.Required[A]): Segment[A] = Segment(name, codec)
  def parameter[A](name: String, codec: Union.Required[A]): Segment[A] = Segment(name, codec)
  def query[A](name: String, codec: Value[A]): Query[A] = Query(name, codec)
  def query[A](name: String, codec: Collection.Of[Value[?], A]): Query[A] = Query(name, codec)

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

  object request:
    inline def apply[A, B](method: Method, url: Url[A], body: Request.Body[B]): Request[(A, B)] =
      Request(method, url, body)
    inline def apply[A](method: Method, url: Url[A]): Request[A] =
      Request(method, url, input.empty).imap { case (a, _) => a }(a => (a, ()))
    inline def apply[A](method: Method, url: Url[Unit], body: Request.Body[A]): Request[A] =
      Request(method, url, body).imap { case (_, a) => a }(((), _))
    inline def apply(method: Method, url: Url[Unit]): Request[Unit] =
      Request(method, url, input.empty).imap(_ => ())(_ => ((), ()))

  object input:
    val empty: Request.Body.Singlepart.Strict[Unit] = Request.Body.Singlepart.Strict.Empty
    val binary: Request.Body.Singlepart.Strict[Array[Byte]] = Request.Body.Singlepart.Strict.Binary
    def apply[A](
        f: (Http.Headers, Array[Byte]) => Validated[Violations, Data],
        g: Data => (Http.Headers, Array[Byte]),
        codec: Codec[A]
    ): Request.Body.Singlepart.Strict[A] = Request.Body.Singlepart.Strict(f, g, codec)
//    def text(charset: Option[Charset]): Request.Body.Singlepart.Strict[String] =
//      (binary :* headers.contentType.optional).imap { case (bytes, contentType) =>
//        val charset = contentType
//          .flatMap(_.charset)
//          .flatMap { value =>
//            try Charset.forName(value).some
//            catch case _: IllegalCharsetNameException | _: UnsupportedCharsetException => none
//          }
//          .getOrElse(StandardCharsets.UTF_8)
//        new String(bytes, charset)
//      } { value =>
//        (
//          value.getBytes(charset.getOrElse(StandardCharsets.UTF_8)),
//          ContentType(MediaType.text.plain, charset.map(_.name)).some,
//        )
//      }
//    val text: Request.Body.Singlepart.Strict[String] = text(StandardCharsets.UTF_8.some)

  def result[A](code: Code, body: Response.Body.Strict[A]): Result[A] = Result(code, body)
  def result(code: Code): Result[Unit] = Result(code, output.empty)

  object output:
    def apply[A](body: Response.Body.Strict[Data], codec: Codec[A]): Response.Body.Strict[A] =
      body.andThen(codec.decode)(codec.encode)
    val binary: Response.Body.Strict[Array[Byte]] = (Response.Body.Strict.Bytes :* headers.contentLength.optional)
      .imap { case (bytes, _) => bytes }(bytes => (bytes, bytes.length.toLong.some))
    val empty: Response.Body.Strict[Unit] = binary.imap(_ => ())(_ => Array.emptyByteArray)
    val text: Response.Body.Strict[String] =
      binary.imap(new String(_, StandardCharsets.UTF_8))(_.getBytes(StandardCharsets.UTF_8))
