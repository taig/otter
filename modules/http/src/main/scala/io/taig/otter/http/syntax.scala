package io.taig.otter.http

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Schema
import io.taig.otter.schemas
import org.typelevel.ci.CIString

import java.nio.charset.{Charset, IllegalCharsetNameException, StandardCharsets, UnsupportedCharsetException}

object syntax:
  val __ : Url[Unit] = Url.Root

  def header[A](name: CIString, schema: => Schema.Value[A] | Schema.Collection[Schema.Value, A]): Header[A] =
    Header(name, schema)

  def parameter[A](name: String, schema: => Schema.Value[A]): Segment[A] = Segment(name, schema)

  def query[A](name: String, schema: => Schema.Value[A] | Schema.Collection[Schema.Value, A]): Query[A] =
    Query(name, schema)

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
    inline def apply[A](method: Method, url: Url[Unit], body: Request.Body[A]): Request[A] =
      Request(method, url, body).imap { case (_, a) => a }(((), _))
    inline def apply(method: Method, url: Url[Unit]): Request[Unit] =
      Request(method, url, empty).imap(_ => ())(_ => ((), ()))

    val binary: Request.Body.Singlepart.Strict[Array[Byte]] = Request.Body.Singlepart.Strict.Root
    val empty: Request.Body.Singlepart.Strict[Unit] = binary.imap(_ => ())(_ => Array.emptyByteArray)
    def text(charset: Option[Charset]): Request.Body.Singlepart.Strict[String] = ???
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
    val text: Request.Body.Singlepart.Strict[String] = text(StandardCharsets.UTF_8.some)
    def of[A](body: Request.Body.Singlepart.Strict[A], schema: Schema[A]): Request.Body.Singlepart.Strict[A] = ???
//      openapi.andThen(schema.decode)(schema.encode(_).getOrElse(OpenApi.Null))

//    object streaming:
//      val empty: Request.Body.Singlepart.Streaming[Unit] = Request.Body.Singlepart.Streaming.Empty
//      val bytes: Request.Body.Singlepart.Streaming[Stream[Byte]] = Request.Body.Singlepart.Streaming.Bytes

  def result[A](code: Code, body: Response.Body.Strict[A]): Result[A] = Result(code, body)
  def result(code: Code): Result[Unit] = Result(code, response.empty)

  object response:
    def apply[A](results: Results[A]): Response[A] = Response(
      results,
      result(code.unprocessableEntity, response.of(???, schemas.violations))
    )
    def apply[A](result: Result[A]): Response[A] = response(result.toResults)

    val binary: Response.Body.Strict[Array[Byte]] = (Response.Body.Strict.Bytes :* headers.contentLength.optional)
      .imap { case (bytes, _) => bytes }(bytes => (bytes, bytes.length.toLong.some))
    val empty: Response.Body.Strict[Unit] = binary.imap(_ => ())(_ => Array.emptyByteArray)
    val text: Response.Body.Strict[String] =
      binary.imap(new String(_, StandardCharsets.UTF_8))(_.getBytes(StandardCharsets.UTF_8))
    def of[A](openapi: Response.Body.Strict[A], schema: => Schema[A]): Response.Body.Strict[A] = ???
//      openapi.andThen(schema.decode)(schema.encode(_).getOrElse(OpenApi.Null))

  extension [A: Eq, B](self: Chain[(A, B)])
    def all(key: A): Chain[B] = self.collect { case (reference, value) if key === reference => value }
    def first(key: A): Option[B] = self.collectFirst { case (reference, value) if key === reference => value }
    def removeAll(key: A): Chain[(A, B)] = self.filter:
      case (reference, _) if key === reference => false
      case _                                   => true
    def removeFirst(key: A): Chain[(A, B)] =
      var removed = false
      val result = List.newBuilder[(A, B)]
      self.iterator.foreach {
        case (reference, _) if key == reference && !removed => removed = true; ()
        case entry                                          => result += entry
      }
      Chain.fromSeq(result.result())
    def allWithRemainders(key: A): (Chain[B], Chain[(A, B)]) = (all(key), removeAll(key))
    def firstWithRemainders(key: A): Option[(B, Chain[(A, B)])] = first(key).tupleRight(removeFirst(key))
