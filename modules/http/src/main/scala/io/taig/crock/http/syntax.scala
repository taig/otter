package io.taig.crock.http

import cats.Eval
import cats.syntax.all.*
import org.typelevel.ci.{CIString, CIStringSyntax}

import java.nio.charset.{Charset, IllegalCharsetNameException, StandardCharsets, UnsupportedCharsetException}

object syntax:
  val __ : Url[Unit] = Url.Root

  object header:
    def apply[A](name: CIString, schema: => Schema.Value[A]): Header[A] = Header.single(name, Eval.later(schema))
    def collection[A](name: CIString, schema: => Collection.Value[A]): Header[A] =
      Header.multiple(name, Eval.later(schema))

  def parameter[A](name: String, schema: => Schema.Value[A]): Segment[A] = Segment.Parameter(name, Eval.later(schema))

  def query[A](name: String, schema: => Schema.Value[A]): Query[A] = Query(name, Eval.later(schema))

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

  object input:
    val empty: Input.Body.Singlepart.Strict[Unit] = Input.Body.Singlepart.Strict.Empty
    val binary: Input.Body.Singlepart.Strict[Array[Byte]] = Input.Body.Singlepart.Strict.Bytes
    def binary(mediaType: MediaType): Input.Body.Singlepart.Strict[Array[Byte]] =
      val validation: Validation[String, String, String, MediaType] = ContentType.validation.map(_.mediaType)
      binary :* header(ci"Content-Type", string.ivalidate(validation)(_.toString).const(mediaType))

    val text: Input.Body.Singlepart.Strict[String] = binary.imapWithHeaders { (headers, bytes) =>
      val charset = headers
        .getFirst(ci"Content-Type")
        .flatMap(ContentType.parse)
        .flatMap(_.charset)
        .flatMap { charset =>
          try Charset.forName(charset).some
          catch {
            case _: IllegalCharsetNameException | _: UnsupportedCharsetException => none
          }
        }
        .getOrElse(StandardCharsets.UTF_8)
      new String(bytes, charset)
    } { value =>
      (
        Http.Headers.one(ci"Content-Type", ContentType(MediaType.text.plain, "utf-8".some).render),
        value.getBytes(StandardCharsets.UTF_8)
      )
    }

    val streaming: Input.Body.Singlepart.Streaming[Stream] = ???

    object multipart:
      val empty: Input.Body.Multipart[Unit] = ???
      def apply[A](part: Input.Body.Multipart.Part[A]): Input.Body.Multipart[A] = ???
