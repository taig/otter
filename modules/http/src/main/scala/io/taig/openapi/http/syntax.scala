package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.http.headers.{ContentType, MediaType}
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.schema.{Collection, Schema, Void}
import io.taig.openapi.validation.{Constraint, Validation}
import org.typelevel.ci.{CIString, CIStringSyntax}

import java.nio.charset.{Charset, IllegalCharsetNameException, StandardCharsets, UnsupportedCharsetException}

object syntax:
  val __ : Url[Void] = Url.Root

  object header:
    def apply[A](name: CIString, schema: => Schema.Value[A]): Header[A] = Header.single(name, Eval.later(schema))
    def collection[A](name: CIString, schema: => Collection.Value[A]): Header[A] =
      Header.multiple(name, Eval.later(schema))

  def parameter[A](name: String, schema: => Schema.Value[A]): Segment[A] = Segment.Parameter(name, Eval.later(schema))

  def query[A](name: String, schema: => Schema.Value[A]): Query[A] = Query(name, Eval.later(schema))

  object input:
    object body:
      object strict:
        def binary[A](headers: Headers[A]): Input.Body.Singlepart[(A, Array[Byte])] =
          Input.Body.Singlepart.strict(headers)
        def binary[A](header: Header[A]): Input.Body.Singlepart[(A, Array[Byte])] = binary(header.toHeaders)
        val binary: Input.Body.Singlepart[Array[Byte]] = Input.Body.Singlepart.strict

        val text: Input.Body.Singlepart[(Option[Charset], String)] =
          val validation: Validation[String, String, String, Option[Charset]] =
            ContentType.validation.andThen:
              val textPlain: Validation[String, String, ContentType, Unit] = Validation
                .condNec(Constraint.text.equal(MediaType.text.plain.toString)) { (contentType: ContentType) =>
                  contentType.mediaType === MediaType.text.plain
                }
                .mapActual(_.mediaType.toString)
              val charset: Validation[String, String, ContentType, Option[Charset]] =
                Validation
                  .fromOptionNec(Constraint("encoding", reference = none)) { (contentType: ContentType) =>
                    contentType.charset.map: charset =>
                      try Charset.forName(charset).some
                      catch {
                        case _: IllegalCharsetNameException | _: UnsupportedCharsetException => none
                      }
                  }
                  .mapActual(_.charset.getOrElse(StandardCharsets.UTF_8.name()))

              textPlain *> charset

          val contentType = string.ivalidate(validation) { charset =>
            ContentType(MediaType.text.plain, charset.map(_.name())).render
          }

          binary(header(ci"Content-Type", contentType)).imap { case (charset, bytes) =>
            (charset, new String(bytes, charset.getOrElse(StandardCharsets.UTF_8)))
          } { case (charset, value) =>
            (charset, value.getBytes(charset.getOrElse(StandardCharsets.UTF_8)))
          }
