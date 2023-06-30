package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import io.taig.openapi.http.Input.Body
import io.taig.openapi.http.headers.{ContentType, MediaType}
import io.taig.openapi.schema.{Collection, Schema}
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.validation.{Constraint, Validation}
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

  object input:
    object body:
      object strict:
        transparent inline def binary[A](headers: Headers[A]): Input.Body.Singlepart[?] =
          Input.Body.Singlepart.strict(headers)
        transparent inline def binary[A](header: Header[A]): Input.Body.Singlepart[?] = binary(header.toHeaders)
        def binary(mediaType: MediaType): Input.Body.Singlepart[Array[Byte]] =
          val validation: Validation[String, String, String, MediaType] = ContentType.validation.map(_.mediaType)
          binary(header(ci"Content-Type", string.ivalidate(validation)(_.toString).const(mediaType)))
        val binary: Input.Body.Singlepart[Array[Byte]] = Input.Body.Singlepart.strict(Headers.Empty)

        val text: Input.Body.Singlepart[String] = binary.imapWithHeaders { (headers, bytes) =>
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
