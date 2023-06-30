package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import io.taig.openapi.http.headers.{ContentType, MediaType}
import io.taig.openapi.schema.{Collection, Schema, Void}
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
