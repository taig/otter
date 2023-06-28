package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import io.taig.openapi.schema.{Collection, Schema, Void}
import io.taig.openapi.validation.Validation
import io.taig.openapi.validation.Constraint
import org.typelevel.ci.{CIString, CIStringSyntax}

import java.nio.charset.{Charset, IllegalCharsetNameException, StandardCharsets, UnsupportedCharsetException}
import scala.collection.immutable.VectorMap

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
        val binary: Input.Body.Singlepart[Array[Byte]] = Input.Body.Singlepart.Strict

        def string(mediaType: Option[String]): Input.Body.Singlepart[String] =
          val validation: Validation[
            String,
            (Http.Headers, Array[Byte]),
            (Http.Headers, Array[Byte]),
            (Option[String], Charset, Array[Byte])
          ] = Validation.fromOptionNec(Constraint.parser("Content-Type")) { case (headers, bytes) =>
            headers.getFirst(ci"Content-Type").flatMap(parseContentType) match
              case Some(contentType) if mediaType.forall(_ === contentType.mediaType) =>
                (mediaType, contentType.charset.getOrElse(StandardCharsets.UTF_8), bytes).some
              case Some(_) => none
              case None    => (none, StandardCharsets.UTF_8, bytes).some
          }

          binary.withHeaders
            .ivalidate(validation) {
              case (Some(mediaType), charset, bytes) =>
                (Http.Headers.one(ci"Content-Type", s"$mediaType; charset=${charset.name()}"), bytes)
              case (None, _, bytes) => (Http.Headers.Empty, bytes)
            }
            .imap { case (_, charset, bytes) => new String(bytes, charset) } { value =>
              (mediaType, StandardCharsets.UTF_8, value.getBytes(StandardCharsets.UTF_8))
            }

        val text: Input.Body.Singlepart[String] = string(mediaType = "text/plain".some)

      object streaming:
        val binary: Input.Body.Singlepart[Entity[Byte]] = Input.Body.Singlepart.Streaming

//    val empty: Input.Body.Singlepart[Void] = Input.Body.Singlepart.Empty
//    val strict: Input.Body.Singlepart[Array[Byte]] = Input.Body.Singlepart.Strict
//     TODO get proper charset
//    val text: Input.Body.Singlepart[String] = strict.imap(bytes => new String(bytes))(_.getBytes)

//    inline def apply[A](method: Method, url: Url[A]): Input[?] =
//      Input(method, url, Headers.Empty, Input.Body.Singlepart.Empty)
//
//    inline def apply[A, B](method: Method, url: Url[A], headers: Headers[B]): Input[?] =
//      Input(method, url, headers, Input.Body.Singlepart.Empty)
//
//    inline def apply[A, B](method: Method, url: Url[A], body: Input.Body[B]): Input[?] =
//      Input(method, url, Headers.Empty, body)
//
//    inline def apply[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Input.Body[C]): Input[?] =
//      Input(method, url, headers, body)
