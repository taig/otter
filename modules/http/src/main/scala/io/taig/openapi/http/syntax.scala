package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
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
    val contentType: Header[ContentType] = header(ci"Content-Type", string.ivalidate(ContentType.validation)(_.render))

  def parameter[A](name: String, schema: => Schema.Value[A]): Segment[A] = Segment.Parameter(name, Eval.later(schema))

  def query[A](name: String, schema: => Schema.Value[A]): Query[A] = Query(name, Eval.later(schema))

  object input:
    object body:
      object strict:
        def binary[A](headers: Headers[A]): Input.Body.Singlepart[(A, Array[Byte])] =
          Input.Body.Singlepart.strict(headers)
        def binary[A](header: Header[A]): Input.Body.Singlepart[(A, Array[Byte])] = binary(header.toHeaders)
        val binary: Input.Body.Singlepart[Array[Byte]] = Input.Body.Singlepart.strict

        val text: Input.Body.Singlepart[String] =
          val validation: Validation[String, String, String, Option[Charset]] =
            ContentType.validation.andThen:
              val textPlain: Validation[String, String, ContentType, Unit] = Validation
                .condNec(Constraint.text.equal(MediaType.text.plain.toString)) { (contentType: ContentType) =>
                  contentType.mediaType === MediaType.text.plain
                }
                .mapActual(_.render)
              val charset: Validation[String, String, ContentType, Option[Charset]] = ???

              textPlain *> charset

          ???

//        val binaryWithContentType: Input.Body.Singlepart[(Option[ContentType], Array[Byte])] =
//          Input.Body.Singlepart.strict(header.contentType.optional.toHeaders)
//
//        def stringWithMediaTypeAndCharset: Input.Body.Singlepart[(Option[MediaType], Option[Charset], String)] =
//          binaryWithContentType.imap { case (contentType, bytes) =>
//            val charset = contentType
//              .flatMap(_.charset)
//              .flatMap { charset =>
//                try Charset.forName(charset).some
//                catch {
//                  case _: IllegalCharsetNameException | _: UnsupportedCharsetException => none
//                }
//              }
//
//            (contentType.map(_.mediaType), charset, new String(bytes, charset.getOrElse(StandardCharsets.UTF_8)))
//          } { case (mediaType, charset, value) =>
//            (
//              mediaType.map(ContentType(_, charset.map(_.name()))),
//              value.getBytes(charset.getOrElse(StandardCharsets.UTF_8))
//            )
//          }

//        def string(mediaType: Option[String]): Input.Body.Singlepart[String] = ???
//
//        val text: Input.Body.Singlepart[String] = string(mediaType = "text/plain".some)
//
//      object streaming:
//        val binary: Input.Body.Singlepart[Entity[Byte]] = Input.Body.Singlepart.Streaming

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
