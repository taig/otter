package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import io.taig.openapi.schema.{Collection, Schema, Void}
import io.taig.openapi.validation.Validation
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
        def string(mediaType: String, charset: Charset): Input.Body.Singlepart[String] =
          // TODO verify media type
          binary.imapWithHeaders { case (headers, bytes) =>
            val charsetOrFallback = headers
              .getFirst(ci"Content-Type")
              .flatMap { value =>
                value
                  .split(";\\s*")
                  .map(_.split("\\s*=\\s*", 2))
                  .collectFirst { case Array(name, value) if name.equalsIgnoreCase("charset") => value }
                  .map(charset =>
                    if charset.startsWith("\"") && charset.endsWith("\"") then charset.tail.init else charset
                  )
                  .flatMap { charset =>
                    try Charset.forName(charset).some
                    catch {
                      case _: IllegalCharsetNameException | _: UnsupportedCharsetException => none
                    }
                  }
              }
              .getOrElse(charset)

            new String(bytes, charsetOrFallback)
          } { value =>
            (Http.Headers.one(ci"Content-Type", s"$mediaType; charset=${charset.name()}"), value.getBytes(charset))
          }

        val string: Input.Body.Singlepart[String] = string(mediaType = "text/plain", charset = StandardCharsets.UTF_8)

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
