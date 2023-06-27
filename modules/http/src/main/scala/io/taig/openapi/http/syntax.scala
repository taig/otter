package io.taig.openapi.http

import cats.Eval
import io.taig.openapi.schema.{Value, Void}
import org.typelevel.ci.CIString

object syntax:
  val __ : Url[Void] = Url.Root

  def header[A](name: CIString, schema: => Value[A]): Header[A] = Header(name, Eval.later(schema))

  def parameter[A](name: String, schema: => Value[A]): Segment[A] = Segment.Parameter(name, Eval.later(schema))

  def query[A](name: String, schema: => Value[A]): Query[A] = Query(name, Eval.later(schema))

//  object input:
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
