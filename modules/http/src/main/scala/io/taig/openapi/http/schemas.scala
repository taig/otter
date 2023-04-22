package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import cats.{Applicative, Eq, Eval, Id}
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.schema.syntax.*
import io.taig.openapi.schema.*
import org.typelevel.ci.CIString
import fs2.{Pure, Stream}
import scodec.bits.ByteVector

import scala.annotation.targetName

object schemas:
  val cistring: Primitive[CIString] = string.imap(CIString.apply)(_.toString)

  object request:
    val singlepart: Product[Request.Body.Singlepart[Pure]] =
      field("data", dynamic.nil.imap[Stream[Pure, Byte]](_ => Stream.empty)(_ => OpenApi.Null)).gimap

    val part: Product[Request.Body.Multipart.Part[Pure]] = (field("name", string) :*
      field("filename", string.optional) :*
      field("body", singlepart)).gimap

    val multipart: Product[Request.Body.Multipart[Pure]] = field("parts", collection.chain(part)).gimap

    val body: Schema[Request.Body[Pure]] =
      (branch("multipart", multipart) + branch("singlepart", singlepart)).gimap

    val main: Product[Request[Pure]] = (
      field("method", string.imap(Method.apply)(_.toString)) :*
        field("path", collection.chain(dynamic.primitive)) :*
        field("queries", collection.chain(field("name", string) :* field("value", dynamic.primitive))) :*
        field("headers", collection.chain(field("name", cistring) :* field("value", dynamic.primitive))) :*
        field("body", body)
    ).gimap

  val response: Product[Response] = (
    field("code", int.imap(Code.apply)(_.toInt)) :*
      field("headers", collection.chain(field("name", cistring) :* field("value", dynamic.primitive))) :*
      field("body", dynamic.any.optional)
  ).gimap

  object error:
    def product[A](identifier: String, payload: => Schema[A], hint: Option[String] = none): Product[A] =
      field("value", payload) <*
        field("type", string.const(identifier)) <*
        field("hint", string.optional.const(hint))

    def apply[A](identifier: String, payload: => Schema[A], hint: Option[String] = none): Sum[String, A] =
      val schema = error.product(identifier, payload, hint)
      Sum.of(branch(identifier, payload), Eval.now(schema))
