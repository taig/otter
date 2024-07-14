package io.taig.otter.http

import io.taig.otter.Schema
import io.taig.otter.Decoder

sealed trait Request[+A]:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Request.Body[?]

object Request:
  sealed trait Body[+A] extends Product, Serializable

  object Body:
    sealed trait Singlepart[+A] extends Request.Body[A]

    object Singlepart:
      sealed trait Strict[+A] extends Request.Body.Singlepart[A]:
        final def optional: Request.Body.Singlepart.Strict[Option[A]] = Strict.Optional(this)

      object Strict:
        final case class Apply[A, B](
            parser: Array[Byte] => A,
            decoder: Decoder[Schema[String, ?, *], A],
            schema: Schema[String, ?, B]
        ) extends Request.Body.Singlepart.Strict[B]

        case object Binary extends Request.Body.Singlepart.Strict[Array[Byte]]

        case object Empty extends Request.Body.Singlepart.Strict[Unit]

        final case class Optional[A](self: Request.Body.Singlepart.Strict[A])
            extends Request.Body.Singlepart.Strict[Option[A]]
