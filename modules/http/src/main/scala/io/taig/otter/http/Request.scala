package io.taig.otter.http

import io.taig.otter.Schema
import io.taig.otter.Decoder

sealed trait Request[A]:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Request.Body[?]

object Request:
  sealed trait Body[A] extends Product, Serializable

  object Body:
    sealed trait Singlepart[A] extends Request.Body[A]

    object Singlepart:
      sealed trait Strict[A] extends Request.Body.Singlepart[A]:
        final def optional: Request.Body.Singlepart.Strict[Option[A]] = Strict.Optional(this)

      object Strict:
        final case class Apply[A, B](headers: Headers[A], schema: Schema[?, B])
            extends Request.Body.Singlepart.Strict[(A, B)]

        case object Binary extends Request.Body.Singlepart.Strict[Array[Byte]]

        case object Empty extends Request.Body.Singlepart.Strict[Unit]

        final case class Optional[A](self: Request.Body.Singlepart.Strict[A])
            extends Request.Body.Singlepart.Strict[Option[A]]

        final case class OrElse[A, B](left: Request.Body.Singlepart.Strict[A], right: Request.Body.Singlepart.Strict[B])
            extends Request.Body.Singlepart.Strict[Either[A, B]]

  final case class Root[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Request.Body[C])
      extends Request[(A, B, C)]
