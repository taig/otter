package io.taig.otter.http

import io.taig.otter.Schema
import io.taig.otter.Decoder

sealed trait Request[+Segment[+_], +Query[+_], +Header[+_], +Body[+_], +Schema[+_], +A]:
  def method: Method
  def url: Url[Segment, Query, Schema, ?]
  def headers: Headers[Header, Schema, ?]
  def body: Body[Request.Body[Schema, ?]]

object Request:
  sealed trait Body[+F[+_], +A] extends Product, Serializable

  object Body:
    sealed trait Singlepart[+F[+_], +A] extends Request.Body[F, A]

    object Singlepart:
      sealed trait Strict[+F[+_], +A] extends Request.Body.Singlepart[F, A]:
        final def optional: Request.Body.Singlepart.Strict[F, Option[A]] = Strict.Optional(this)

      object Strict:
        final case class Apply[F[+_], A, B](
            parser: Array[Byte] => A,
            decoder: Decoder[[a] =>> F[Schema.Reader[F, ?, a]], A],
            schema: F[Schema.Reader[F, ?, B]]
        ) extends Request.Body.Singlepart.Strict[F, B]

        case object Binary extends Request.Body.Singlepart.Strict[Nothing, Array[Byte]]

        case object Empty extends Request.Body.Singlepart.Strict[Nothing, Unit]

        final case class Optional[F[+_], A](self: Request.Body.Singlepart.Strict[F, A])
            extends Request.Body.Singlepart.Strict[F, Option[A]]
