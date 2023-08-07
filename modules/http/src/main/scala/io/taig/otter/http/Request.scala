package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.schema.Violations
import io.taig.otter.validation.Validation

sealed abstract class Request[A]:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Request.Body[?]
  final def imap[B](f: A => B)(g: B => A): Request[B] = Request.Modify(this, f, g)

object Request:
  sealed abstract class Body[A]:
    self =>
    type Self[a] <: Body[a] { type Self[a] = self.Self[a] }
    def withHeaders: Self[(Http.Headers, A)]
    def andThen[B](f: A => Validated[Violations, B])(g: B => A): Self[B]
    def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
    final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
    final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)

  object Body:
    sealed abstract class Singlepart[A] extends Request.Body[A]:
      self =>
      override type Self[a] <: Request.Body.Singlepart[a] { type Self[a] = self.Self[a] }

    object Singlepart:
      sealed abstract class Strict[A] extends Request.Body.Singlepart[A]:
        final override type Self[a] = Request.Body.Singlepart.Strict[a]
        final override def withHeaders: Strict[(Http.Headers, A)] = Strict.WithHeaders(this)
        final override def andThen[B](f: A => Validated[Violations, B])(g: B => A): Strict[B] =
          Strict.AndThen(this, f, g)
        final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Request.Body.Singlepart.Strict[B] =
          Strict.Validate(this, validation, g)

      object Strict:
        private[otter] case object Bytes extends Request.Body.Singlepart.Strict[Array[Byte]]

        final private[otter] case class WithHeaders[A](self: Request.Body.Singlepart.Strict[A])
            extends Request.Body.Singlepart.Strict[(Http.Headers, A)]

        final private[otter] case class AndThen[A, B](
            self: Request.Body.Singlepart.Strict[A],
            f: A => Validated[Violations, B],
            g: B => A
        ) extends Request.Body.Singlepart.Strict[B]

        final private[otter] case class Validate[A, B](
            self: Request.Body.Singlepart.Strict[A],
            validation: Validation[A, B],
            g: B => A
        ) extends Request.Body.Singlepart.Strict[B]

        val Empty: Request.Body.Singlepart.Strict[Unit] = Bytes.imap(_ => ())(_ => Array.emptyByteArray)

      sealed abstract class Streaming[A] extends Request.Body.Singlepart[A]:
        final override type Self[a] = Request.Body.Singlepart.Streaming[a]
        final override def withHeaders: Streaming[(Http.Headers, A)] = ???
        final override def andThen[B](f: A => Validated[Violations, B])(g: B => A): Streaming[B] = ???
        final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Request.Body.Singlepart.Streaming[B] =
          Streaming.Validate(this, validation, g)

      object Streaming:
        private[otter] case object Bytes extends Request.Body.Singlepart.Streaming[Stream[Byte]]

        final private[otter] case class Validate[A, B](
            self: Request.Body.Singlepart.Streaming[A],
            validation: Validation[A, B],
            g: B => A
        ) extends Request.Body.Singlepart.Streaming[B]

        val Empty: Request.Body.Singlepart.Streaming[Unit] = Bytes.imap(_ => ())(_ => Stream.Empty)

  final private[otter] case class Root[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Request.Body[C])
      extends Request[(A, B, C)]

  final private[otter] case class Modify[A, B](self: Request[A], f: A => B, g: B => A) extends Request[B]:
    export self.{body, headers, method, url}

  def apply[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Request.Body[C]): Request[(A, B, C)] =
    Root(method, url, headers, body)
