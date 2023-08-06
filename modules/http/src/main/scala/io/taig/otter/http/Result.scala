package io.taig.otter.http

sealed abstract class Result[A]:
  def code: Code
  def headers: Headers[?]
  def body: Response.Body[?]
  final def imap[B](f: A => B)(g: B => A): Result[B] = Result.Modify(this, f, g)
  def toResults: Results[A] = Results(this)

object Result:
  final private[otter] case class Root[A, B](code: Code, headers: Headers[A], body: Response.Body[B])
      extends Result[(A, B)]

  final private[otter] case class Modify[A, B](self: Result[A], f: A => B, g: B => A) extends Result[B]:
    export self.{body, code, headers}

  def apply[A, B](code: Code, headers: Headers[A], body: Response.Body[B]): Result[(A, B)] = Root(code, headers, body)
  def apply[A](code: Code, body: Response.Body[A]): Result[A] =
    Root(code, Headers.Empty, body).imap { case (_, a) => a }(((), _))
  def apply[A](code: Code, headers: Headers[A]): Result[A] =
    Root(code, headers, Response.Body.Strict.Empty).imap { case (a, _) => a }((_, ()))
  def apply(code: Code): Result[Unit] =
    Root(code, Headers.Empty, Response.Body.Strict.Empty).imap(_ => ())(_ => ((), ()))
