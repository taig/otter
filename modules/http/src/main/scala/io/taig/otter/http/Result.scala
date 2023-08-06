package io.taig.otter.http

sealed abstract class Result[A]:
  def code: Code
  def headers: Headers[?]
  def body: Response.Body[?]
  def toResults: Results[A] = ???

object Result:
  final private[otter] case class Root[A, B](code: Code, headers: Headers[A], body: Response.Body[B])
      extends Result[(A, B)]

  final private[otter] case class Modify[A, B](self: Result[A], f: A => B, g: B => A) extends Result[B]:
    export self.{body, code, headers}
