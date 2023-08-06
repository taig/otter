package io.taig.otter.http

import cats.Eval
import io.taig.otter.schema.Schema

sealed abstract class Result[A]:
  def code: Code
  def headers: Headers[?]
  def body: Response.Body[?]

object Result:
  final case class Root[A](code: Code, body: Response.Body[A]) extends Result[A]:
    override def headers: Headers[?] = Headers.Empty
