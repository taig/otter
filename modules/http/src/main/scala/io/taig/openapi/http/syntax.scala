package io.taig.openapi.http

import cats.Eval
import io.taig.openapi.schema.{Value, Void}

object syntax:
  val __ : Url[Void] = Url.Root

  def parameter[A](name: String, schema: => Value[A]): Segment[A] = Segment.parameter(name, Eval.later(schema))
