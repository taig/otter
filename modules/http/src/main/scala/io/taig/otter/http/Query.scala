package io.taig.otter.http

import io.taig.otter.Value
import cats.syntax.all.*
import io.taig.otter.Metadata

sealed trait Query[+A]:
  def metadata: Metadata
  def schema: Value[String, ?, ?]
  def update(f: Metadata => Metadata): Query[A]

object Query:
  final case class Root[A](metadata: Metadata, name: String, schema: Value[String, ?, A]) extends Query[A]:
    override def update(f: Metadata => Metadata): Query[A] = copy(metadata = f(metadata))

  final case class Transform[A, B](self: Query[A], f: A => B) extends Query[B]:
    export self.{metadata, schema}
    override def update(f: Metadata => Metadata): Query[B] = copy(self = self.update(f))
