package io.taig.otter.http

import io.taig.otter.Value
import cats.syntax.all.*
import io.taig.otter.Metadata

sealed trait Parameter[A]:
  final def imap[B](f: A => B)(g: B => A): Parameter[B] = Parameter.Transform(this, f, g)
  def metadata: Metadata
  def schema: Value[?, ?]
  def update(f: Metadata => Metadata): Parameter[A]

object Parameter:
  final case class Root[A](metadata: Metadata, name: String, schema: Value[?, A]) extends Parameter[A]:
    override def update(f: Metadata => Metadata): Parameter[A] = copy(metadata = f(metadata))

  final case class Transform[A, B](self: Parameter[A], f: A => B, g: B => A) extends Parameter[B]:
    export self.{metadata, schema}
    override def update(f: Metadata => Metadata): Parameter[B] = copy(self = self.update(f))
