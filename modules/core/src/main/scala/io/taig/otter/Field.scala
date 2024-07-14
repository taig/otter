package io.taig.otter

import cats.syntax.all.*
import scala.Product as SProduct

sealed trait Field[-F, +O, A] extends SProduct, Serializable:
  final def imap[B](f: A => B)(g: B => A): Field[F, O, B] = Field.Transform(this, f, g)
  def metadata: Metadata
  def name: String
  def schema: Schema[F, ?, ?]

object Field:
  type Via[F, A] = Field[F, ?, A]

  final case class Root[F, O <: Schema[F, ?, A], A](metadata: Metadata, name: String, schema: O) extends Field[F, O, A]

  final case class Transform[F, O, A, B](self: Field[F, O, A], f: A => B, g: B => A) extends Field[F, O, B]:
    export self.{metadata, name, schema}
