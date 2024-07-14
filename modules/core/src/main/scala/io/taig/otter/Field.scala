package io.taig.otter

import cats.syntax.all.*
import scala.Product as SProduct

sealed trait Field[+O, A] extends SProduct, Serializable:
  final def imap[B](f: A => B)(g: B => A): Field[O, B] = Field.Transform(this, f, g)
  def metadata: Metadata
  def name: String
  def schema: Schema[?, ?]

object Field:
  final case class Root[F, O <: Schema[?, A], A](metadata: Metadata, name: String, schema: O) extends Field[O, A]

  final case class Transform[F, O, A, B](self: Field[O, A], f: A => B, g: B => A) extends Field[O, B]:
    export self.{metadata, name, schema}
