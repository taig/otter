package io.taig.otter

import cats.syntax.all.*
import scala.Product as SProduct

sealed trait Branch[-F, +O, A] extends SProduct, Serializable:
  final def imap[B](f: A => B)(g: B => A): Branch[F, O, B] = Branch.Transform(this, f, g)
  def metadata: Metadata
  def name: String
  def schema: Schema[F, ?, ?]

object Branch:
  type Via[F, A] = Branch[F, ?, A]

  final case class Root[F, +O <: Schema[F, ?, A], A](metadata: Metadata, name: String, schema: O)
      extends Branch[F, O, A]

  final case class Transform[F, O, A, B](self: Branch[F, O, A], f: A => B, g: B => A) extends Branch[F, O, B]:
    export self.{metadata, name, schema}
