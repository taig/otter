package io.taig.otter

import cats.syntax.all.*
import scala.Product as SProduct

sealed trait Branch[+O, A] extends SProduct, Serializable:
  final def imap[B](f: A => B)(g: B => A): Branch[O, B] = Branch.Transform(this, f, g)
  def metadata: Metadata
  def name: String
  def schema: Codec[?, ?]

object Branch:
  final case class Root[F, O <: Codec[?, A], A](metadata: Metadata, name: String, schema: O) extends Branch[O, A]

  final case class Transform[F, O, A, B](self: Branch[O, A], f: A => B, g: B => A) extends Branch[O, B]:
    export self.{metadata, name, schema}
