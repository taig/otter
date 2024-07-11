package io.taig.otter

import cats.Functor
import cats.syntax.all.*

sealed trait Branch[-A, +B, C] extends Branch.Reader[A, B, C], Branch.Writer[A, B, C]:
  override def schema: Schema[A, ?, ?]

object Branch:
  sealed trait Reader[-A, +B, +C]:
    def name: String
    def schema: Schema.Reader[A, ?, ?]

  object Reader:
    final case class Root[A, +B <: Schema.Reader[A, ?, C], C](name: String, schema: B) extends Branch.Reader[A, B, C]

  sealed trait Writer[-A, +B, -C]:
    def name: String
    def schema: Schema.Writer[A, ?, ?]

  object Writer:
    final case class Root[A, +B <: Schema.Writer[A, ?, C], C](name: String, schema: B) extends Branch.Writer[A, B, C]

  final case class Root[A, +B <: Schema[A, ?, C], C](name: String, schema: B) extends Branch[A, B, C]
