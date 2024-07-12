package io.taig.otter

import cats.syntax.all.*

sealed trait Branch[-F, +O, A] extends Branch.Reader[F, O, A], Branch.Writer[F, O, A]:
  override def schema: Schema[F, ?, ?]

object Branch:
  type Via[F, A] = Branch[F, ?, A]

  sealed trait Reader[-F, +O, +A]:
    def name: String
    def schema: Schema.Reader[F, ?, ?]

  object Reader:
    type Via[F, A] = Branch.Reader[F, ?, A]

    final case class Root[F, +O <: Schema.Reader[F, ?, A], A](metadata: Metadata, name: String, schema: O)
        extends Branch.Reader[F, O, A]

  sealed trait Writer[-F, +O, -A]:
    def name: String
    def schema: Schema.Writer[F, ?, ?]

  object Writer:
    type Via[F, A] = Branch.Writer[F, ?, A]

    final case class Root[F, +O <: Schema.Writer[F, ?, A], A](metadata: Metadata, name: String, schema: O)
        extends Branch.Writer[F, O, A]

  final case class Root[F, +O <: Schema[F, ?, A], A](metadata: Metadata, name: String, schema: O)
      extends Branch[F, O, A]
