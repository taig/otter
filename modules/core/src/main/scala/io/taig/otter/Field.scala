package io.taig.otter

import cats.syntax.all.*

sealed trait Field[-F, +O, A] extends Field.Reader[F, O, A], Field.Writer[F, O, A]:
  override def schema: Schema[F, ?, ?]

object Field:
  type Via[F, A] = Field[F, ?, A]

  sealed trait Reader[-F, +O, +A]:
    def metadata: Metadata
    def name: String
    def schema: Schema.Reader[F, ?, ?]

  object Reader:
    type Via[F, A] = Field.Reader[F, ?, A]

    final case class Root[F, +O <: Schema.Reader[F, ?, A], A](metadata: Metadata, name: String, schema: O)
        extends Field.Reader[F, O, A]

  sealed trait Writer[-F, +O, -A]:
    def metadata: Metadata
    def name: String
    def schema: Schema.Writer[F, ?, ?]

  object Writer:
    type Via[F, A] = Field.Writer[F, ?, A]

    final case class Root[F, +O <: Schema.Writer[F, ?, A], A](metadata: Metadata, name: String, schema: O)
        extends Field.Writer[F, O, A]

  final case class Root[F, +O <: Schema[F, ?, A], A](metadata: Metadata, name: String, schema: O) extends Field[F, O, A]
