package io.taig.otter

import io.taig.otter as Base

sealed trait Schema[+S[_], A] extends Schema.Reader[S, A], Schema.Writer[S, A]:
  override def optional: Schema[S, Option[A]] = Schema.Optional(this)

object Schema:
  sealed trait Required[+S[_], A] extends Schema[S, A], Schema.Required.Reader[S, A], Schema.Required.Writer[S, A]:
    override def optional: Schema.Required[S, Option[A]] = Schema.Required.Optional(this)

  object Required:
    sealed trait Reader[+S[_], +A] extends Schema.Reader[S, A]:
      override def optional: Schema.Required.Reader[S, Option[A]] = Reader.Optional(this)

    object Reader:
      final case class Optional[S[_], A](self: Schema.Required.Reader[S, A])
          extends Schema.Required.Reader[S, Option[A]]

    sealed trait Writer[+S[_], -A] extends Schema.Writer[S, A]:
      override def optional: Schema.Required.Writer[S, Option[A]] = Writer.Optional(this)

    object Writer:
      final case class Optional[S[_], A](self: Schema.Required.Writer[S, A])
          extends Schema.Required.Writer[S, Option[A]]

    final case class Optional[S[_], A](self: Schema.Required[S, A]) extends Schema.Required[S, Option[A]]

  sealed trait Reader[+S[_], +A]:
    def optional: Schema.Reader[S, Option[A]] = Reader.Optional(this)

  object Reader:
    final case class Optional[S[_], A](self: Schema.Reader[S, A]) extends Schema.Reader[S, Option[A]]

    type Any[+S[+_], +A] = Schema.Reader[Data[S, ?, *], A]

  sealed trait Writer[+S[_], -A]:
    def optional: Schema.Writer[S, Option[A]] = Writer.Optional(this)

  object Writer:
    final case class Optional[S[_], A](self: Schema.Writer[S, A]) extends Schema.Writer[S, Option[A]]

  final case class Optional[S[_], A](self: Schema[S, A]) extends Schema[S, Option[A]]

  final case class Root[S[_], A](data: S[A]) extends Schema[S, A]

  type Any[+S[+_], A] = Schema[Data[S, ?, *], A]
  // type Any[+S[+_], A] = Schema.Reader[Data[S, ?, *], A] | Schema.Writer[Data[S, ?, *], A]
