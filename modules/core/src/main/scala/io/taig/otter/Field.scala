package io.taig.otter

sealed trait Field[F[+_], A, +B, C] extends Field.Reader[F, A, B, C], Field.Writer[F, A, B, C]:
  def key: F[Primitive.Required[?]]
  def value: F[Schema[F, ?, ?]]

object Field:
  sealed trait Reader[F[+_], +A, +B, +C]:
    def key: F[Primitive.Required.Reader[?]]
    def value: F[Schema.Reader[F, ?, ?]]

  object Reader:
    final case class Root[F[+_], A, B <: F[Schema.Reader[F, ?, C]], C](key: F[Primitive.Required.Reader[A]], value: B)
        extends Field.Reader[F, A, B, C]

  sealed trait Writer[F[+_], -A, +B, -C]:
    def key: F[Primitive.Required.Writer[?]]
    def value: F[Schema.Writer[F, ?, ?]]

  object Writer:
    final case class Root[F[+_], A, B <: F[Schema.Writer[F, ?, C]], C](key: F[Primitive.Required.Writer[A]], value: B)
        extends Field.Writer[F, A, B, C]

  final case class Root[F[+_], A, B <: F[Schema[F, ?, C]], C](key: F[Primitive.Required[A]], value: B)
      extends Field[F, A, B, C]
