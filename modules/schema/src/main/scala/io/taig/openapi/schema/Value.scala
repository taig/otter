package io.taig.openapi.schema

abstract class Value[A] extends Schema[A]:
  self =>

  override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }
  override type Metadata[a] <: Value.Metadata[a] { type Self[a] <: Metadata[a] }

  final def default: Field[A] = Field(
    metadata.default,
    f => copy(metadata.copy(f(metadata.default), metadata.description, metadata.example))
  )

object Value:
  trait Metadata[A] extends Schema.Metadata[A]:
    override type Self[a] <: Value.Metadata[a]
    def default: Option[A]
    def copy(default: Option[A], description: Option[String], example: Option[A]): Self[A]
    final override def copy(description: Option[String], example: Option[A]): Self[A] =
      copy(default, description, example)
