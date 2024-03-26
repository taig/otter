package io.taig.otter.openapi

abstract class Field[+S, A]:
  def value: A
  def modify(f: A => A): S
  final def apply(a: A): S = modify(_ => a)

object Field:
  extension [S, A](self: Field[S, Option[A]])
    def apply(a: A): S = self.apply(Some(a))
    def clear: S = self.apply(None)

  def apply[S, A](a: A)(f: A => S): Field[S, A] = new Field[S, A]:
    override def value: A = a
    override def modify(g: A => A): S = ???

object Metadata:
  sealed abstract class Schema[+S](attributes: Schema.Attributes)(copy: Schema.Attributes => S):
    final def description: Field[S, Option[String]] = ???

    final def name: Field[S, Option[String]] = ???

  object Schema:
    final case class Attributes(description: Option[String], name: Option[String])

    val Default: Metadata.Schema.Attributes = Attributes(description = None, name = None)

  final case class Primitive[+S](attributes: Primitive.Attributes)(copy: Primitive.Attributes => S)
      extends Schema[S](attributes.schema)(a => ???):
    def format: Field[S, Option[String]] = ???

  object Primitive:
    final case class Attributes(schema: Schema.Attributes, format: Option[String])

    val Default: Metadata.Primitive.Attributes = Attributes(schema = Schema.Default, format = None)
