package io.taig.otter.openapi

object Metadata:
  sealed abstract class Schema[+S](attributes: Schema.Attributes)(copy: Schema.Attributes => S):
    abstract class Field[A]:
      def value: A
      def modify(f: A => A): S
      final def apply(a: A): S = modify(_ => a)

    object Field:
      def apply[A](a: A)(f: A => Schema.Attributes): Field[A] = new Field[A]:
        override def value: A = a
        override def modify(g: A => A): S = copy(f(g(a)))

    final def description: Field[Option[String]] = Field(attributes.description): description =>
      attributes.copy(description = description)

    final def name: Field[Option[String]] = Field(attributes.name): name =>
      attributes.copy(name = name)

  object Schema:
    final case class Attributes(description: Option[String], name: Option[String])

    val Default: Metadata.Schema.Attributes = Attributes(description = None, name = None)

  final case class Primitive[+S](attributes: Primitive.Attributes)(copy: Primitive.Attributes => S)
      extends Schema[S](attributes.schema)(a => ???)

  object Primitive:
    final case class Attributes(schema: Schema.Attributes, format: Option[String])

    val Default: Metadata.Primitive.Attributes = Attributes(schema = Schema.Default, format = None)
