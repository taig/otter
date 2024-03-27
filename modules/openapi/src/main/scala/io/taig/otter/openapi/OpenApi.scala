package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import io.taig.otter.Annotation
import cats.Functor

object OpenApi extends Dsl:
  self =>
  override object Metadata extends self.Metadata:
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
        override def modify(g: A => A): S = f(g(a))

    sealed abstract class Schema[+S](attributes: Schema.Attributes)(copy: Schema.Attributes => S):
      type Self[+s] <: Metadata.Schema[s]

      def map[T](f: S => T): Self[T]

      final def description: Field[S, Option[String]] =
        Field(attributes.description)(description => copy(attributes.copy(description = description)))

      final def name: Field[S, Option[String]] =
        Field(attributes.name)(name => copy(attributes.copy(name = name)))

    object Schema:
      final case class Attributes(description: Option[String], name: Option[String])

      val Default: Metadata.Schema.Attributes = Attributes(description = None, name = None)

    final case class Primitive[+S](attributes: Primitive.Attributes)(copy: Primitive.Attributes => S)
        extends Schema[S](attributes.schema)(schema => copy(attributes.copy(schema = schema))):
      override type Self[+s] = Metadata.Primitive[s]
      override def map[T](f: S => T): Metadata.Primitive[T] = Primitive(attributes)(copy.andThen(f))

      def format: Field[S, Option[String]] = Field(attributes.format)(format => copy(attributes.copy(format = format)))

    object Primitive:
      final case class Attributes(schema: Schema.Attributes, format: Option[String])

      val Default: Metadata.Primitive.Attributes = Attributes(schema = Schema.Default, format = None)

      given Functor[Metadata.Primitive] with
        override def map[A, B](fa: Metadata.Primitive[A])(f: A => B): Metadata.Primitive[B] = fa.map(f)

  def primitive[A](tpe: Type[A], attributes: Metadata.Primitive.Attributes): Primitive.Required[A] =
    Annotation(Plain.Primitive.Required.Root(tpe), Metadata.Primitive(attributes)(primitive(tpe, _)))
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = primitive(tpe, Metadata.Primitive.Default)

object Playground {
  import OpenApi.*
  // import OpenApi.given

  val x: Primitive.Required[String] = string
  val y: Schema[String] = x

  val z: Primitive.Required[String] = x.metadata.name.modify(identity)

  // given toMetadata[S, M[+_]]: Conversion[Annotation[S, M], M[Annotation[S, M]]] = _.metadata

  val a: Primitive.Required[String] = z.imap(_.reverse)(_.reverse)

  @main
  def run = {
    val b = a.name("foobar")
    println(b.name.value)
  }

  // z.name.apply("")
  // z.name.apply(None)
  // val a: Schema[String] = z.name.apply(Some("lol"))
}
