package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import io.taig.otter.Annotation

object OpenApi extends Dsl:
  self =>

  abstract class Field[+S, A]:
    def value: A
    def modify(g: A => A): S

  object Field:
    def apply[S, A](a: A)(f: A => S): Field[S, A] = ???

  override object Metadata extends self.Metadata:
    sealed abstract class Schema[+A](attributes: Schema.Attributes):
      final def description: Field[A, Option[String]] = ???
      // Field(attributes.description)(description => copy(attributes.copy(description = description)))

      final def name: Field[A, Option[String]] =
        ??? // Field(attributes.name)(name => copy(attributes.copy(name = name)))

    object Schema:
      final case class Attributes(description: Option[String], name: Option[String])

      val Default: Metadata.Schema.Attributes = Attributes(description = None, name = None)

    final case class Primitive[+A](attributes: Primitive.Attributes) extends Schema[A](attributes.schema):
      def format: Field[A, Option[String]] = ???
      // Field(attributes.format)(format => copy(attributes.copy(format = format)))

    object Primitive:
      final case class Attributes(schema: Schema.Attributes, format: Option[String])

      val Default: Metadata.Primitive.Attributes = Attributes(schema = Schema.Default, format = None)

  def primitive[A](tpe: Type[A], attributes: Metadata.Primitive.Attributes): Primitive.Required[A] =
    Annotation(Plain.Primitive.Required.Root(tpe), Metadata.Primitive(attributes))

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = primitive(tpe, Metadata.Primitive.Default)

object Playground {
  import OpenApi.*

  val x: Primitive.Required[String] = string
  val y: Schema[String] = x

  x.metadata.name
  // x.name.modify(self => Annotation(x.self, self))(_.map(_.reverse))
  // val z: Primitive.Required[String] = x.metadata.name.modify(identity)

  // given toMetadata[S, M[+_]]: Conversion[Annotation[S, M], M[Annotation[S, M]]] = _.metadata

  // val a: Primitive.Required[String] = z.imap(_.reverse)(_.reverse)

  @main
  def run = {
    // val b = a.name("foobar")
    // println(b.name.value)
  }

  // z.name.apply("")
  // z.name.apply(None)
  // val a: Schema[String] = z.name.apply(Some("lol"))
}
