package io.taig.otter.openapi

import cats.Id as Identity

sealed abstract class Metadata[F[_]]:
  def name: F[Option[String]]
  def description: F[Option[String]]

object Metadata:
  final case class Primitive[F[_]](
      description: F[Option[String]],
      format: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata[F]

  object Primitive:
    extension (self: Metadata.Primitive[Identity])
      def toFields[S](f: Metadata.Primitive[Identity] => S): Metadata.Primitive[Field[S, *]] = Primitive(
        description = Field(self.description)(g => f(self.copy(description = g(self.description)))),
        format = Field(self.format)(g => f(self.copy(format = g(self.format)))),
        name = Field(self.name)(g => f(self.copy(name = g(self.name))))
      )

    val Default: Metadata.Primitive[Identity] = Metadata.Primitive(description = None, format = None, name = None)

  final case class Product[F[_]](
      description: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata[F]

  object Product:
    extension (self: Metadata.Product[Identity])
      def toFields[S](f: Metadata.Product[Identity] => S): Metadata.Product[Field[S, *]] = Product(
        description = Field(self.description)(g => f(self.copy(description = g(self.description)))),
        name = Field(self.name)(g => f(self.copy(name = g(self.name))))
      )

  abstract class Field[S, A]:
    def value: A
    def apply(f: A => A): S
    final def apply(a: A): S = apply(_ => a)

  object Field:
    extension [S, A](self: Metadata.Field[S, Option[A]])
      final def apply(a: A): S = self.apply(Some(a))
      final def clear: S = self.apply(None)

    def apply[S, A](a: A)(f: (A => A) => S): Field[S, A] = new Field[S, A]:
      override def value: A = a
      override def apply(g: A => A): S = f(g)
