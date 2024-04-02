package io.taig.otter.openapi

import cats.Id as Identity

sealed abstract class Metadata[F[_]]:
  def name: F[Option[String]]
  def description: F[Option[String]]

object Metadata:
  abstract class Field[S, A]:
    def value: A
    def apply(f: A => A): S

  object Field:
    def apply[S, A](a: A)(f: (A => A) => S): Field[S, A] = new Field[S, A]:
      override def value: A = a
      override def apply(g: A => A): S = f(g)

  final case class Primitive[F[_]](
      description: F[Option[String]],
      format: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata[F]

  object Primitive:
    extension (self: Metadata.Primitive[Identity])
      def toFields[S](f: Metadata.Primitive[Identity] => S): Metadata.Primitive[Field[S, *]] = Primitive(
        description = Field(self.description)(g => f(self.copy[Identity](description = g(self.description)))),
        format = Field(self.format)(???),
        name = Field(self.name)(???)
      )

  final case class Product[F[_]](
      description: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata[F]
