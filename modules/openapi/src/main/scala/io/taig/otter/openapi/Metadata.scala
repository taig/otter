package io.taig.otter.openapi

import cats.Id as Identity
import io.taig.otter.Attribute

sealed abstract class Metadata[F[_]]:
  type Self[f[_]] <: Metadata[f]
  def name: F[Option[String]]
  def description: F[Option[String]]

  extension (self: Self[Identity]) def toAttributes[S](f: Self[Identity] => S): Self[Attribute[S, *]]

object Metadata:
  final case class Primitive[F[_]](
      description: F[Option[String]],
      format: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata[F]:
    override type Self[f[_]] = Metadata.Primitive[f]

    extension (self: Metadata.Primitive[Identity])
      override def toAttributes[S](f: Metadata.Primitive[Identity] => S): Metadata.Primitive[Attribute[S, *]] =
        Primitive(
          description = Attribute(self.description)(???),
          format = Attribute(self.format)(???),
          name = ???
        )

  object Primitive:
    extension (self: Metadata.Primitive[Identity])
      def toAttributes[S](f: Metadata.Primitive[Identity] => S): Metadata.Primitive[Attribute[S, *]] = Primitive(
        description = Attribute(self.description)(g => f(self.copy(description = g(self.description)))),
        format = Attribute(self.format)(g => f(self.copy(format = g(self.format)))),
        name = Attribute(self.name)(g => f(self.copy(name = g(self.name))))
      )

    val Default: Metadata.Primitive[Identity] = Metadata.Primitive(description = None, format = None, name = None)

  final case class Tuple[F[_]](
      description: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata[F]:
    override type Self[f[_]] = Metadata.Tuple[f]

    extension (self: Metadata.Tuple[Identity])
      override def toAttributes[S](f: Metadata.Tuple[Identity] => S): Metadata.Tuple[Attribute[S, *]] = Tuple(
        description = Attribute(self.description)(???),
        name = ???
      )
