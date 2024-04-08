package io.taig.otter.openapi

import cats.Id as Identity
import io.taig.otter.Attribute

sealed abstract class Metadata[F[_]]:
  type Self[f[_]] <: Metadata[f]
  def name: F[Option[String]]
  def description: F[Option[String]]

  def asSelf: Self[F]
  def toAttributes[S](f: Self[Identity] => S)(using Self[F] =:= Self[Identity]): Self[Attribute[S, *]]

object Metadata:
  sealed abstract class Value[F[_]] extends Metadata[F]:
    override type Self[f[_]] <: Value[f]

  final case class Primitive[F[_]](
      description: F[Option[String]],
      format: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata.Value[F]:
    override type Self[f[_]] = Metadata.Primitive[f]

    override def asSelf: Metadata.Primitive[F] = this

    override def toAttributes[S](
        f: Primitive[Identity] => S
    )(using ev: Self[F] =:= Metadata.Primitive[Identity]): Metadata.Primitive[Attribute[S, *]] =
      val self = ev.apply(this)
      Primitive(
        description = Attribute(self.description)(g => f(self.copy(description = g(self.description)))),
        format = Attribute(self.format)(g => f(self.copy(format = g(self.format)))),
        name = Attribute(self.name)(g => f(self.copy(description = g(self.name))))
      )

  object Primitive:
    val Default: Metadata.Primitive[Identity] = Metadata.Primitive(description = None, format = None, name = None)

  final case class Tuple[F[_]](
      description: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata[F]:
    override type Self[f[_]] = Metadata.Tuple[f]

    override def asSelf: Metadata.Tuple[F] = this

    override def toAttributes[S](f: Metadata.Tuple[Identity] => S)(using
        ev: Self[F] =:= Metadata.Tuple[Identity]
    ): Metadata.Tuple[Attribute[S, *]] =
      val self = ev.apply(this)
      Tuple(
        description = Attribute(self.description)(g => f(self.copy(description = g(self.description)))),
        name = Attribute(self.name)(g => f(self.copy(description = g(self.name))))
      )

  object Tuple:
    val Default: Metadata.Tuple[Identity] = Metadata.Tuple(description = None, name = None)
