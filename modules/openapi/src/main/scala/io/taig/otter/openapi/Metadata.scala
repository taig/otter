package io.taig.otter.openapi

import cats.Id as Identity
import io.taig.otter.Attribute

sealed abstract class Metadata[F[_]]:
  type Self[f[_]] <: Metadata[f]
  def name: F[Option[String]]
  def description: F[Option[String]]

  def asSelf: Self[F]
  def toAttributes[S](f: Self[Identity] => S)(using Self[F] =:= Self[Identity]): Self[Attribute[S, *]]
  // extension (self: Self[Identity]) def toAttributes[S](f: Self[Identity] => S): Self[Attribute[S, *]]

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
    )(using ev: Self[F] =:= Metadata.Primitive[Identity]): Metadata.Primitive[Attribute[S, *]] = Primitive(
      description =
        Attribute(ev.apply(this).description)(g => f(ev.apply(this).copy(description = g(ev.apply(this).description)))),
      format = ???,
      name = ???
    )

    // extension (self: Metadata.Primitive[Identity])
    //   override def toAttributes[S](f: Metadata.Primitive[Identity] => S): Metadata.Primitive[Attribute[S, *]] =
    //     Primitive(
    //       description = Attribute(self.description)(g => f(self.copy(description = g(self.description)))),
    //       format = Attribute(self.format)(???),
    //       name = ???
    //     )

  object Primitive:
    val Default: Metadata.Primitive[Identity] = Metadata.Primitive(description = None, format = None, name = None)

  final case class Tuple[F[_]](
      description: F[Option[String]],
      name: F[Option[String]]
  ) extends Metadata[F]:
    override type Self[f[_]] = Metadata.Tuple[f]

    override def asSelf: Metadata.Tuple[F] = this

    override def toAttributes[S](f: Metadata.Tuple[Identity] => S)(using
        Self[F] =:= Metadata.Tuple[Identity]
    ): Metadata.Tuple[Attribute[S, *]] = ???

    // extension (self: Metadata.Tuple[Identity])
    //   override def toAttributes[S](f: Metadata.Tuple[Identity] => S): Metadata.Tuple[Attribute[S, *]] = Tuple(
    //     description = Attribute(self.description)(g => f(self.copy(description = g(self.description)))),
    //     name = ???
    //   )
