package io.taig.otter

import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping

abstract class Enumeration[+F[+a] <: Data.Optional[a], A] extends Codec[F, Data.Primitive, A]:
  self =>

  def codec: Codec[?, ?, ?]

  override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, A] = new Enumeration[F, A]:
    export self.{codec, decode, default, encode, isOptional}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Enumeration[F, A] = new Enumeration[F, A]:
    export self.{codec, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def isOptional: Boolean = default.nonEmpty
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  override def imap[B](f: A => B)(g: B => A): Enumeration[F, B] = new Enumeration[F, B]:
    export self.{codec, isOptional, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[Data.Primitive] = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Enumeration[F, B] = imap(convert.to)(convert.from)

  final override def optional: Enumeration[Data.Optional, Option[A]] = new Enumeration[Data.Optional, Option[A]]:
    export self.{codec, metadata}
    override def isOptional: Boolean = true
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.asValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Primitive] = a.map(self.encode).getOrElse(Data.Null)

object Enumeration:
  def apply[A, B](
      of: => Codec[Data.Required, Data.Primitive, A],
      mapping: Mapping[B, A]
  ): Enumeration[Data.Required, B] = new Enumeration[Data.Required, B]:
    override def isOptional: Boolean = false
    override def codec: Codec[?, ?, ?] = of
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[B] = None
    override def decode(data: Data): Codec.Result[B] = of
      .decode(data)
      .andThen: a =>
        mapping
          .unapply(a)
          .toValid(Violations.rootNec(Violation.oneOf(mapping.values.toList.map(encode), actual = data)))
    override def encode(b: B): Data.Primitive = of.encode(mapping(b))

  given [F[+a] <: Data.Optional[a]]: CodecInvariant[Enumeration[F, *]] with
    override def imap[A, B](fa: Enumeration[F, A])(f: A => B)(g: B => A): Enumeration[F, B] = fa.imap(f)(g)

  given [F[+a] <: Data.Optional[a], A]: Metadata.Ops[Enumeration[F, A]] with
    extension (self: Enumeration[F, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, A] = self.modifyMetadata(f)
