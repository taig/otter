package io.taig.otter

import cats.syntax.all.*
import cats.Invariant
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Codec.Result
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.Id as Identity

abstract class Enumeration[+F[+a <: Data] <: Data.Optional[a], A] extends Codec[F, Data.Primitive, A]:
  self =>

  def codec: Codec[?, ?, ?]

  override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, A] = new Enumeration[F, A]:
    export self.{codec, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Enumeration[F, A] = new Enumeration[F, A]:
    export self.{codec, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  override def imap[B](f: A => B)(g: B => A): Enumeration[F, B] = new Enumeration[F, B]:
    export self.{codec, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[Data.Primitive] = self.encode(g(b))

  final override def optional: Enumeration[Data.Optional, Option[A]] = new Enumeration[Data.Optional, Option[A]]:
    export self.{codec, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Primitive] = a.map(self.encode).getOrElse(Data.Null)

object Enumeration:
  def apply[A, B](codec: Codec[Identity, Data.Primitive, A], mapping: Mapping[B, A]): Enumeration[Identity, B] =
    val _codec = codec

    new Enumeration[Identity, B]:
      override def codec: Codec[Identity, Data.Primitive, A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[B] = None
      override def decode(data: Data): Codec.Result[B] = codec
        .decode(data)
        .andThen: a =>
          mapping
            .unapply(a)
            .toValid(Violations.rootNec(Violation(Constraint.OneOf(mapping.values.map(encode)), actual = data)))
      override def encode(b: B): Data.Primitive = codec.encode(mapping(b))

  given [F[+a <: Data] <: Data.Optional[a]]: Invariant[Enumeration[F, *]] with
    override def imap[A, B](fa: Enumeration[F, A])(f: A => B)(g: B => A): Enumeration[F, B] = fa.imap(f)(g)
