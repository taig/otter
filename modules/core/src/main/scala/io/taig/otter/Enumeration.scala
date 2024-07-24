package io.taig.otter

import cats.syntax.all.*
import cats.Invariant
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Codec.Result
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Enumeration[+O <: Data.Optional[Data.Primitive], A] extends Codec[O, A]:
  self =>

  def codec: Codec[?, ?]

  override def modifyMetadata(f: Metadata => Metadata): Enumeration[O, A] = new Enumeration[O, A]:
    export self.{codec, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Enumeration[O, A] = new Enumeration[O, A]:
    export self.{codec, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  override def imap[B](f: A => B)(g: B => A): Enumeration[O, B] = new Enumeration[O, B]:
    export self.{codec, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  final override def optional: Enumeration[Data.Optional[O], Option[A]] = new Enumeration[Data.Optional[O], Option[A]]:
    export self.{codec, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[O] = a.map(self.encode).getOrElse(Data.Null)

object Enumeration:
  def apply[O <: Data.Primitive, A, B](of: Codec[O, A], mapping: Mapping[B, A]): Enumeration[O, B] =
    new Enumeration[O, B]:
      override def codec: Codec[?, ?] = of
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[B] = None
      override def decode(data: Data): Codec.Result[B] = of
        .decode(data)
        .andThen: a =>
          mapping
            .unapply(a)
            .toValid(Violations.rootNec(Violation(Constraint.OneOf(mapping.values.map(encode)), actual = data)))
      override def encode(b: B): O = of.encode(mapping(b))

  given [O <: Data.Optional[Data.Primitive]]: Invariant[Enumeration[O, *]] with
    override def imap[A, B](fa: Enumeration[O, A])(f: A => B)(g: B => A): Enumeration[O, B] = fa.imap(f)(g)
