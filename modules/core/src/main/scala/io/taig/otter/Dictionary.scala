package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result
import cats.data.Chain
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Dictionary[+O, A] extends Codec[O, A]:
  self =>

  final override def metadata(f: Metadata => Metadata): Dictionary[O, A] = new Dictionary[O, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def default(f: Option[A] => Option[A]): Dictionary[O, A] = new Dictionary[O, A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Dictionary[O, B] = ???

  final override def optional: Dictionary[O, Option[A]] = ???

object Dictionary:
  def apply[A, B](key: Value.Required[?, A], value: Codec[?, B]): Dictionary[value.type, Chain[(A, B)]] =
    new Dictionary:
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Chain[(A, B)]] = None
      override def decode(data: Data): Result[Chain[(A, B)]] = data.toObject
        .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
        .andThen(_.values.traverse { case (a, b) => (key.parseValue(a), value.decode(b)).tupled })
      override def encode(abs: Chain[(A, B)]): Data =
        Data.Object(abs.map { case (a, b) => (key.printValue(a), value.encode(b)) })
