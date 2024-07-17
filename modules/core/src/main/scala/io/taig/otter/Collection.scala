package io.taig.otter

import cats.data.Validated

import io.taig.otter.validation.Violations
import cats.syntax.all.*
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Validation
import io.taig.otter.Codec.Result

sealed abstract class Collection[+O, A] extends Codec[O, A]:
  self =>

  def codec: Codec[?, ?]

  final override def modifyMetadata(f: Metadata => Metadata): Collection[O, A] = new Collection[O, A]:
    export self.{codec, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def default(f: Option[A] => Option[A]): Collection[O, A] = new Collection[O, A]:
    export self.{codec, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Collection[O, B] = ivalidate(Validation.lift(f))(g)

  final def ivalidate[B](validation: CodecValidation.Collection[A, B])(f: B => A): Collection[O, B] =
    new Collection[O, B]:
      export self.{codec, metadata}
      override def default: Option[B] = self.default.flatMap(validation(_).toOption)
      override def decode(data: Data): Codec.Result[B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
      override def encode(b: B): Data = self.encode(f(b))

  final override def optional: Collection[O, Option[A]] = new Collection[O, Option[A]]:
    export self.{codec, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)

object Collection:
  def apply[A](of: Codec[?, A]): Collection[of.type, Vector[A]] = new Collection[of.type, Vector[A]]:
    override def codec: Codec[?, ?] = of
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Vector[A]] = None
    override def decode(data: Data): Codec.Result[Vector[A]] = data.toArray
      .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))))
      .andThen(_.values.traverse(of.decode))
    override def encode(a: Vector[A]): Data = Data.Array(a.map(of.encode))
